{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}

module Market.Onchain
    ( apiBuyScript
    , buyScriptAsShortBs
    , typedBuyValidator
    , Sale
    , buyValidator
    , buyValidatorHash
    , nftDatum
    ) where

import qualified Data.ByteString.Lazy  as LB
import qualified Data.ByteString.Short as SBS
import           Codec.Serialise       ( serialise )

import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)
import qualified PlutusTx
import PlutusTx.Prelude as Plutus
    ( Bool(..), Eq((==)), (/=), (.), length, (&&), Integer, Maybe(..), (>=), fromInteger, (*), ($), (%), (-), map )
import Ledger
    ( TokenName,
      PubKeyHash(..),
      ValidatorHash,
      Address(Address),
      validatorHash,
      pubKeyHashAddress,
      CurrencySymbol,
      DatumHash,
      Datum(..),
      txOutDatum,
      txSignedBy,
      ScriptContext(scriptContextTxInfo),
      ownHash,
      TxInfo,
      Validator,
      TxOut,
      txInfoSignatories,
      unValidatorScript,
      txInInfoResolved,
      txInfoInputs,
      valuePaidTo,
      findDatum,
      txInfoOutputs,
      txOutValue,
      txOutAddress,
      getContinuingOutputs)
import qualified Ledger.Typed.Scripts      as Scripts
import qualified Plutus.V1.Ledger.Scripts as Plutus
import           Ledger.Value              as Value ( valueOf )
import qualified Plutus.V1.Ledger.Ada as Ada (fromValue, Ada (getLovelace))
import           Plutus.V1.Ledger.Credential (Credential(ScriptCredential))


import Market.Types    (NFTSale(..), SaleAction(..), MarketParams(..))
import Updator.Onchain (tokenDatum)
import Updator.Types   (UpdateVHash(..))


{-# INLINABLE nftDatum #-}
nftDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe NFTSale
nftDatum o f = do
    dh <- txOutDatum o
    Datum d <- f dh
    PlutusTx.fromBuiltinData d

{-# INLINABLE mkBuyValidator #-}
mkBuyValidator :: MarketParams -> NFTSale -> SaleAction -> ScriptContext -> Bool
mkBuyValidator mp nfts r ctx = case r of
    Buy     -> checkFee (nPrice nfts) &&
               (valueOf (valuePaidTo info sig) (nCurrency nfts) (nToken nfts) == 1) &&
               checkSellerOut (nSeller nfts) (nRoyAddr nfts) (nRoyPrct nfts) (nPrice nfts) &&
               checkSingleBuy
    Update  -> txSignedBy (scriptContextTxInfo ctx) (nSeller nfts) &&
               checkDatum &&
               checkContinuing (nCurrency nfts) (nToken nfts)
    Close   -> txSignedBy (scriptContextTxInfo ctx) (nSeller nfts) &&
               (valueOf (valuePaidTo info (nSeller nfts)) (nCurrency nfts) (nToken nfts) == 1)
    UpdateC -> checkDatumUpdate &&
               checkUpdate (nCurrency nfts) (nToken nfts)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    sig :: PubKeyHash
    sig = case txInfoSignatories info of
            [pubKeyHash] -> pubKeyHash

    getTokenDatum :: Maybe UpdateVHash
    getTokenDatum = let is = [ i | i <- map txInInfoResolved (txInfoInputs info), valueOf (txOutValue i) (updateCs mp) (updateTn mp) == 1 && txOutAddress i /= pubKeyHashAddress (feeAddr mp) ] in
                    case is of
                        [i] -> tokenDatum i (`findDatum` info)

    checkSingleBuy :: Bool
    checkSingleBuy = let is = [ i | i <- map txInInfoResolved (txInfoInputs info), txOutAddress i == Address (ScriptCredential $ ownHash ctx) Nothing ] in
        length is == 1

    getSaleDatum :: CurrencySymbol -> TokenName -> Maybe NFTSale
    getSaleDatum cs tn = let os = [ o | o <- txInfoOutputs info, valueOf (txOutValue o) cs tn == 1 ] in
                  case os of
                    [o] -> nftDatum o (`findDatum` info)

    checkFee :: Integer -> Bool
    checkFee price = fromInteger (Ada.getLovelace (Ada.fromValue (valuePaidTo info (feeAddr mp)))) >= 1 % 100 * fromInteger price

    checkSellerOut :: PubKeyHash -> PubKeyHash -> Integer -> Integer -> Bool
    checkSellerOut seller nroyaddr nroyprct price = if nroyprct  == 0
        then fromInteger (Ada.getLovelace (Ada.fromValue (valuePaidTo info seller))) >= (100 - 1) % 100 * fromInteger price
        else checkSellerOut' seller nroyprct price && checkRoyalty nroyaddr nroyprct price

    checkSellerOut' :: PubKeyHash -> Integer -> Integer -> Bool
    checkSellerOut' seller royPrct price = fromInteger (Ada.getLovelace (Ada.fromValue (valuePaidTo info seller))) >= (1000 - 10 - royPrct) % 1000 * fromInteger price

    checkRoyalty :: PubKeyHash -> Integer -> Integer -> Bool
    checkRoyalty royAddr royPrct price = fromInteger (Ada.getLovelace (Ada.fromValue (valuePaidTo info royAddr))) >= royPrct % 1000 * fromInteger price

    checkDatum :: Bool
    checkDatum = case getSaleDatum (nCurrency nfts) (nToken nfts) of
      Just ns -> nSeller ns == nSeller nfts && nCurrency ns == nCurrency nfts && nToken ns == nToken nfts && nRoyAddr ns == nRoyAddr nfts && nRoyPrct ns == nRoyPrct nfts

    checkContinuing :: CurrencySymbol -> TokenName -> Bool
    checkContinuing cs tn = let cos = [ co | co <- getContinuingOutputs ctx, valueOf (txOutValue co) cs tn == 1 ] in
        length cos == 1

    checkDatumUpdate :: Bool
    checkDatumUpdate = case getSaleDatum (nCurrency nfts) (nToken nfts) of
        Just ns -> nPrice ns == nPrice nfts && checkDatum

    checkUpdate :: CurrencySymbol -> TokenName -> Bool
    checkUpdate cs tn = case getTokenDatum of
        Just uvh -> let
            addrv2 = Address (ScriptCredential (vhash uvh)) Nothing
            os = [ o | o <- txInfoOutputs info, txOutAddress o == addrv2 && valueOf (txOutValue o) cs tn == 1 ] in
                length os == 1



data Sale
instance Scripts.ValidatorTypes Sale where
    type instance DatumType Sale    = NFTSale
    type instance RedeemerType Sale = SaleAction


typedBuyValidator :: MarketParams -> Scripts.TypedValidator Sale
typedBuyValidator mp = Scripts.mkTypedValidator @Sale
    ($$(PlutusTx.compile [|| mkBuyValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode mp)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @NFTSale @SaleAction


buyValidator :: MarketParams -> Validator
buyValidator = Scripts.validatorScript . typedBuyValidator

buyValidatorHash :: MarketParams -> ValidatorHash
buyValidatorHash = validatorHash . buyValidator

buyScript :: MarketParams -> Plutus.Script
buyScript = Ledger.unValidatorScript . buyValidator

buyScriptAsShortBs :: MarketParams -> SBS.ShortByteString
buyScriptAsShortBs = SBS.toShort . LB.toStrict . serialise . buyScript

apiBuyScript :: MarketParams -> PlutusScript PlutusScriptV1
apiBuyScript = PlutusScriptSerialised . buyScriptAsShortBs