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
    ( Bool(..), Eq((==)), (.), (||), length, (&&), Integer, Maybe(..), (>=), fromInteger, (*), ($), (%), (-), map )
import Ledger
    ( PubKeyHash(..),
      ValidatorHash,
      Address(Address),
      validatorHash,
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
      txOutAddress)
import qualified Ledger.Typed.Scripts      as Scripts
import qualified Plutus.V1.Ledger.Scripts as Plutus
import           Ledger.Value              as Value ( valueOf )
import qualified Plutus.V1.Ledger.Ada as Ada (fromValue, Ada (getLovelace))
import           Plutus.V1.Ledger.Credential (Credential(ScriptCredential))


import Market.Types    (NFTSale(..), SaleAction(..), MarketParams(..))


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
               checkSellerOut (nSeller nfts) (nRoyPrct nfts) (nPrice nfts) &&
               checkRoyalty (nRoyAddr nfts) (nRoyPrct nfts) (nPrice nfts) &&
               checkSingleBuy
    Close   -> txSignedBy (scriptContextTxInfo ctx) (nSeller nfts)

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    sig :: PubKeyHash
    sig = case txInfoSignatories info of
            [pubKeyHash] -> pubKeyHash

    checkSingleBuy :: Bool
    checkSingleBuy = let is = [ i | i <- map txInInfoResolved (txInfoInputs info), txOutAddress i == Address (ScriptCredential $ ownHash ctx) Nothing ] in
        length is == 1

    checkFee :: Integer -> Bool
    checkFee price = fromInteger (Ada.getLovelace (Ada.fromValue (valuePaidTo info (feeAddr mp)))) >= 1 % 100 * fromInteger price

    checkSellerOut :: PubKeyHash -> Integer -> Integer -> Bool
    checkSellerOut seller royPrct price = fromInteger (Ada.getLovelace (Ada.fromValue (valuePaidTo info seller))) >= (1000 - 10 - royPrct) % 1000 * fromInteger price

    checkRoyalty :: PubKeyHash -> Integer -> Integer -> Bool
    checkRoyalty royAddr royPrct price = (royPrct == 0) || (fromInteger (Ada.getLovelace (Ada.fromValue (valuePaidTo info royAddr))) >= royPrct % 1000 * fromInteger price)



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