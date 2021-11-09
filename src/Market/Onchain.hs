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
    , nftDatum
    ) where

import qualified Data.ByteString.Lazy     as LB
import qualified Data.ByteString.Short    as SBS
import           Codec.Serialise          ( serialise )

import           Cardano.Api.Shelley      (PlutusScript (..), PlutusScriptV1)
import qualified PlutusTx
import PlutusTx.Prelude as Plutus
    ( Bool(..), Eq((==)), (.), (&&), traceIfFalse, Integer, Maybe(..), (>=), fromInteger, (*), (%) )
import Ledger
    ( TokenName,
      PubKeyHash(..),
      CurrencySymbol,
      DatumHash,
      Datum(..),
      txOutDatum,
      txSignedBy,
      ScriptContext(scriptContextTxInfo),
      TxInfo,
      Validator,
      TxOut,
      txInfoSignatories,
      unValidatorScript, 
      valuePaidTo,
      findDatum,
      txInfoOutputs,
      txOutValue,
      getContinuingOutputs)
import qualified Ledger.Typed.Scripts      as Scripts
import qualified Plutus.V1.Ledger.Scripts as Plutus
import           Ledger.Value              as Value ( valueOf )
import qualified Plutus.V1.Ledger.Ada as Ada (fromValue, Ada (getLovelace))

import           Market.Types               (NFTSale(..), SaleAction(..))


{-# INLINABLE nftDatum #-}
nftDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe NFTSale
nftDatum o f = do
    dh <- txOutDatum o
    Datum d <- f dh
    PlutusTx.fromBuiltinData d

{-# INLINABLE mkBuyValidator #-}
mkBuyValidator :: PubKeyHash -> NFTSale -> SaleAction -> ScriptContext -> Bool
mkBuyValidator pkh nfts r ctx =
    case r of
        Buy    -> traceIfFalse "NFT not sent to buyer" checkNFTOut &&
                  traceIfFalse "Seller not paid" checkSellerOut &&
                  traceIfFalse "Fee not paid" checkFee
        Update -> traceIfFalse "No rights to perform this action" checkUser &&
                  traceIfFalse "Modified datum other than price" checkDatum &&
                  traceIfFalse "NFT left the script" checkContinuingNFT
        Close  -> traceIfFalse "No rights to perform this action" checkUser &&
                  traceIfFalse "Close output invalid" checkCloseOut
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    tn :: TokenName
    tn = nToken nfts

    cs :: CurrencySymbol
    cs = nCurrency nfts

    seller :: PubKeyHash
    seller = nSeller nfts

    sig :: PubKeyHash
    sig = case txInfoSignatories info of
            [pubKeyHash] -> pubKeyHash

    price :: Integer
    price = nPrice nfts

    getSaleDatum :: Maybe NFTSale
    getSaleDatum = let os = [ o | o <- txInfoOutputs info, valueOf (txOutValue o) cs tn == 1 ] in
                  case os of
                    [o] -> nftDatum o (`findDatum` info)
                    _   -> Nothing

    checkNFTOut :: Bool
    checkNFTOut = valueOf (valuePaidTo info sig) cs tn == 1
    
    checkSellerOut :: Bool
    checkSellerOut = fromInteger (Ada.getLovelace (Ada.fromValue (valuePaidTo info seller))) >= 98 % 100 * fromInteger price

    checkFee :: Bool
    checkFee = fromInteger (Ada.getLovelace (Ada.fromValue (valuePaidTo info pkh))) >= 2 % 100 * fromInteger price

    checkUser :: Bool
    checkUser = txSignedBy info seller

    checkDatum :: Bool
    checkDatum = case getSaleDatum of
      Nothing -> False
      Just ns -> nSeller   ns == nSeller   nfts &&
                 nCurrency ns == nCurrency nfts &&
                 nToken    ns == nToken    nfts 

    checkContinuingNFT :: Bool
    checkContinuingNFT = let cos = [ co | co <- getContinuingOutputs ctx, valueOf (txOutValue co) cs tn == 1 ] in
        case cos of
            [_] -> True
            _   -> False

    checkCloseOut :: Bool
    checkCloseOut = valueOf (valuePaidTo info seller) cs tn == 1


data Sale
instance Scripts.ValidatorTypes Sale where
    type instance DatumType Sale    = NFTSale
    type instance RedeemerType Sale = SaleAction


typedBuyValidator :: PubKeyHash -> Scripts.TypedValidator Sale
typedBuyValidator pkh = Scripts.mkTypedValidator @Sale
    ($$(PlutusTx.compile [|| mkBuyValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode pkh)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @NFTSale @SaleAction


buyValidator :: PubKeyHash -> Validator
buyValidator = Scripts.validatorScript . typedBuyValidator

buyScript :: PubKeyHash -> Plutus.Script
buyScript = Ledger.unValidatorScript . buyValidator

buyScriptAsShortBs :: PubKeyHash -> SBS.ShortByteString
buyScriptAsShortBs = SBS.toShort . LB.toStrict . serialise . buyScript

apiBuyScript :: PubKeyHash -> PlutusScript PlutusScriptV1
apiBuyScript = PlutusScriptSerialised . buyScriptAsShortBs
