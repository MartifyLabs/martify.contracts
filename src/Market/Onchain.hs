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
    ( Bool(..), Eq((==)), (.), ($), (&&), traceIfFalse, Integer, Maybe, (>=) )
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
      unValidatorScript, valuePaidTo )
import qualified Ledger.Typed.Scripts      as Scripts
import qualified Plutus.V1.Ledger.Scripts as Plutus
import           Ledger.Value              as Value ( valueOf )
import qualified Plutus.V1.Ledger.Ada as Ada (lovelaceOf, fromValue)

import           Market.Types               (NFTSale(..), SaleAction(..))


{-# INLINABLE nftDatum #-}
nftDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe NFTSale
nftDatum o f = do
    dh <- txOutDatum o
    Datum d <- f dh
    PlutusTx.fromBuiltinData d

{-# INLINABLE mkBuyValidator #-}
mkBuyValidator :: NFTSale -> SaleAction -> ScriptContext -> Bool
mkBuyValidator nfts r ctx =
    case r of
        Buy   -> traceIfFalse "NFT not sent to buyer" checkNFTOut &&
                 traceIfFalse "Seller not paid" checkSellerOut
        Close -> traceIfFalse "No rights to perform this action" checkCloser &&
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

    checkNFTOut :: Bool
    checkNFTOut = valueOf (valuePaidTo info sig) cs tn == 1
    
    checkSellerOut :: Bool
    checkSellerOut = Ada.fromValue (valuePaidTo info seller) >= Ada.lovelaceOf price

    checkCloser :: Bool
    checkCloser = txSignedBy info seller

    checkCloseOut :: Bool
    checkCloseOut = valueOf (valuePaidTo info seller) cs tn == 1


data Sale
instance Scripts.ValidatorTypes Sale where
    type instance DatumType Sale    = NFTSale
    type instance RedeemerType Sale = SaleAction


typedBuyValidator :: Scripts.TypedValidator Sale
typedBuyValidator = Scripts.mkTypedValidator @Sale
    $$(PlutusTx.compile [|| mkBuyValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @NFTSale @SaleAction


buyValidator :: Validator
buyValidator = Scripts.validatorScript typedBuyValidator

buyScript :: Plutus.Script
buyScript = Ledger.unValidatorScript buyValidator

buyScriptAsShortBs :: SBS.ShortByteString
buyScriptAsShortBs = SBS.toShort . LB.toStrict $ serialise buyScript

apiBuyScript :: PlutusScript PlutusScriptV1
apiBuyScript = PlutusScriptSerialised buyScriptAsShortBs