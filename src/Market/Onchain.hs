{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}

module Market.Onchain
    ( apiBuyScript
    , typedBuyValidator
    , Sale
    , buyValidator
    ) where

import qualified Data.ByteString.Lazy     as LB
import qualified Data.ByteString.Short    as SBS
import           Codec.Serialise          ( serialise )

import           Cardano.Api.Shelley      (PlutusScript (..), PlutusScriptV1)
import qualified PlutusTx
import PlutusTx.Prelude as Plutus
    ( Bool(..), Eq((==)), (.), (&&), traceIfFalse, Integer, BuiltinData )
import Ledger
    ( TokenName,
      PubKeyHash,
      CurrencySymbol,
      pubKeyHashAddress,
      txSignedBy,
      ScriptContext(scriptContextTxInfo),
      TxInfo(txInfoOutputs),
      Validator,
      TxOut(txOutValue, txOutAddress),
      unValidatorScript )
import qualified Ledger.Typed.Scripts      as Scripts
import qualified Plutus.V1.Ledger.Scripts as Plutus
import           Ledger.Value              as Value ( geq, valueOf )
import qualified Plutus.V1.Ledger.Ada as Ada (lovelaceValueOf)

import           Market.Types               (NFTSale(..), SaleAction(..))


{-# INLINABLE mkBuyValidator #-}
mkBuyValidator :: NFTSale -> BuiltinData -> SaleAction -> ScriptContext -> Bool
mkBuyValidator nfts _ r ctx =
    case r of
        Buy   -> traceIfFalse "Buy output invalid" checkBuyOut
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

    price :: Integer
    price = nPrice nfts

    checkBuyOut :: Bool
    checkBuyOut = let os = [ o | o <- txInfoOutputs info, txOutValue o `geq` Ada.lovelaceValueOf price && txOutAddress o == pubKeyHashAddress seller ] in
        case os of
            [_] -> let os' = [ o | o <- txInfoOutputs info, valueOf (txOutValue o) cs tn == 1 ] in
                    case os' of
                        [_] -> True
                        _   -> False
            _  -> False

    checkCloser :: Bool
    checkCloser = txSignedBy info (nSeller nfts)

    checkCloseOut :: Bool
    checkCloseOut = let os = [ o | o <- txInfoOutputs info, valueOf (txOutValue o) cs tn == 1 && txOutAddress o == pubKeyHashAddress seller ] in
        case os of
            [_] -> True
            _   -> False


data Sale
instance Scripts.ValidatorTypes Sale where
    type instance DatumType Sale    = BuiltinData
    type instance RedeemerType Sale = SaleAction


typedBuyValidator :: NFTSale -> Scripts.TypedValidator Sale
typedBuyValidator chn = Scripts.mkTypedValidator @Sale
    ($$(PlutusTx.compile [|| mkBuyValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode chn)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @BuiltinData @SaleAction


buyValidator :: NFTSale -> Validator
buyValidator = Scripts.validatorScript . typedBuyValidator

buyScript :: NFTSale -> Plutus.Script
buyScript = Ledger.unValidatorScript . buyValidator

buyScriptAsShortBs :: NFTSale -> SBS.ShortByteString
buyScriptAsShortBs = SBS.toShort . LB.toStrict . serialise . buyScript

apiBuyScript :: NFTSale -> PlutusScript PlutusScriptV1
apiBuyScript = PlutusScriptSerialised . buyScriptAsShortBs