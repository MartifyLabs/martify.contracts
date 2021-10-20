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
    , nftDatum
    ) where

import qualified Data.ByteString.Lazy     as LB
import qualified Data.ByteString.Short    as SBS
import           Codec.Serialise          ( serialise )

import           Cardano.Api.Shelley      (PlutusScript (..), PlutusScriptV1)
import qualified PlutusTx
import PlutusTx.Prelude as Plutus
    ( Bool(..), Eq((==)), (.), ($), (&&), traceIfFalse, Integer, Maybe )
import Ledger
    ( TokenName,
      PubKeyHash,
      CurrencySymbol,
      DatumHash,
      Datum(..),
      txOutDatum,
      pubKeyHashAddress,
      txSignedBy,
      ScriptContext(scriptContextTxInfo),
      TxInfo(txInfoOutputs),
      Validator,
      TxOut(txOutValue, txOutAddress),
      unValidatorScript,
      getContinuingOutputs )
import qualified Ledger.Typed.Scripts      as Scripts
import qualified Plutus.V1.Ledger.Scripts as Plutus
import           Ledger.Value              as Value ( singleton, geq, valueOf )
import qualified Plutus.V1.Ledger.Ada as Ada (lovelaceValueOf)

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
        Buy   -> traceIfFalse "Buy output invalid" checkBuyOut &&
                 traceIfFalse "NFT still in script" checkContinuing
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
            [] -> False
            _  -> let os' = [ o | o <- txInfoOutputs info, valueOf (txOutValue o) cs tn == 1 ] in
                    case os' of
                        [_] -> True
                        _   -> False

    checkContinuing :: Bool
    checkContinuing = let os = [ o | o <- getContinuingOutputs ctx, valueOf (txOutValue o) cs tn == 1 ] in
        case os of
            [] -> True
            _  -> False

    checkCloser :: Bool
    checkCloser = txSignedBy info (nSeller nfts)

    checkCloseOut :: Bool
    checkCloseOut = let os = [ o | o <- txInfoOutputs info, txOutValue o == Value.singleton cs tn 1 && txOutAddress o == pubKeyHashAddress seller ] in
        case os of
            [_] -> True
            _   -> False


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
apiBuyScript = PlutusScriptSerialised  buyScriptAsShortBs