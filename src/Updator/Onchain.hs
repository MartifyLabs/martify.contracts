{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}

module Updator.Onchain
    ( apiUpdateScript
    , updateScriptAsShortBs
    , typedUpdateValidator
    , Updator
    , updateValidator
    , updateValidatorHash
    , tokenDatum
    ) where

import qualified Data.ByteString.Lazy     as LB
import qualified Data.ByteString.Short    as SBS
import           Codec.Serialise          ( serialise )

import           Cardano.Api.Shelley      (PlutusScript (..), PlutusScriptV1)
import qualified PlutusTx
import PlutusTx.Prelude as Plutus
    ( Bool(..), Eq((==)), (.), Maybe(..), length )
import Ledger
    ( TokenName,
      ValidatorHash,
      CurrencySymbol,
      DatumHash,
      Datum(..),
      txOutDatum,
      txSignedBy,
      ScriptContext(scriptContextTxInfo),
      Validator,
      TxOut,
      unValidatorScript,
      txOutValue,
      getContinuingOutputs, validatorHash)
import qualified Ledger.Typed.Scripts      as Scripts
import qualified Plutus.V1.Ledger.Scripts as Plutus
import           Ledger.Value              as Value ( valueOf )

import           Updator.Types               (MarketParams'(..), UpdateVHash(..))

{-# INLINABLE tokenDatum #-}
tokenDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe UpdateVHash
tokenDatum o f = do
    dh <- txOutDatum o
    Datum d <- f dh
    PlutusTx.fromBuiltinData d

{-# INLINABLE mkUpdateValidator #-}
mkUpdateValidator :: MarketParams' -> UpdateVHash -> PlutusTx.BuiltinData -> ScriptContext -> Bool
mkUpdateValidator mp _ _ ctx
  | txSignedBy (scriptContextTxInfo ctx) (feeAddrNami mp) = checkContinuing (updateCs' mp) (updateTn' mp)
  | txSignedBy (scriptContextTxInfo ctx) (feeAddrCli mp) = checkContinuing (updateCs' mp) (updateTn' mp)
  where
    checkContinuing :: CurrencySymbol -> TokenName -> Bool
    checkContinuing cs tn = let cos = [co | co <- getContinuingOutputs ctx, valueOf (txOutValue co) cs tn == 1]
        in length cos == 1


data Updator
instance Scripts.ValidatorTypes Updator where
    type instance DatumType Updator    = UpdateVHash
    type instance RedeemerType Updator = PlutusTx.BuiltinData


typedUpdateValidator :: MarketParams' -> Scripts.TypedValidator Updator
typedUpdateValidator mp = Scripts.mkTypedValidator @Updator
    ($$(PlutusTx.compile [|| mkUpdateValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode mp)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @UpdateVHash @PlutusTx.BuiltinData


updateValidator :: MarketParams' -> Validator
updateValidator = Scripts.validatorScript . typedUpdateValidator

updateValidatorHash :: MarketParams' -> ValidatorHash
updateValidatorHash = validatorHash . updateValidator

updateScript :: MarketParams' -> Plutus.Script
updateScript = Ledger.unValidatorScript . updateValidator

updateScriptAsShortBs :: MarketParams' -> SBS.ShortByteString
updateScriptAsShortBs = SBS.toShort . LB.toStrict . serialise . updateScript

apiUpdateScript :: MarketParams' -> PlutusScript PlutusScriptV1
apiUpdateScript = PlutusScriptSerialised . updateScriptAsShortBs
