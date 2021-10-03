{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE NumericUnderscores         #-}

module Plutus.Contracts.Market
    ( endpoints
    , MarketSchema
    , NFTParams (..)
    , PayParams (..)
    , BuyParams (..)
    , test
    ) where

import qualified Data.Map                  as Map
import           Plutus.Contract           as Contract
import qualified PlutusTx
import           Wallet.Emulator.Wallet    ( walletPubKey, Wallet(Wallet) )
import           Plutus.Trace.Emulator     as Emulator
import           Control.Monad             hiding (fmap)
import           Data.Aeson                (ToJSON, FromJSON)
import           Data.Text                 (Text)
import           Data.ByteString.Char8 ( ByteString, pack, unpack )
import           GHC.Generics              (Generic)
import           PlutusTx.Prelude          as Plutus hiding (Semigroup(..), unless)
import           Ledger                    hiding (singleton)
import           Ledger.Constraints        as Constraints
import qualified Ledger.Scripts            as Scripts
import qualified Ledger.Typed.Scripts      as Scripts
import           Ledger.Value              as Value
import           Ledger.Ada as Ada ( lovelaceValueOf )
import           Prelude                   (Semigroup (..), Show (..), String, IO)
import qualified Prelude
import           Schema                    (ToSchema)
import           Text.Printf               (printf)

{-
 - This contract is created by the NFT issuer.
 - That creates a script instance on the blockchain that waits for payment.
 - When a customer sends the required ADA to the script, the script sends the NFT to the customer and the ADA to the seller.
 -}

data NFTSale = NFTSale
    { nSeller    :: !PubKeyHash
    , nRoyalties :: ![PubKeyHash]
    , nPrice     :: !Integer
    , nCurrency  :: !CurrencySymbol
    , nToken     :: !TokenName
    , nAmount    :: !Integer
    } deriving (Prelude.Eq, Prelude.Ord, Show, Generic, ToJSON, FromJSON, ToSchema)

instance Eq NFTSale where
    {-# INLINABLE (==) #-}
    a == b = (nSeller    a == nSeller    b) &&
             (nRoyalties a == nRoyalties b) &&
             (nPrice     a == nPrice     b) &&
             (nCurrency  a == nCurrency  b) &&
             (nToken     a == nToken     b) &&
             (nAmount    a == nAmount    b)

PlutusTx.makeIsDataIndexed ''NFTSale [('NFTSale, 0)]
PlutusTx.makeLift ''NFTSale

data SaleParams = SaleParams {
      spCurrency  :: !CurrencySymbol
    , spToken     :: !TokenName
    , spSeller    :: !PubKeyHash
    , spRoyalties :: ![PubKeyHash]
    , spPrice     :: !Integer
    }

PlutusTx.makeIsDataIndexed ''SaleParams [('SaleParams, 0)]
PlutusTx.makeLift ''SaleParams

data NFTParams = NFTParams
     { tPrice     :: !Integer
     , tRoyalties :: ![PubKeyHash]
     , tAmount    :: !Integer
     } deriving (Prelude.Eq, Prelude.Ord, Show, Generic, ToJSON, FromJSON, ToSchema)

PlutusTx.makeIsDataIndexed ''NFTParams [('NFTParams, 0)]
PlutusTx.makeLift '' NFTParams

data SaleAction = SendNFT PubKeyHash TokenName | GetADA
    deriving Show

PlutusTx.makeIsDataIndexed ''SaleAction [('SendNFT, 0), ('GetADA, 1)]
PlutusTx.makeLift ''SaleAction

data SaleDatum = SaleDatum
    { ndSale  :: !NFTSale
    , ndId    :: !Integer
    } deriving Show

instance Eq SaleDatum where
    {-# INLINABLE (==) #-}
    a == b = ndSale a == ndSale b &&
             ndId   a == ndId   b

PlutusTx.makeIsDataIndexed ''SaleDatum [('SaleDatum, 0)]
PlutusTx.makeLift ''SaleDatum

data NFTOnSale
instance Scripts.ValidatorTypes NFTOnSale where
    type instance RedeemerType NFTOnSale = SaleAction
    type instance DatumType    NFTOnSale = SaleDatum

{-# INLINABLE mkPolicy #-}
mkPolicy :: TxOutRef -> () -> ScriptContext -> Bool
mkPolicy oref () ctx = traceIfFalse "UTxO not consumed"   hasUTxO           &&
                       traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx
    
    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = f $ flattenValue (txInfoMint info)
      where 
        f :: [(CurrencySymbol, TokenName, Integer)] -> Bool
        f []                     = False
        f [(cs, _tn, amt)]       = cs == ownCurrencySymbol ctx && amt == 1
        f ((cs, _tn, amt) : txs) = cs == ownCurrencySymbol ctx && amt == 1 && f txs

policy :: TxOutRef -> NFTParams -> Scripts.MintingPolicy
policy oref nftp = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkPolicy ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref

curSymbol :: TxOutRef -> NFTParams -> CurrencySymbol
curSymbol oref nftp = scriptCurrencySymbol $ policy oref nftp

{-# INLINABLE saleDatum #-}
saleDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe SaleDatum
saleDatum o f = do
    dh <- txOutDatum o
    Datum d <- f dh
    PlutusTx.fromBuiltinData d

{-# INLINABLE mkSaleValidator #-}
mkSaleValidator :: SaleParams -> SaleDatum -> SaleAction -> ScriptContext -> Bool
mkSaleValidator sp sd redeemer ctx = 
    traceIfFalse "Datum-in invalid" datumInValid &&
    traceIfFalse "You do not have permission to perform this action" checkUser &&
    case redeemer of
        SendNFT pkh tn -> 
            traceIfFalse "wrong output token value" (checkOutputTokenValue pkh tn)
        GetADA      ->
            traceIfFalse "Wrong output ada value" checkOutputAdaValue
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    price :: Integer
    price = spPrice sp

    seller :: PubKeyHash
    seller = spSeller sp

    royalties :: [PubKeyHash]
    royalties = spRoyalties sp

    royalrecv :: Integer
    royalrecv = Plutus.length royalties
                                  
    cs :: CurrencySymbol
    cs = spCurrency sp

    tn' :: TokenName
    tn' = spToken sp

    datumInValid :: Bool
    datumInValid = isJust getNFTDatum

    getNFTDatum :: Maybe SaleDatum
    getNFTDatum = let is = [ i | i <- Plutus.map txInInfoResolved (txInfoInputs info), txOutValue i == Value.singleton cs tn' 1 ] in
                  case is of
                    [i] -> saleDatum i (`findDatum` info)
                    _   -> Nothing

    checkOutputAdaValue :: Bool
    checkOutputAdaValue = True -- TODO, need to verify that there are exactly  `royalrecv` ada outputs going exactly to all `royalties`

    checkOutputTokenValue :: PubKeyHash -> TokenName -> Bool
    checkOutputTokenValue pkh tn = let os = [ o | o <- txInfoOutputs info, txOutValue o == Value.singleton cs tn 1 && txOutAddress o == pubKeyHashAddress pkh ] in
        case os of
            [_o] -> True
            _    -> False

    checkUser :: Bool
    checkUser = txSignedBy info seller

saleInstance :: SaleParams -> Scripts.TypedValidator NFTOnSale
saleInstance p = Scripts.mkTypedValidator @NFTOnSale
    ($$(PlutusTx.compile [|| mkSaleValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode p)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @SaleDatum @SaleAction

saleValidator :: SaleParams -> Validator
saleValidator = Scripts.validatorScript . saleInstance

saleAddress :: SaleParams -> Ledger.Address
saleAddress = Ledger.scriptAddress . saleValidator

data PayParams = PayParams
    { ppAmount    :: !Integer
    , ppCurSymbol :: !CurrencySymbol
    } deriving (Prelude.Eq, Prelude.Ord, Show, Generic, ToJSON, FromJSON, ToSchema)

data BuyParams = BuyParams
    { bpCurSymbol :: !CurrencySymbol
    , bpToken     :: !TokenName
    } deriving (Prelude.Eq, Prelude.Ord, Show, Generic, ToJSON, FromJSON, ToSchema)

type MarketSchema =
    Endpoint "buy" BuyParams
        .\/ Endpoint "payToScript" PayParams
        .\/ Endpoint "startSale" NFTParams

packStr'' :: String -> ByteString
packStr'' = Data.ByteString.Char8.pack

unpackStr'' :: ByteString -> String
unpackStr'' = Data.ByteString.Char8.unpack

startSale :: NFTParams -> Contract w  MarketSchema Text ()
startSale nftp = do
    pk    <- Contract.ownPubKey
    pkh   <- pubKeyHash <$> Contract.ownPubKey
    utxos <- utxoAt (pubKeyAddress pk)
    case Map.keys utxos of
        [] -> Contract.logError @String "no utxo found"
        oref : _ -> do
            let s   = NFTSale
                      { nSeller    = pkh
                      , nRoyalties = tRoyalties nftp
                      , nPrice     = tPrice nftp
                      , nCurrency  = curSymbol oref nftp 
                      , nToken     = TokenName "The Legend Of Kaizen"
                      , nAmount    = tAmount nftp
                      }
                sd  = SaleDatum
                      { ndSale  = s
                      , ndId    = 0
                      }
                sp  = SaleParams
                      { spCurrency  = nCurrency s
                      , spToken     = nToken    s
                      , spSeller    = pkh
                      , spPrice     = tPrice nftp
                      , spRoyalties = tRoyalties nftp
                      }
                names   = [ TokenName $ toBuiltin $ pack ("The Legend Of Kaizen[" ++ show x ++ "]") | x <- [1..(tAmount nftp)] ]
                vals    = [ Value.singleton (nCurrency s) name 1 | name <- names ]
                val'    = Value.singleton (nCurrency s) (TokenName "The Legend Of Kaizen") 1
                lookups = Constraints.mintingPolicy (policy oref nftp) <> Constraints.unspentOutputs utxos <> Constraints.typedValidatorLookups (saleInstance sp)
                tx      = mconcat [ Constraints.mustMintValue val | val <- vals ]        <>
                          Constraints.mustMintValue val'                                 <>
                          Constraints.mustSpendPubKeyOutput oref                          <>
                          mconcat [ Constraints.mustPayToTheScript sd val | val <- vals ] <>
                          Constraints.mustPayToTheScript sd val'
            ledgerTx <- submitTxConstraintsWith @NFTOnSale lookups tx
            void $ awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ printf "Currency Symbol : %s" (unpack $ fromBuiltin $ unCurrencySymbol $ nCurrency s)

payToScript :: PayParams -> Contract w  MarketSchema Text ()
payToScript pp = do
    let sp  = SaleParams { spCurrency = ppCurSymbol pp
                         , spToken = TokenName "The Legend Of Kaizen"
                         , spSeller = pubKeyHash $ walletPubKey $ Wallet 1
                         , spRoyalties = [pubKeyHash $ walletPubKey $ Wallet 3]
                         , spPrice = 500_000 }
    sale <- findSale sp
    case sale of
        Nothing -> Contract.logError @String $ printf "no sale found for this token name cs : %s" (show $ ppCurSymbol pp)
        Just (_oref, _o, sd, _x) -> do
            logInfo @String "found sale"
            let nftsale = (ndSale sd) { nSeller = pubKeyHash $ walletPubKey (Wallet 3), nToken = TokenName emptyByteString }
                sd' = sd { ndSale = nftsale }
                lookups = Constraints.typedValidatorLookups (saleInstance sp)   <>
                          Constraints.otherScript (saleValidator sp)
                tx = Constraints.mustPayToTheScript sd' (Ada.lovelaceValueOf $ ppAmount pp)
            ledgerTx <- submitTxConstraintsWith lookups tx
            void $ awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ printf "sent %s lovelace" (show $ ppAmount pp)

buy :: BuyParams -> Contract w  MarketSchema Text ()
buy bp = do
    pk <- Contract.ownPubKey
    let sp = SaleParams { spCurrency = bpCurSymbol bp
                         , spToken = TokenName "The Legend Of Kaizen"
                         , spSeller = pubKeyHash pk
                         , spRoyalties = [pubKeyHash $ walletPubKey $ Wallet 3]
                         , spPrice = 500_000 }
    sale <- findSale sp
    case sale of 
        Nothing -> Contract.logError @String $ printf "no sale found for this token name cs : %s" (show $ bpCurSymbol bp)
        Just (oref', o', sd, _) -> do
            payment <- findPayment (nPrice $ ndSale sd) sp
            let royalties = nRoyalties $ ndSale sd
                royalrecv = Plutus.length royalties
            case payment of
                Nothing           -> Contract.logError @String "Payment to the script has not arrived"
                Just (oref, o, x) -> do
                  let tx'      = txOutTxTx o
                      pkh'     = pubKeyHash $ Plutus.head $ Map.keys (txSignatures tx')
                      r        = Redeemer $ PlutusTx.toBuiltinData GetADA
                      tnStr    = "The Legend Of Kaizen[" ++ show (ndId sd + 1) ++ "]"
                      tn       = TokenName $ toBuiltin $ pack tnStr
                      r'       = Redeemer $ PlutusTx.toBuiltinData $ SendNFT pkh' tn
                      sd'      = sd { ndId = ndId sd + 1 }
                      val      = Value.singleton (bpCurSymbol bp) tn 1
                      sp'      = sp { spToken = tn }
                  nftB <- findSale sp'
                  case nftB of
                    Nothing -> Contract.logError @String "no more nfts"
                    Just (oref'', o'', _, _) -> do
                      let val'     = Value.singleton (bpCurSymbol bp) (TokenName "The Legend Of Kaizen") 1
                          lookups  = Constraints.typedValidatorLookups (saleInstance sp) <>
                                     Constraints.otherScript (saleValidator sp)          <>
                                     Constraints.unspentOutputs (Map.singleton oref o)   <>
                                     Constraints.unspentOutputs (Map.singleton oref' o') <>
                                     Constraints.unspentOutputs (Map.singleton oref'' o'')
                          tx       = mconcat [ Constraints.mustPayToPubKey pkh (Ada.lovelaceValueOf $ Plutus.divide x royalrecv) | pkh <- royalties ] <>
                                     Constraints.mustSpendScriptOutput oref r                                                                         <>
                                     Constraints.mustPayToPubKey pkh' val                                                                             <>
                                     Constraints.mustSpendScriptOutput oref' r'                                                                       <>
                                     Constraints.mustSpendScriptOutput oref'' r'                                                                      <>
                                     Constraints.mustPayToTheScript sd' val'
                      ledgerTx <- submitTxConstraintsWith lookups tx
                      void $ awaitTxConfirmed $ txId ledgerTx
                      logInfo @String $ printf "payer : %s" (show pkh')
                      logInfo @String "Purchase completed"

findSale :: SaleParams -> Contract w  MarketSchema Text (Maybe (TxOutRef, TxOutTx, SaleDatum, Integer))
findSale sp = do
    let sp' = sp { spToken = TokenName "The Legend Of Kaizen" }
    utxos <- Map.filter f <$> utxoAt (scriptAddress $ saleValidator sp')
    return $ case Map.toList utxos of
        [(oref, o)] -> do
            saleDatum' <- getSaleDatum (txOutTxOut o) $ \dh -> Map.lookup dh $ txData $ txOutTxTx o
            return (oref, o, saleDatum', assetClassValueOf (txOutValue $ txOutTxOut o) (AssetClass (spCurrency sp, spToken sp)))
        _           -> Nothing
  where
    f :: TxOutTx -> Bool
    f o = assetClassValueOf (txOutValue $ txOutTxOut o) (AssetClass (spCurrency sp, spToken sp)) == 1

    getSaleDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe SaleDatum
    getSaleDatum o fh = do
        dh <- txOutDatum o
        Datum d <- fh dh
        PlutusTx.fromBuiltinData d

findPayment :: Integer -> SaleParams -> Contract w  MarketSchema Text (Maybe (TxOutRef, TxOutTx, Integer))
findPayment price sp = do
    utxos <- Map.filter f <$> utxoAt (scriptAddress $ saleValidator sp)
    return $ g $ Map.toList utxos
  where
    g :: [(TxOutRef, TxOutTx)] -> Maybe (TxOutRef, TxOutTx, Integer)
    g [(oref, o)] = let x = assetClassValueOf (txOutValue $ txOutTxOut o) (AssetClass (CurrencySymbol emptyByteString, TokenName emptyByteString)) in
                        Just (oref, o, x)
    g ((_, _) : os) = g os
    g _ = Nothing

    f :: TxOutTx -> Bool
    f o = assetClassValueOf (txOutValue $ txOutTxOut o) (AssetClass (CurrencySymbol emptyByteString, TokenName emptyByteString)) >= price

{-go :: (AsContractError e) => BuyParams -> Contract w  MarketSchema Text ()
go bp = do
    void $ Contract.waitNSlots 1
    void $ buy bp
    void $ go bp-}

endpoints :: Contract ()  MarketSchema Text ()
endpoints = forever
        $ handleError logError
        $ awaitPromise
        $ startSale' `select` payToScript' `select` buy'
    where
        startSale'      = endpoint @"startSale"  $ \nftp -> startSale nftp
        payToScript'    = endpoint @"payToScript" $ \pp -> payToScript pp
        buy'            = endpoint @"buy" $ \bp -> buy bp

test :: IO ()
test = runEmulatorTraceIO $ do
    let nftpar = NFTParams 
                    { tPrice  = 500_000
                    , tRoyalties = Plutus.map (pubKeyHash . walletPubKey . Wallet) [1,3]
                    , tAmount = 10
                    }
        paypar = PayParams
                    { ppAmount    = 600_000
                    , ppCurSymbol = "ccc500e80f045b6630be96d509e75fac6e2d05c5aa8dffe2a1c184fa" 
                    }
        bp     = BuyParams
                    { bpCurSymbol = "ccc500e80f045b6630be96d509e75fac6e2d05c5aa8dffe2a1c184fa", bpToken = TokenName "The Legend Of Kaizen" }
    h1 <- activateContractWallet (Wallet 1) endpoints
    void $ Emulator.waitNSlots 1
    callEndpoint @"startSale" h1 nftpar
    void $ Emulator.waitNSlots 3

    h2 <- activateContractWallet (Wallet 2) endpoints
    callEndpoint @"payToScript" h2 paypar
    h3 <- activateContractWallet (Wallet 3) endpoints
    callEndpoint @"payToScript" h3 paypar
    void $ Emulator.waitNSlots 2
    h4 <- activateContractWallet (Wallet 4) endpoints
    callEndpoint @"payToScript" h4 paypar
    void $ Emulator.waitNSlots 2
    callEndpoint @"buy" h1 bp
    void $ Emulator.waitNSlots 2
    callEndpoint @"buy" h1 bp
    void $ Emulator.waitNSlots 2
    callEndpoint @"buy" h1 bp
    void $ Emulator.waitNSlots 20
