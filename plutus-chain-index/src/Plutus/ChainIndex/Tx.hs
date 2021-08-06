{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-| The chain index' version of a transaction
-}
module Plutus.ChainIndex.Tx(
    ChainIndexTx(..)
    , fromOnChainTx
    , txOutRefs
    , txOutsWithRef
    , txOutRefMap
    , txOutRefMapForAddr
    -- ** Lenses
    , citxTxId
    , citxInputs
    , citxOutputs
    , citxValidRange
    , citxData
    , citxRedeemers
    , citxMintingPolicies
    , citxStakeValidators
    , citxValidators
    ) where

import           Control.Lens              (makeLenses)
import           Data.Aeson                (FromJSON, ToJSON)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Data.Text.Prettyprint.Doc
import           Data.Tuple                (swap)
import           GHC.Generics              (Generic)
import           Ledger                    (Address, Datum, DatumHash, MintingPolicy, MintingPolicyHash, OnChainTx (..),
                                            Redeemer (..), RedeemerHash, SlotRange, StakeValidator, StakeValidatorHash,
                                            Tx (..), TxId, TxIn (txInType), TxInType (..), TxOut (txOutAddress),
                                            TxOutRef (..), Validator, ValidatorHash, datumHash, mintingPolicyHash,
                                            redeemerHash, txId, validatorHash)

data ChainIndexTx =
    ValidChainIndexTx {
      _citxTxId            :: TxId,
      -- ^ The id of this valid transaction.
      _citxInputs          :: Set TxIn,
      -- ^ The inputs to this valid transaction.
      _citxOutputs         :: [TxOut],
      -- ^ The outputs of this valid transaction, ordered so they can be referenced by index.
      _citxValidRange      :: !SlotRange,
      -- ^ The 'SlotRange' during which this valid transaction may be validated.
      _citxData            :: Map DatumHash Datum,
      -- ^ Datum objects recorded on this valid transaction.
      _citxRedeemers       :: Map RedeemerHash Redeemer,
      -- ^ Redeemers of the minting scripts.
      _citxMintingPolicies :: Map MintingPolicyHash MintingPolicy,
      -- ^ The scripts used to check minting conditions.
      _citxStakeValidators :: Map StakeValidatorHash StakeValidator,
      _citxValidators      :: Map ValidatorHash Validator
    }
  | InvalidChainIndexTx {
      _citxTxId            :: TxId,
      -- ^ The id of this invalid transaction.
      _citxInputs          :: Set TxIn,
      -- ^ The collateral inputs to this invalid transaction.
      _citxValidRange      :: !SlotRange,
      -- ^ The 'SlotRange' during which this transaction may be validated.
      _citxData            :: Map DatumHash Datum,
      -- ^ Datum objects recorded on this invalid transaction.
      _citxRedeemers       :: Map RedeemerHash Redeemer,
      -- ^ Redeemers of the minting scripts.
      _citxMintingPolicies :: Map MintingPolicyHash MintingPolicy,
      -- ^ The scripts used to check minting conditions.
      _citxStakeValidators :: Map StakeValidatorHash StakeValidator,
      _citxValidators      :: Map ValidatorHash Validator
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)

makeLenses ''ChainIndexTx

instance Pretty ChainIndexTx where
    pretty ValidChainIndexTx{_citxTxId, _citxInputs, _citxOutputs, _citxValidRange, _citxMintingPolicies, _citxData, _citxRedeemers} =
        let lines' =
                [ hang 2 (vsep ("inputs:" : fmap pretty (Set.toList _citxInputs)))
                , hang 2 (vsep ("outputs:" : fmap pretty _citxOutputs))
                , hang 2 (vsep ("minting policies:": fmap (pretty . fst) (Map.toList _citxMintingPolicies)))
                , "validity range:" <+> viaShow _citxValidRange
                , hang 2 (vsep ("data:": fmap (pretty . snd) (Map.toList _citxData) ))
                , hang 2 (vsep ("redeemers:": fmap (pretty . snd) (Map.toList _citxRedeemers) ))
                ]
        in nest 2 $ vsep ["Valid tx" <+> pretty _citxTxId <> colon, braces (vsep lines')]
    pretty InvalidChainIndexTx{_citxTxId, _citxInputs, _citxValidRange, _citxMintingPolicies, _citxData, _citxRedeemers} =
        let lines' =
                [ hang 2 (vsep ("inputs:" : fmap pretty (Set.toList _citxInputs)))
                , hang 2 (vsep ["no outputs:"])
                , hang 2 (vsep ("minting policies:": fmap (pretty . fst) (Map.toList _citxMintingPolicies)))
                , "validity range:" <+> viaShow _citxValidRange
                , hang 2 (vsep ("data:": fmap (pretty . snd) (Map.toList _citxData) ))
                , hang 2 (vsep ("redeemers:": fmap (pretty . snd) (Map.toList _citxRedeemers) ))
                ]
        in nest 2 $ vsep ["Invalid tx" <+> pretty _citxTxId <> colon, braces (vsep lines')]

-- | Get tx output references from tx.
txOutRefs :: ChainIndexTx -> [TxOutRef]
txOutRefs ValidChainIndexTx{_citxTxId, _citxOutputs} =
  map (\idx -> TxOutRef _citxTxId idx) [0 .. fromIntegral $ length _citxOutputs - 1]
txOutRefs InvalidChainIndexTx {} = []

-- | Get tx output references and tx outputs from tx.
txOutsWithRef :: ChainIndexTx -> [(TxOut, TxOutRef)]
txOutsWithRef tx@ValidChainIndexTx{_citxOutputs} = zip _citxOutputs $ txOutRefs tx
txOutsWithRef InvalidChainIndexTx {}             = []

-- | Get 'Map' of tx outputs references to tx.
txOutRefMap :: ChainIndexTx -> Map TxOutRef (TxOut, ChainIndexTx)
txOutRefMap tx =
    fmap (, tx) $ Map.fromList $ fmap swap $ txOutsWithRef tx

-- | Get 'Map' of tx outputs from tx for a specific address.
txOutRefMapForAddr :: Address -> ChainIndexTx -> Map TxOutRef (TxOut, ChainIndexTx)
txOutRefMapForAddr addr tx =
    Map.filter ((==) addr . txOutAddress . fst) $ txOutRefMap tx

-- | Convert a 'OnChainTx' to a 'ChainIndexTx'. An invalid 'OnChainTx' will not
-- produce any 'ChainIndexTx' outputs and the collateral inputs of the
-- 'OnChainTx' will be the inputs of the 'ChainIndexTx'.
fromOnChainTx :: OnChainTx -> ChainIndexTx
fromOnChainTx = \case
    Valid tx@Tx{txInputs, txOutputs, txValidRange, txData, txMintScripts} ->
        let (validatorHashes, otherDataHashes, redeemers) = validators txInputs in
        ValidChainIndexTx
            { _citxTxId = txId tx
            , _citxInputs = txInputs
            , _citxOutputs = txOutputs
            , _citxValidRange = txValidRange
            , _citxData = txData <> otherDataHashes
            , _citxRedeemers = redeemers
            , _citxMintingPolicies = mintingPolicies txMintScripts
            , _citxStakeValidators = mempty
            , _citxValidators = validatorHashes
            }
    Invalid tx@Tx{txCollateral, txValidRange, txData, txInputs, txMintScripts} ->
        let (validatorHashes, otherDataHashes, redeemers) = validators txInputs in
        InvalidChainIndexTx
            { _citxTxId = txId tx
            , _citxInputs = txCollateral
            , _citxValidRange = txValidRange
            , _citxData = txData <> otherDataHashes
            , _citxRedeemers = redeemers
            , _citxMintingPolicies = mintingPolicies txMintScripts
            , _citxStakeValidators = mempty
            , _citxValidators = validatorHashes
            }

mintingPolicies :: Set MintingPolicy -> Map MintingPolicyHash MintingPolicy
mintingPolicies =
    let withHash mps = (mintingPolicyHash mps, mps) in
    Map.fromList . fmap withHash . Set.toList

validators :: Set TxIn -> (Map ValidatorHash Validator, Map DatumHash Datum, Map RedeemerHash Redeemer)
validators =
    let withHash (ConsumeScriptAddress val red dat) =
            ( Map.singleton (validatorHash val) val
            , Map.singleton (datumHash dat) dat
            , Map.singleton (redeemerHash red) red
            )
        withHash ConsumePublicKeyAddress    = mempty
    in foldMap (maybe mempty withHash . txInType) . Set.toList
