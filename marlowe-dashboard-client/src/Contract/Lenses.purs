module Contract.Lenses
  ( _followerAppId
  , _marloweParams
  , _metadata
  , _executionState
  , _tab
  , _previousSteps
  , _selectedStep
  , _participants
  , _userParties
  , _namedActions
  ) where

import Contract.Types (PreviousStep, State, Tab)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Set (Set)
import Data.Symbol (SProxy(..))
import Marlowe.Execution.Types (NamedAction)
import Marlowe.Execution.Types (State) as Execution
import Marlowe.Extended.Metadata (MetaData)
import Marlowe.PAB (PlutusAppId, MarloweParams)
import Marlowe.Semantics as Semantic
import WalletData.Types (WalletNickname)

_followerAppId :: Lens' State PlutusAppId
_followerAppId = prop (SProxy :: SProxy "followerAppId")

_marloweParams :: Lens' State MarloweParams
_marloweParams = prop (SProxy :: SProxy "marloweParams")

_metadata :: Lens' State MetaData
_metadata = prop (SProxy :: SProxy "metadata")

_executionState :: Lens' State Execution.State
_executionState = prop (SProxy :: SProxy "executionState")

_tab :: forall a. Lens' { tab :: Tab | a } Tab
_tab = prop (SProxy :: SProxy "tab")

_previousSteps :: Lens' State (Array PreviousStep)
_previousSteps = prop (SProxy :: SProxy "previousSteps")

_selectedStep :: Lens' State Int
_selectedStep = prop (SProxy :: SProxy "selectedStep")

_participants :: Lens' State (Map Semantic.Party (Maybe WalletNickname))
_participants = prop (SProxy :: SProxy "participants")

_userParties :: Lens' State (Set Semantic.Party)
_userParties = prop (SProxy :: SProxy "userParties")

_namedActions :: Lens' State (Array NamedAction)
_namedActions = prop (SProxy :: SProxy "namedActions")
