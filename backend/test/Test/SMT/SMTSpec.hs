-- | State machine testing for Edna.

module Test.SMT.SMTSpec
  ( spec
  ) where

import Universum

import Hedgehog
  (Callback(..), Command(..), HTraversable(..), MonadGen, executeSequential, forAll, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lens.Micro.Platform (at, (?~))
import RIO (runRIO)
import Test.Hspec (Spec, context, it)
import Test.Hspec.Hedgehog (hedgehog)

import Edna.ExperimentReader.Types (FileContents(..), TargetMeasurements(..))
import Edna.Setup (EdnaContext)
import Edna.Upload.Service (parseFile')
import Edna.Web.Types (FileSummary(..), FileSummaryItem(..))

import Test.Gen
import Test.SMT.State
import Test.Setup (withContext)

spec :: Spec
spec = context "State machine tests" $ withContext $
  it "Edna implementation matches Edna model" $ \ctx -> hedgehog $ do
    actions <- forAll $
      Gen.sequential (Range.linear 1 100) initialState
        [ cmdParseFile ctx
        ]
    executeSequential initialState actions

newtype ParseFile (v :: Type -> Type) = ParseFile FileContents
  deriving stock (Show, Eq)

instance HTraversable ParseFile where
  htraverse _ (ParseFile fc) = pure (ParseFile fc)

cmdParseFile ::
  (MonadGen gen, MonadIO m) => EdnaContext -> Command gen m EdnaState
cmdParseFile ctx =
  let
    -- We can always parse a file, state doesn't matter.
    gen _state = Just $ ParseFile <$> genFileContents
    execute (ParseFile fileContents) =
      runRIO ctx $ parseFile' fileContents

    noStateChanges :: Ensure ParseFile FileSummary
    noStateChanges oldState newState _ _  = oldState === newState

  in Command gen execute
  [ Ensure noStateChanges
  , Ensure correctFileSummary
  ]

correctFileSummary :: Ensure ParseFile FileSummary
correctFileSummary oldState _ (ParseFile fileContents) fileSummary =
  fileSummary === runReader expectedFileSummary oldState
  where
    expectedFileSummary :: EdnaReader FileSummary
    expectedFileSummary =
      fmap (FileSummary . toList) . foldM step mempty . toPairs $
      fcMeasurements fileContents

    targetNameToId name = view (esTargetByName . at name)
    compoundNameToId name = view (esCompoundByName . at name)

    step ::
      HashMap Text FileSummaryItem -> (Text, TargetMeasurements) ->
      EdnaReader (HashMap Text FileSummaryItem)
    step acc (targetName, TargetMeasurements targetMeasurements) = do
      target <- maybeToLeft targetName <$> targetNameToId targetName
      compounds <- forM (keys targetMeasurements) $ \compoundName ->
        maybeToLeft compoundName <$> compoundNameToId compoundName
      return $ acc & at targetName ?~ FileSummaryItem
        { fsiTarget = target
        , fsiCompounds = compounds
        }
