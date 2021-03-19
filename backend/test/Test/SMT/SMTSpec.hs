-- | State machine testing for Edna.

module Test.SMT.SMTSpec
  ( spec
  ) where

import Universum

import qualified Data.ByteString.Lazy as BSL

import Hedgehog
  (Callback(..), Command(..), HTraversable(..), MonadGen, MonadTest, Var, annotate, assert,
  executeSequential, failure, forAll, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lens.Micro.Platform (at, (.=), (?~))
import RIO (runRIO)
import Test.Hspec (Spec, context, it)
import Test.Hspec.Hedgehog (modifyMaxSuccess)

import Edna.ExperimentReader.Types (FileContents(..), TargetMeasurements(..))
import Edna.Orphans ()
import Edna.Setup (EdnaContext)
import Edna.Upload.Service (UploadError(..), parseFile', uploadFile')
import Edna.Web.Types (FileSummary(..), FileSummaryItem(..), Project, SqlId(..), TestMethodology)

import Test.Gen
import Test.SMT.State
import Test.Setup (ednaTestMode, withContext)

spec :: Spec
spec = context "State machine tests" $ withContext $ modifyMaxSuccess (min 20) $
  it "Edna implementation matches Edna model" $ \ctx -> ednaTestMode ctx $ do
    actions <- forAll $
      Gen.sequential (Range.linear 1 100) initialState
        [ cmdParseFile ctx
        , cmdUploadFile ctx
        ]
    executeSequential initialState actions

----------------
-- ParseFile
----------------

newtype ParseFile (v :: Type -> Type) = ParseFile FileContents
  deriving stock (Show, Eq)

instance HTraversable ParseFile where
  htraverse _ (ParseFile fc) = pure (ParseFile fc)

cmdParseFile ::
  (MonadGen gen, MonadIO m) => EdnaContext -> Command gen m EdnaState
cmdParseFile ctx =
  let
    -- We can always parse a file, state doesn't matter.
    gen _state = Just $ ParseFile <$> genFileContents genName genName
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

----------------
-- UploadFile
----------------

data UploadFile (v :: Type -> Type) = UploadFile
  { ufProject :: SqlId Project
  , ufTestMethodology :: SqlId TestMethodology
  , ufDescription :: Text
  , ufFileName :: Text
  , ufBytes :: LByteString
  , ufFileContents :: FileContents
  }
  deriving stock (Show, Eq)

instance HTraversable UploadFile where
  htraverse _ (UploadFile p tm d fn b fc) = pure (UploadFile p tm d fn b fc)

-- If the list is not empty, generate one of its elements with high probability.
-- If the list is empty or unlikely scenario happens, the provided generator
-- will be used instead.
genOneOfLikely :: MonadGen m => m a -> [a] -> m a
genOneOfLikely genA as
  | null as = genA
  | otherwise = Gen.frequency [(1, genA), (10, Gen.element as)]

cmdUploadFile ::
  (MonadGen gen, MonadIO m) => EdnaContext -> Command gen m EdnaState
cmdUploadFile ctx =
  let
    gen EdnaState {..} = Just do
      -- It's not allowed to add a file not referring to an existing project and
      -- test methodology.
      -- We still generate such cases to check that an error happens, but
      -- we try to generate valid cases because they are more interesting and
      -- useful.
      ufProject <- genOneOfLikely genSqlId $ keys _esProjects
      ufTestMethodology <- genOneOfLikely genSqlId $ keys _esTestMethodologies
      ufDescription <- genDescription
      ufFileName <- genName
      -- Generate empty bytes for now because generating proper bytestring
      -- with xlsx contents is complicated and not worth it.
      -- Probably later we will generate just a random string here and
      -- will only check that exactly this bytestring is returned.
      let ufBytes = BSL.empty
      let genTargetName = genOneOfLikely genName $ keys _esTargetByName
      let genCompoundName = genOneOfLikely genName $ keys _esCompoundByName
      ufFileContents <- genFileContents genTargetName genCompoundName
      return UploadFile {..}
    execute UploadFile {..} =
      liftIO $ try @_ @UploadError $
      runRIO ctx $ uploadFile'
        ufProject ufTestMethodology ufDescription ufFileName ufBytes ufFileContents

  in Command gen execute
  [ Update applyUploadFile
  -- We expect the same file summary as in @ParseFile@.
  , Ensure $ \oldState newState uploadFile ->
    either (const pass) $
    correctFileSummary oldState newState (ParseFile $ ufFileContents uploadFile)
  , Ensure failsOnUnknown
  ]

applyUploadFile ::
  EdnaState v -> UploadFile v -> Var output v -> EdnaState v
applyUploadFile es UploadFile {..} _ = executingState es $ runMaybeT $ do
  projectState <- MaybeT $ use (esProjects . at ufProject)
  let newProjectState =
        projectState & psFiles %~ ((ufFileContents, ufTestMethodology):)
  esProjects . at ufProject .= Just newProjectState
  let
    insertToBothMaps ::
      Lens' (EdnaState v) (HashMap (SqlId x) Text) ->
      Lens' (EdnaState v) (HashMap Text (SqlId x)) ->
      Text -> State (EdnaState v) ()
    insertToBothMaps mapToName mapFromName name =
      whenNothingM_ (use $ mapFromName . at name) $ do
        newId <- SqlId . fromIntegral . length <$> use mapFromName
        mapFromName . at name .= Just newId
        mapToName . at newId .= Just name
  lift $ forM_ (toPairs $ fcMeasurements ufFileContents) $
    \(targetName, TargetMeasurements targetMeasurements) -> do
      insertToBothMaps esTargetToName esTargetByName targetName
      mapM_ (insertToBothMaps esCompoundToName esCompoundByName) $
        keys targetMeasurements

-- Upload fails if one tries to pass an unknown project or test methodology.
failsOnUnknown :: Ensure UploadFile (Either UploadError FileSummary)
failsOnUnknown oldState _ uploadFile res = do
  let proj = ufProject uploadFile
  let method = ufTestMethodology uploadFile
  let expectErr expectedErr = case res of
        Right _ -> failedTest "unexpected success"
        Left err -> expectedErr === err
  case (oldState ^. esProjects . at proj, oldState ^. esTestMethodologies . at method) of
    (Just _, Nothing) -> expectErr (UEUnknownTestMethodology method)
    (Nothing, Just _) -> expectErr (UEUnknownProject proj)
    (Nothing, Nothing) ->
      assert $ res == Left (UEUnknownTestMethodology method)
        || res == Left (UEUnknownProject proj)
    (Just _, Just _) -> pass

-- | A 'Property' that always fails with given message.
failedTest :: (HasCallStack, MonadTest m) => Text -> m ()
failedTest r = withFrozenCallStack $ annotate (toString r) >> failure
