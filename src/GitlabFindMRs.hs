#!/usr/bin/env stack
-- stack --resolver lts-18.28 --install-ghc runghc --package req --package aeson --package aeson-casing --package regex-tdfa --package universum --package time --package optparse-applicative --package http-client --package http-types --package async
{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}

-- | Finds all merged Merge Requests in a project that are linked to an issue with the given label.
--
-- Usage:
--   Find all merged MRs in the `morley` project that mention an issue with the label `test-lib`
--   and were last updated this month.
--     stack GitlabFindMRs.hs -p morley-framework/morley --label test-lib
--
--   Find all merged MRs in the `morley` project that mention an issue with the label `test-lib`
--   and were last updated on or after August 2020.
--     stack GitlabFindMRs.hs -p morley-framework/morley --label test-lib --month 8 --year 2020
module GitlabFindMRs where

import           Control.Concurrent.Async  (forConcurrently)
import           Data.Aeson                (FromJSON, ToJSON)
import           Data.Aeson.Casing         (aesonPrefix, snakeCase)
import           Data.Aeson.TH             (deriveFromJSON)
import           Data.List                 (delete, nub)
import           Data.Time                 (Day, UTCTime, fromGregorian,
                                            getCurrentTime, toGregorian,
                                            utctDay)
import qualified Network.HTTP.Client       as C
import           Network.HTTP.Req
import qualified Network.HTTP.Types.Status as C
import           Options.Applicative       as Opt
import           Text.Regex.TDFA           ((=~))
import           Universum
import qualified Universum.Unsafe          as Unsafe

mkBaseUrl :: Options -> Url 'Https
mkBaseUrl opts = https "gitlab.com" /: "api" /: "v4" /: "projects" /: oProjectId opts

data Options = Options
  { oProjectId :: Text
  , oLabels    :: [Text]
  , oStartDate :: Maybe Day
  }
  deriving Show

newtype MergeRequestId = MergeRequestId Integer
  deriving newtype (Show, Eq, ToJSON, FromJSON)

newtype IssueId = IssueId Integer
  deriving newtype (Show, Eq, ToJSON, FromJSON, Read)

data Author = Author
  { aName :: Text
  }
  deriving Show

deriveFromJSON (aesonPrefix snakeCase) 'Author

data MergeRequest = MergeRequest
  { mrId          :: Integer
  , mrIid         :: MergeRequestId
  , mrTitle       :: String
  , mrDescription :: String
  , mrWebUrl      :: Text
  , mrAuthor      :: Author
  , mrUpdatedAt   :: UTCTime
  }
  deriving Show

deriveFromJSON (aesonPrefix snakeCase) 'MergeRequest

data Issue = Issue
  { iIid    :: IssueId
  , iTitle  :: Text
  , iLabels :: [Text]
  , iWebUrl :: Text
  }
  deriving Show

deriveFromJSON (aesonPrefix snakeCase) 'Issue

main :: IO ()
main = do
  opts <- execParser optsInfo

  let baseUrl = mkBaseUrl opts
  updatedAfter <- mkUpdatedAfter opts
  mrs <- findMergeRequests baseUrl updatedAfter

  putTextLn $ "Found " <> show (length mrs) <> " MRs updated since " <> show updatedAfter <> ".\n"

  matches <- fmap catMaybes . forConcurrently mrs $ \mr -> do
    let issueIds = extractMentions mr
    issues <- forConcurrently issueIds (getIssue opts)
    let issuesWithLabel =
          flip filter (catMaybes issues) \i ->
            flip any (oLabels opts) \l ->
              l `elem` iLabels i
    if null issuesWithLabel
      then pure Nothing
      else pure (Just (mr, issuesWithLabel))

  putTextLn $ "Found " <> show (length matches) <> " matches.\n"

  forM_ matches $ \(mr, issues) -> printMatch mr issues


optsParser :: Parser Options
optsParser = Options
  <$> option str
        ( long "project-id"
        <> short 'p'
        <> metavar "PROJECTID"
        <> help "The ID of the project in which to search for merge requests." )
  <*> some (option str
        ( long "label"
        <> short 'l'
        <> metavar "LABEL"
        <> help "The label by which to filter issues." ))
  <*> optional (option auto
        ( long "start"
        <> short 's'
        <> metavar "DATE"
        <> help "Defaults to the start of the current month" ))

optsInfo :: ParserInfo Options
optsInfo = info (optsParser <**> helper)
  ( fullDesc
  <> progDesc "Find all merged Merge Requests in a project that are linked to issues with the given label."
  )

printMatch :: MonadIO m => MergeRequest -> [Issue] -> m ()
printMatch mr issues = do
  putTextLn $ "Match: !" <> show (mrIid mr)
  putTextLn $ "    Title      : " <> toText (mrTitle mr)
  putTextLn $ "    Link       : " <> mrWebUrl mr
  putTextLn $ "    Author     : " <> aName (mrAuthor mr)
  putTextLn $ "    Updated At : " <> show (utctDay (mrUpdatedAt mr))
  putTextLn $ "    Mentions"
  forM_ issues $ \issue -> do
    putTextLn $ "        #" <> show (iIid issue)
    putTextLn $ "          Title : " <> iTitle issue
    putTextLn $ "          Link  : " <> iWebUrl issue
    putTextLn $ "          Labels: " <> show (iLabels issue)
  putTextLn ""

getIssue :: Options -> IssueId -> IO (Maybe Issue)
getIssue opts (IssueId id) =
  runHandle404 $ responseBody <$> req
    GET
    (mkBaseUrl opts /: "issues" /~ id)
    NoReqBody
    jsonResponse
    mempty

findMergeRequests :: Url 'Https -> Day -> IO [MergeRequest]
findMergeRequests baseUrl updatedAfter =
  runReq defaultHttpConfig $ paginate $ \page ->
    req
      GET
      (baseUrl /: "merge_requests")
      NoReqBody
      jsonResponse
      ( "updated_after" =: updatedAfter
      <> "state" =: ("merged" :: Text)
      <> "per_page" =: (100 :: Int)
      <> "page" =: page
      <> "order_by" =: ("updated_at" :: Text)
      <> "sort" =: ("desc" :: Text)
      )

mkUpdatedAfter :: MonadIO m => Options -> m Day
mkUpdatedAfter opts = do
  case oStartDate opts of
    Nothing -> do
      (currentYear, currentMonth, _) <- toGregorian . utctDay <$> liftIO getCurrentTime
      pure $ fromGregorian currentYear currentMonth 1
    Just d -> pure d

paginate :: forall a. (Semigroup a, FromJSON a) => (Int -> Req (JsonResponse a)) -> Req a
paginate mkRequest =
  go 1
  where
    go :: Int -> Req a
    go page = do
      rsp <- mkRequest page
      let x = responseBody rsp

      case responseHeader rsp "X-Next-Page" of
        Just nextPageStr | not (null nextPageStr) -> do
          let nextPage = Unsafe.fromJust (readMaybe (decodeUtf8 nextPageStr))
          xs <- go nextPage
          pure $ x <> xs
        _ -> pure x

extractMentions :: MergeRequest -> [IssueId]
extractMentions mr =
  -- Most MRs use a template that contains a markdown comment which mentions a fake issue #999.
  delete (IssueId 999) $
  nub $
    extract (mrTitle mr) <> extract (mrDescription mr)

  where
    extract :: String -> [IssueId]
    extract s =
      s =~ regex <&> \case
        [_, captured] -> Unsafe.fromJust $ readMaybe captured

    regex = "#([0-9]+)" :: String


newtype Handle404 a = Handle404 (MaybeT Req a)
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance MonadHttp Handle404 where
  handleHttpException (VanillaHttpException (C.HttpExceptionRequest _ (C.StatusCodeException rsp _)))
    | C.responseStatus rsp == C.status404 = Handle404 $ MaybeT $ pure Nothing
  handleHttpException ex = Handle404 $ lift (handleHttpException @Req ex)

runHandle404 :: MonadIO m => Handle404 a -> m (Maybe a)
runHandle404 (Handle404 m) = runReq defaultHttpConfig $ runMaybeT m
