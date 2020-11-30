{-# LANGUAGE
    OverloadedStrings
  , QuasiQuotes
  , TupleSections
  , DeriveGeneric
  , DerivingStrategies
  , DeriveAnyClass
  , GeneralizedNewtypeDeriving
  , PackageImports
#-}

module Lib where

import GitHub

import System.Environment ( getEnv )

import Data.ByteString.Internal ( packChars )
import qualified Data.ByteString as BS

import Data.Aeson ( FromJSON )

import Data.Map ( Map )
import qualified Data.Map as M

import Data.Vector ( Vector )
import qualified Data.Vector as V

import Control.Monad.Except ( ExceptT(..), runExceptT )
import Control.Monad.Trans ( lift )

import Control.Arrow ( (&&&) )

import Control.Exception ( Exception, throwIO )

import Text.Pretty.Simple (pPrint)

message :: String
message = "Ready? Get set... GO!"

withErrorsInIO :: Exception e => ExceptT e IO a -> IO a
withErrorsInIO = (>>= either throwIO return) . runExceptT

entrypoint :: IO ()
entrypoint = withErrorsInIO $ do
  token <- fmap packChars $ lift $ getEnv "GITHUB_TOKEN"

  let 
    gh :: FromJSON x => Request 'RW x -> ExceptT Error IO x
    gh = ExceptT . github (OAuth token)

  -- Get all the pull requests
  prs <- gh $ pullRequestsForR "MercuryTechnologies" "mercury-web-backend" mempty 60

  -- For each pull request, get the files that it modifies
  fs <- fmap (fmap (fmap fileFilename)) $ traverse gh $ M.fromList $ fmap (simplePullRequestNumber &&& \pr -> pullRequestFilesR "MercuryTechnologies" "mercury-web-backend" (simplePullRequestNumber pr) 60) $ V.toList $ prs

  -- Check which PRs modify Foundation.hs
  let prsbad = M.filter (any (== "src/Foundation.hs")) fs
  -- lift $ pPrint fs
  lift $ pPrint prsbad
