{-# LANGUAGE ScopedTypeVariables #-}

module Test.Fluent.Hspec
  ( assertThat,
  )
where

import Control.Exception (throwIO, try)
import Data.List (intercalate)
import GHC.Exception (prettySrcLoc)
import GHC.Stack (HasCallStack)
import Test.Fluent.Assertions (Assertion', FluentTestFailure)
import qualified Test.Fluent.Internal.Assertions as FA
import Test.HUnit.Lang
  ( FailureReason (Reason),
    HUnitFailure (HUnitFailure),
  )

assertThat :: HasCallStack => a -> Assertion' a b -> IO ()
assertThat a assertions = do
  res :: Either FluentTestFailure () <- try $ FA.assertThat a assertions
  case res of
    Left (FA.FluentTestFailure srcLoc msg _ _) -> do
      let assertionMessages = intercalate "\n" (fmap formatMsg msg)
      throwIO $ HUnitFailure srcLoc (Reason assertionMessages)
    Right () -> pure ()
  where
    formatMsg (message, Just srcLoc) = prettySrcLoc srcLoc <> "\n" <> message
    formatMsg (message, Nothing) = message