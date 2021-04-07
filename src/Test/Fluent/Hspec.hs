{-# LANGUAGE ScopedTypeVariables #-}

module Test.Fluent.Hspec
  ( -- ** Assertion util functions for HSpec
    assertThat,
    assertThat',
    assertThatIO,
    assertThatIO',
    assertThrown,
    assertThrown',
    assertThrows,
    assertThrows',
  )
where

import Control.Exception (Exception, throwIO, try)
import Data.List (intercalate)
import GHC.Exception (prettySrcLoc)
import GHC.Stack (HasCallStack)
import Test.Fluent.Assertions (Assertion', AssertionConfig, FluentTestFailure)
import qualified Test.Fluent.Assertions.Core as FC
import Test.Fluent.Assertions.Exceptions (ExceptionSelector)
import Test.Fluent.Internal.Assertions (FluentTestFailure (FluentTestFailure))
import Test.HUnit.Lang
  ( FailureReason (Reason),
    HUnitFailure (HUnitFailure),
  )

-- |
-- Module      : Test.Fluent.Assertions.Core
-- Description : Set util function to execute assertions against given value. Those functions integrates with HSpec.
-- Copyright   : (c) Pawel Nosal, 2021
-- License     : MIT
-- Maintainer  : p.nosal1986@gmail.com
-- Stability   : experimental

-- | Verify if given `IO` action throws expected exception.
assertThrows :: (HasCallStack, Exception e) => IO a -> ExceptionSelector e -> IO ()
assertThrows given selector = toHspecError $ FC.assertThrows given selector

assertThrows' :: (HasCallStack, Exception e) => AssertionConfig -> IO a -> ExceptionSelector e -> IO ()
assertThrows' config given selector = toHspecError $ FC.assertThrows' config given selector

assertThrown' :: (HasCallStack, Exception e) => AssertionConfig -> IO a -> ExceptionSelector e -> Assertion' e b -> IO ()
assertThrown' config given selector assertions = toHspecError $ FC.assertThrown' config given selector assertions

-- | Execute assertions against selected exception
assertThrown :: (HasCallStack, Exception e) => IO a -> ExceptionSelector e -> Assertion' e b -> IO ()
assertThrown given selector assertions = toHspecError $ FC.assertThrown given selector assertions

-- | Execute assertions against given subject under test extracted from IO action.
assertThatIO :: HasCallStack => IO a -> Assertion' a b -> IO ()
assertThatIO given assertions = toHspecError $ FC.assertThatIO given assertions

-- | A variant of `assertThatIO` which allow to pass additional configuration.
assertThatIO' :: HasCallStack => AssertionConfig -> IO a -> Assertion' a b -> IO ()
assertThatIO' config given assertions = toHspecError $ FC.assertThatIO' config given assertions

-- | A variant of `assertThat` which allow to pass additional configuration.
assertThat' :: HasCallStack => AssertionConfig -> a -> Assertion' a b -> IO ()
assertThat' config given assertions = toHspecError $ FC.assertThat' config given assertions

-- | Execute assertions against given subject under test.
assertThat :: HasCallStack => a -> Assertion' a b -> IO ()
assertThat given assertions = toHspecError $ FC.assertThat given assertions

toHspecError :: HasCallStack => IO () -> IO ()
toHspecError a = do
  res :: Either FluentTestFailure () <- try a
  case res of
    Left (FluentTestFailure srcLoc msg _ _) -> do
      let assertionMessages = intercalate "\n" (fmap formatMsg msg)
      throwIO $ HUnitFailure srcLoc (Reason assertionMessages)
    Right () -> pure ()
  where
    formatMsg (message, Just srcLoc) = prettySrcLoc srcLoc <> "\n" <> message
    formatMsg (message, Nothing) = message