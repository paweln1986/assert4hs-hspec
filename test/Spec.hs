{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception (throwIO, try)
import Data.List (intercalate)
import GHC.Exception (SrcLoc, prettySrcLoc)
import Test.Fluent.Assertions
  ( Assertion',
    FluentTestFailure (FluentTestFailure),
    focus,
    isEqualTo,
  )
  
import Test.HUnit.Lang
  ( FailureReason (Reason),
    HUnitFailure (HUnitFailure),
  )
import Test.Hspec (HasCallStack, describe, hspec, it)

main :: IO ()
main = hspec $
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      assertThatHspec [23 ..] $ 
        focus head 
        . isEqualTo 21
        . isEqualTo 21
        . isEqualTo 21
        . isEqualTo 21
