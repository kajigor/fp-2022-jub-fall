module Test.Reductions_property where

import Reductions
import Lambda
import Data.Maybe

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

