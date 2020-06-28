module TestCases(performTests) where

import Test.HUnit
import SolidShape
import VividShape
import World
import Objects

--RUNNING ALL TEST CASES---------------------------------------------------------------------------------------------------------------------------------------------
performTests = runTestTT (TestList [solidShapeTest, objectTest, testAdjustForVividShape])


