module HuffmanTests where

import Huffman

import Test.Framework
import Test.HUnit

testWeight1 = 2 ~=? weight (Leaf 'c' 2)
testWeight2 = 5 ~=? weight (Fork (Leaf 'c' 2) (Leaf 'd' 3) ['c', 'd'] 5)

testWeights = TestList ["testWeight1" ~: testWeight1, "testWeight2" ~: testWeight2]

testChars1 = "c"  ~=? chars (Leaf 'c' 2)
testChars2 = "cd" ~=? chars (Fork (Leaf 'c' 2) (Leaf 'd' 3) ['c', 'd'] 5)

testChars  = TestList ["testChars1" ~: testChars1, "testChars2" ~: testChars2]

testMakeCodeTree1 = Fork (Leaf 'c' 1) (Leaf 'd' 2) "cd" 3
                    ~=?
                    makeCodeTree (Leaf 'c' 1) (Leaf 'd' 2)
testMakeCodeTree2 = Fork (Fork (Leaf 'c' 1) (Leaf 'd' 2) "cd" 3) (Leaf 'd' 2) "cdd" 5
                    ~=?
                    makeCodeTree (Fork (Leaf 'c' 1) (Leaf 'd' 2) "cd" 3) (Leaf 'd' 2)

testMakeCodeTrees = TestList ["testMakeCodeTree1" ~: testMakeCodeTree1, "testMakeCodeTree2" ~: testMakeCodeTree2]

testTimes1 = [('1',4),('2',1),('3',1),('4',1)] ~=? times ['1','3','4','1','1','1','2']
testTimes2 = [('a',4),('b',2),('d',1)] ~=? times ['a','b','b','a','a','a','d']

testTimess = TestList ["testTimes1" ~: testTimes1, "testTimes2" ~: testTimes2]

-- Full tests
tests = TestList [testWeights, testChars, testMakeCodeTrees, testTimess]

-- *HuffmanTests> runTestTT tests
-- Cases: 8  Tried: 8  Errors: 0  Failures: 0
-- Counts {cases = 8, tried = 8, errors = 0, failures = 0}