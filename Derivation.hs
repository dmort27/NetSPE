module Main where

import Data.Phonology.Representations
import Data.Phonology.Rules
import Data.Phonology.RuleParsers

import Network.CGI

toExandedRules :: RuleState -> String -> [[Rule]]
toExandedRules st = map (readRuleV st) . lines

toRepresentations :: RuleState -> String -> [[Segment]]
toRepresentations st = map (readIPA st) . lines

