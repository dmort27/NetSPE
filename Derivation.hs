module Main where

import Data.Phonology.Representations
import Data.Phonology.Rules
import Data.Phonology.RuleParsers

import Data.List (transpose)
import Data.Maybe (fromMaybe)

import Network.CGI

toRepresentations :: RuleState -> String -> [[Segment]]
toRepresentations st = map (readIPA st) . lines

maybeDerivationV' :: RuleState -> String -> String -> [[Maybe String]]
maybeDerivationV' st ruleText = transpose . 
                                map (flip (maybeDerivationV (readRuleV st)) (lines ruleText)) . 
                                toRepresentations st

formatTable :: [[Maybe String]] -> String
formatTable = ("<table>"++) . (++"</table") . 
              concatMap (("<tr>"++) . (++"</tr>") . 
                         concatMap (("<td>"++) . (++"</td>") . fromMaybe "---" ))

cgiMain :: CGI CGIResult
cgiMain = setHeader "Content-type" "text/html; charset=UTF-8" >> 
          getInput "ruletext" >>= 
          \(Just rs) -> (getInput "reptext" >>= 
                         output . formatTable . maybeDerivationV' defState rs . fromMaybe "")

main :: IO ()
main = runCGI (handleErrors cgiMain)