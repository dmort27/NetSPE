module Main where

import Data.Phonology.Representations
import Data.Phonology.Rules
import Data.Phonology.RuleParsers

import Data.List (transpose)
import Data.Maybe (fromMaybe)

import Codec.Binary.UTF8.String (encodeString, decodeString)
import Network.CGI

toRepresentations :: RuleState -> String -> [[Segment]]
toRepresentations st = map (readIPA st) . lines

maybeDerivationV' :: RuleState -> String -> String -> [[Maybe String]]
maybeDerivationV' st ruleText = transpose . 
                                map (flip (maybeDerivationV (readRuleV st)) (lines ruleText)) . 
                                toRepresentations st

formatTable :: [[Maybe String]] -> String
formatTable = ("<table>"++) . (++"</table>") . 
              concatMap (("<tr>"++) . (++"</tr>") . 
                         concatMap (("<td>"++) . 
                                    (++"</td>") . 
                                    fromMaybe "---" ))

cgiMain :: CGI CGIResult
cgiMain = setHeader "Content-type" "text/html; charset=UTF-8" >> 
          fmap (decodeString . fromMaybe "") (getInput "ruletext") >>= 
          \rs -> (getInput "reptext" >>= 
                           output . encodeString . formatTable . 
                           maybeDerivationV' defState rs . decodeString . fromMaybe "<p class='error'>Error in input.</p>")

main :: IO ()
main = runCGI (handleErrors cgiMain)