module Main where

import Data.Maybe (fromMaybe)
import Data.List (intercalate)

import Data.Phonology.Representations
import Data.Phonology.Rules
import Data.Phonology.RuleParsers

import Codec.Binary.UTF8.String (encodeString, decodeString)
import Network.CGI

testRules :: String -> [Bool]
testRules = map (all (==True) . testRuleV defState) . lines

testMessage :: [Bool] -> String
testMessage resp = if (all (==True) resp) 
                   then "All rules are syntactically correct." 
                   else "Rules " ++ badRules ++ " are syntactically incorrect."
    where badRules = intercalate ", " $ map (show . snd) $ filter (not . fst) $ zip resp [1..]

cgiMain :: CGI CGIResult
cgiMain = setHeader "Content-type" "text/plain; charset=UTF-8" >> 
          fmap (testMessage . testRules . decodeString . fromMaybe "Error") 
                   (getInput "ruleText") >>= output
          
main :: IO ()
main = runCGI (handleErrors cgiMain)