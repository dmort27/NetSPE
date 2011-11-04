module Main where

import Data.Maybe (fromMaybe)
import Data.List (intercalate)

import Data.Phonology.Representations
import Data.Phonology.Rules
import Data.Phonology.RuleParsers

import Codec.Binary.UTF8.String (encodeString, decodeString)
import Network.CGI

testRules :: String -> [(String, Bool)]
testRules str = zip (lines str) (testRules' str)

testRules' :: String -> [Bool]
testRules' = map (all (==True) . testRuleV defState) . lines

boolToJSON :: Bool -> String
boolToJSON True = "true"
boolToJSON False = "false"

testRep :: String -> Bool
testRep = readableIPA defState

testReps :: [String] -> [Bool]
testReps = map (readableIPA defState)

nonNullReps :: String -> [String]
nonNullReps = filter (/="") . lines

repTuples :: String -> [(String, Bool)]
repTuples reptext = zip (nonNullReps reptext) (testReps $ nonNullReps reptext)

tupleToJSON :: (String, Bool) -> String
tupleToJSON (str, bool) = 
    "{\"item\": \"" ++ str ++ "\", \"valid\": " ++ (boolToJSON bool) ++ "}" 

enquote :: String -> String
enquote = (++"\"") . ("\""++)

tuplesToJSON :: String -> [(String, Bool)] -> String
tuplesToJSON name =  (((enquote name) ++ ":") ++) . ("["++) . (++"]") 
                     . intercalate "," . map tupleToJSON

cgiMain :: CGI CGIResult
cgiMain = do
  setHeader "Content-type" "application/json; charset=UTF-8"
  rs <- fmap (tuplesToJSON "rules" . testRules . decodeString . fromMaybe "") 
        (getInput "ruletext")
  urs <- fmap (tuplesToJSON "urs". repTuples . decodeString . fromMaybe "")
         (getInput "reptext")
  srs <- fmap (tuplesToJSON "srs". repTuples . decodeString . fromMaybe "")
         (getInput "sreptext")
  output $ encodeString $ ("{"++) $ (++"}") $ intercalate ", " [rs, urs, srs]

main :: IO ()
main = runCGI (handleErrors cgiMain)
