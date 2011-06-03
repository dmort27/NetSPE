module Main where


import Data.Phonology.Representations
import Data.Phonology.Rules
import Data.Phonology.RuleParsers

import Codec.Binary.UTF8.String (encodeString, decodeString)
import Network.CGI

cgiMain :: CGI CGIResult
cgiMain = setHeader "Content-type" "application/json; charset=UTF-8" >> 
          output "Done"
          
main :: IO ()
main = runCGI (handleErrors cgiMain)