module SOM.Options (Options (..), outputOrExtension, parser) where

import SOM.Prelude

import Control.Applicative (optional)

import Data.Maybe (fromMaybe)

import Options.Applicative
  ( Parser
  , argument
  , execParser
  , fullDesc
  , info
  , long
  , metavar
  , short
  , str
  , strOption
  )

import System.FilePath (replaceExtension)

data Options = Options { input ∷ FilePath
                       , output ∷ Maybe FilePath
                       }

parser ∷ Parser Options
parser = Options <$> input <*> output
  where input = argument str (metavar "[INPUT FILE]")
        output = optional (strOption (metavar "[OUTPUT FILE]" <> short 'o' <> long "output"))

outputOrExtension ∷ String → Options → FilePath
outputOrExtension e o = fromMaybe (replaceExtension o.input e) o.output
