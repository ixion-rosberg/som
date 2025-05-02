module Main where

import SOM.Prelude

import SOM.Binary.Piece (Model (..), Vertex (..))
import SOM.GlTF (GlTF (..), Mesh (..), access, loadBufferData, parse)

import Codec.GlTF (fromFile)

import Control.Applicative (optional)

import Data.Binary (encodeFile)
import Data.Binary.Extra (IEEE (..), UnsignedShort (..))
import Data.Either.Extra (mapLeft)
import Data.Maybe qualified as M (fromMaybe)

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

import UnliftIO.Exception
  ( Handler (..)
  , IOException
  , StringException (..)
  , catches
  , fromEither
  , stringException
  )

data Options = Options { input ∷ String, output ∷ Maybe String }

parser ∷ Parser Options
parser = Options <$> input <*> output
  where input = argument str (metavar "[INPUT FILE]")
        output = optional (strOption (metavar "[OUTPUT FILE]" <> short 'o' <> long "output"))

main ∷ IO ()
main = (flip catches) handlers do
      o ← execParser $ info parser fullDesc

      g ← (lift ∘ parse ↢ lift ↢ fromFile) o.input

      b ← loadBufferData g

      ps ← lift $ access b g.mesh.position
      ns ← lift $ access b g.mesh.normal
      ts ← lift $ access b g.mesh.texCoord
      is ← lift $ access b g.mesh.indices

      let vs = zipWith3 vertex ps ns ts
          is' = (.unUnsignedShort) <$> is

      encodeFile (output o) (Model vs is')

      pure ()

  where
    handlers = [ Handler (\ (StringException e _) → putStrLn e)
               , Handler (\ (e ∷ IOException) → print e)
               ]
    lift = fromEither ∘ mapLeft stringException
    vertex p n t = Vertex ((.unIEEE) <$> p) ((.unIEEE) <$> n) ((.unIEEE) <$> t)
    output o = M.fromMaybe (replaceExtension o.input "msm") o.output
