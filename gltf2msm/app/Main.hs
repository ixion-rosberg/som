module Main where

import SOM.Prelude

import Codec.GlTF (GlTF (..), fromFile)
import Codec.GlTF.Buffer (Buffer (..))
import Codec.GlTF.URI (loadURI)

import Data.ByteString (ByteString, readFile)
import Data.Either.Extra (mapLeft)
import Data.Vector (Vector)

import Options.Applicative (Parser, argument, execParser, fullDesc, info, metavar, str)

import UnliftIO.Exception (Handler (..), IOException, StringException (..), catches, fromEither, stringException)
import UnliftIO.Exception.Extra (fromMaybe)

parser ∷ Parser FilePath
parser = argument str (metavar "FILE")

main ∷ IO ()
main = (flip catches) handlers do
      f ← execParser $ info parser fullDesc

      g ← fromEither ∘ mapLeft stringException =≪ fromFile f

      bs ← loadBuffers g

      pure ()

  where
    handlers = [ Handler (\ (StringException e _) → putStrLn e)
               , Handler (\ (e ∷ IOException) → print e)
               ]

loadBuffers ∷ GlTF → IO (Vector ByteString)
loadBuffers g = do
  bs ← fromMaybe (stringException "Missing buffers") g.buffers

  mapM loadBuffer bs
  where
    loadBuffer b = do
      u ← fromMaybe (stringException "Missing URI") b.uri

      x ← loadURI (fmap pure ∘ readFile) u

      fromEither ∘ mapLeft stringException $ x
