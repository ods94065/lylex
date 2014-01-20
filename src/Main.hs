{-# LANGUAGE OverloadedStrings #-}
module Main (main) where
import Control.Monad (unless, when)
import Data.Aeson ((.=), encode, object, ToJSON(..))
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BS8
import System.Environment (getArgs)
import System.Directory (doesFileExist)

import Music.LilyLexer

-- JSON output utilities
instance ToJSON Token where
  toJSON (Token _ EOF _) = object ["type" .= EOF]
  toJSON (Token pos typ mstr) = object alist
    where
      alist = ["type" .= typ, "position" .= pos] ++ (
        case mstr of
          Nothing -> []
          Just str -> ["text" .= str])

instance ToJSON TokenType where
  toJSON = toJSON . T.pack . show

instance ToJSON AlexPosn where
  toJSON (AlexPn pos line col) = object [
    "bytes" .= pos, "line" .= line, "column" .= col]

-- The main routine

-- | Read a file and scan it. If successful, the list of tokens are converted to JSON and printed on
-- stdout.
main :: IO ()
main = do
  fileList <- getArgs
  let filename = head fileList
  fileExists <- doesFileExist filename
  unless fileExists (error ("The following file does not exist: " ++ filename))
  s <- readFile filename
  case scanner s of
    Left msg -> error msg
    Right toks -> (BS8.putStrLn . encode) toks
