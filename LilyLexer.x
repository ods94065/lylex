{
{-# LANGUAGE OverloadedStrings #-}
module LilyLexer (main) where
import Control.Monad (unless, when)
import Data.Aeson ((.=), encode, object, ToJSON(..))
import Data.Maybe (fromJust, isJust)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS8
import qualified Data.Text as T
import System.Environment (getArgs)
import System.Directory (doesFileExist)
}

%wrapper "monadUserState"

$alpha = [a-zA-Z\x80-\xFF]
$digit = [0-9]

@identifier = $alpha ([\-_] $alpha | $alpha)*
-- note: the utf-8 bom sequence has been converted to a Unicode code point
-- by the time the lexer gets its hands on it.
@utf8bom = \xFEFF

state:-
<0> @utf8bom { handleBom }
<0> @identifier { mkT Ident }
<0> "%{" { beginComment }
<0> \" { beginStringLiteral }
<0> $white+ ;
<0> . { \_ _ -> alexError "Illegal character" }
<state_comment> "%}" { endComment }
<state_comment> (~\%)+ ;
<state_comment> \% ;
<state_string> \" { endStringLiteral }
<state_string> [^\\\"]+ { pushStringLiteralCurr }
<state_string> \\\\ { pushStringLiteralChar '\\' }
<state_string> \\\" { pushStringLiteralChar '"' }
<state_string> \\\' { pushStringLiteralChar '\'' }
<state_string> \\n { pushStringLiteralChar '\n' }
<state_string> \\t { pushStringLiteralChar '\t' }

{
-- Utilities

-- | Logs a message to stdout during an Alex sequence. For use during debugging only.
alexLog :: String -> Alex ()
alexLog msg = Alex $ \s -> (return . unsafePerformIO) (putStrLn msg >> return (s, ()))

-- | Returns a simple (line:col) description of the given position.
showPosn :: AlexPosn -> String
showPosn (AlexPn _ line col) = show line ++ ":" ++ show col

-- | Extracts the current position from the lexer.
getPosn :: Alex AlexPosn
getPosn = Alex $ \s@(AlexState{alex_pos=pos}) -> Right (s, pos)

-- | Extracts the current user state from the lexer.
getUserState :: Alex AlexUserState
getUserState = Alex $ \s -> Right (s, alex_ust s)

-- | Sets a new user state.
setUserState :: AlexUserState -> Alex ()
setUserState us = Alex $ \s -> Right (s{alex_ust=us}, ())

-- | Mutates the user state with the given state transformation function.
modifyUserState :: (AlexUserState -> AlexUserState) -> Alex ()
modifyUserState f = getUserState >>= (setUserState . f)

-- Interface with lexer
-- | The different kinds of tokens produced by our lexer.
data TokenType = Ident | StringLit | EOF deriving (Show, Eq)

-- | The lexer token.
data Token = Token {
  tok_pos :: AlexPosn, -- ^ The position in the stream where this token started or was triggered.
                       -- For EOF, this will be undefined.
  tok_type :: TokenType, -- ^ The type of token.
  tok_string :: Maybe String -- ^ The text associated with the token, or Nothing if this is an
                             -- abstract token.
  }

instance Show Token where
  show (Token _ EOF _) = "#<EOF>"
  show (Token pos typ mbs) = "#<" ++ show typ ++ " " ++ showPosn pos ++ ">"

instance Eq Token where
  t1 == t2 = if tok_type t1 == EOF 
             then (tok_type t2 == EOF)
             else (tok_type t1 == tok_type t2
                   && tok_pos t1 == tok_pos t2
                   && tok_string t1 == tok_string t2)

alexEOF :: Alex Token
alexEOF = return $ Token undefined EOF Nothing

data AlexUserState = LilyLexerState {
  pending_literal_pos :: AlexPosn, -- ^ If we are parsing a string literal, this is where it began.
  pending_literal :: String -- ^ If we are parsing a string literal, this is a stack of characters
                            -- processed thus far.
  }
alexInitUserState = LilyLexerState alexStartPos ""

-- Internal action helpers

-- | Creates an action that returns a token with the given token type.
mkT :: TokenType -> AlexAction Token
mkT t (p, _, _, s) n = return $ Token p t (Just (take n s))

handleBom :: AlexAction Token
handleBom inp@((AlexPn pos _ _), _, _, _) len = 
  if pos > 0 
  then alexError "Stray UTF-8 BOM" 
  else skip inp len

beginComment :: AlexAction Token
beginComment inp len = (skip `andBegin` state_comment) inp len

endComment :: AlexAction Token
endComment inp len = do
  -- reset to initial lexer state
  alexSetStartCode 0
  skip inp len

beginStringLiteral :: AlexAction Token
beginStringLiteral inp@(pos, _, _, _) len = 
  modifyUserState setPendingLiteralPos >> alexSetStartCode state_string >> skip inp len
  where
    setPendingLiteralPos us = us{pending_literal_pos=pos}

pushStringLiteralChar :: Char -> AlexAction Token
pushStringLiteralChar c inp len = modifyUserState _pushStringLiteralChar >> skip inp len
  where
    _pushStringLiteralChar us@LilyLexerState{pending_literal=cs} = us{pending_literal=(c:cs)}    

pushStringLiteralCurr :: AlexAction Token
pushStringLiteralCurr inp@(_, _, _, s) len =
  modifyUserState (pushStringLiteral charsToPush) >> skip inp len
  where
    pushStringLiteral s us@LilyLexerState{pending_literal=cs} = us{pending_literal=(s ++ cs)} 
    charsToPush = (reverse . take len) s

endStringLiteral :: AlexAction Token
endStringLiteral inp len =
  popPendingLiteral >>= \(pos, s) -> alexSetStartCode 0 >> returnStringLit pos s
  where
    popPendingLiteral :: Alex (AlexPosn, String)
    popPendingLiteral = do
      us@LilyLexerState{pending_literal_pos=pos, pending_literal=cs} <- getUserState
      setUserState us{pending_literal_pos=alexStartPos, pending_literal=""}
      return (pos, reverse cs)

    returnStringLit :: AlexPosn -> String -> Alex Token
    returnStringLit pos s = return $ Token pos StringLit (Just s)

-- Lexer interface and utilities

-- | Scans a string and returns either an error message or the list of tokens produced by the string.
scanner :: String -> Either String [Token]
scanner str = runAlex str scannerLoop
  where
    scannerLoop = handleError lexerError alexMonadScan >>= handleToken
    handleToken tok = if tok_type tok == EOF 
                      then return []
                      else scannerLoop >>= \rest -> return (tok : rest)

-- | Wraps a lexer with an error handler. If the wrapped lexer fails, invoke the error handler
-- to obtain an error-handling lexer to run, and then run it.
--
-- The error-handling lexer has full access to the lexer state where the error occurred, and may
-- choose to propagate the error or absorb it and return some sort of success.
handleError :: (String -> Alex a) -> Alex a -> Alex a
handleError handler (Alex lex) = Alex $ \s -> case (lex s) of
  Left errMsg -> unAlex (handler errMsg) s
  Right (s', x) -> Right (s', x)

-- | Converts a brief lexer error message to a more detailed error message which includes location
-- information and line context.
lexerError :: String -> Alex a
lexerError msg =
  do (pos, c, _, inp) <- alexGetInput
     alexError $ getLexerErrMsg pos inp msg
  where
    getLexerErrMsg pos inp msg =
      let restOfLine = filter (/= '\r') (takeWhile (/= '\n') inp)
          msg' = if (null msg) then "Lexer error" else msg
      in msg' ++ " at " ++ showPosn pos ++ " " ++ showContext restOfLine
    showContext inp =
      if null inp 
      then "at end of file" 
      else "on char " ++ show (head inp) 
           ++ " before: '" ++ (trim . take 30 . tail) inp ++ "'"
    trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

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
  putStrLn $ "Beginning analysis of " ++ filename
  s <- readFile filename
  case scanner s of
    Left msg -> error msg
    Right toks -> (BS8.putStrLn . encode) toks
}
