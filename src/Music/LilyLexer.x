{
module Music.LilyLexer (
  scanner, scanExactlyOne, Token (..), TokenType (..), AlexPosn(..)) where
import System.IO.Unsafe (unsafePerformIO)

import Control.Monad (when)
import Data.Maybe (fromJust, isJust)
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
<0> \\ @identifier { mkT Command }
<0> "%{" { beginComment }
<0> \" { beginStringLiteral }
<0> $white+ { mkT Skip }
<0> . { \_ _ -> alexError "Illegal character" }
<state_comment> "%}" { \inp len -> alexLog "End of comment" >> endComment inp len }
<state_comment> (~\%)+ { mkT Skip }
<state_comment> \% { mkT Skip }
<state_string> \" { endStringLiteral }
<state_string> [^\\\"]+ { pushStringLiteralCurr }
<state_string> \\\\ { pushStringLiteralChar '\\' }
<state_string> \\\" { pushStringLiteralChar '"' }
<state_string> \\\' { pushStringLiteralChar '\'' }
<state_string> \\n { pushStringLiteralChar '\n' }
<state_string> \\t { pushStringLiteralChar '\t' }

{
-- Utilities

-- | Logs a message to stdout during an Alex sequence. For use during debugging sessions only.
alexLog :: String -> Alex ()
alexLog msg = Alex $ \s -> (return . unsafePerformIO) (putStrLn msg >> return (s, ()))

-- | Wraps a lexer with an error handler. If the wrapped lexer fails, invoke the error handler
-- to obtain an error-handling lexer to run, and then run it.
--
-- The error-handling lexer has full access to the lexer state where the error occurred, and may
-- choose to propagate the error or absorb it and return some sort of success.
handleError :: (String -> Alex a) -> Alex a -> Alex a
handleError handler (Alex lex) = Alex $ \s -> case (lex s) of
  Left errMsg -> unAlex (handler errMsg) s
  Right (s', x) -> Right (s', x)

-- | Returns a simple (line:col) description of the given position.
showPosn :: AlexPosn -> String
showPosn (AlexPn _ line col) = show line ++ ":" ++ show col

debugPosn (AlexPn bytes line col) = show bytes ++ ":" ++ show line ++ ":" ++ show col

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

-- | Checks if we are really at the end of input, and returns an error otherwise.
ensureEOF :: Alex ()
ensureEOF = Alex $ \s -> case s of
  AlexState{alex_inp=[]} -> Right (s, ())
  _ -> Left "Input not fully consumed"

-- | Invoked at the end of lexing to make sure we are not in an unfinished state.
ensureInitStartCode :: Alex ()
ensureInitStartCode = do
  sc <- alexGetStartCode
  Alex $ \s -> case sc of
    val | val == state_comment -> Left "Unterminated comment"
        | val == state_string -> Left "Unterminated string literal"
        | otherwise -> Right (s, ())

-- Interface with lexer

-- | The different kinds of tokens produced by our lexer.
-- Skip and EOF are only used interhally, and will not be returned by the lexer.
data TokenType = Command | StringLit | Skip | EOF deriving (Show, Eq)

-- | The lexer token.
data Token = Token {
  tok_pos :: AlexPosn,       -- ^ The position in the stream where this token started or was triggered.
                             --   For EOF, this will be undefined.
  tok_type :: TokenType,     -- ^ The type of token.
  tok_string :: Maybe String -- ^ The text associated with the token, or Nothing if this is an
                             --   abstract token.
  }

instance Show Token where
  show (Token _ EOF _) = "#<EOF>"
  show (Token pos typ mbs) = "#<" ++ show typ ++ " " ++ showPosn pos ++ " " ++ show mbs ++ ">"

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
  pending_literal :: String        -- ^ If we are parsing a string literal, this is a stack of characters
                                   --   processed thus far.
  }
alexInitUserState = LilyLexerState alexStartPos ""

-- Internal action helpers

logPosn :: Alex ()
logPosn = getPosn >>= \pos -> alexLog ("Position now: " ++ debugPosn pos)

-- | Creates an action that returns a token with the given token type.
mkT :: TokenType -> AlexAction Token
mkT t (p, _, _, s) n = return $ Token p t (Just (take n s))

handleBom :: AlexAction Token
handleBom inp@((AlexPn pos _ _), _, _, _) len =
  if pos > 0
  then alexError "Stray UTF-8 BOM"
  else skip inp len

beginComment :: AlexAction Token
beginComment inp len = (mkT Skip `andBegin` state_comment) inp len

endComment :: AlexAction Token
endComment inp len = (mkT Skip `andBegin` 0) inp len

beginStringLiteral :: AlexAction Token
beginStringLiteral inp@(pos, _, _, _) len =
  modifyUserState setPendingLiteralPos >> alexSetStartCode state_string >> mkT Skip inp len
  where
    setPendingLiteralPos us = us{pending_literal_pos=pos}

pushStringLiteralChar :: Char -> AlexAction Token
pushStringLiteralChar c inp len = modifyUserState _pushStringLiteralChar >> mkT Skip inp len
  where
    _pushStringLiteralChar us@LilyLexerState{pending_literal=cs} = us{pending_literal=(c:cs)}

pushStringLiteralCurr :: AlexAction Token
pushStringLiteralCurr inp@(_, _, _, s) len =
  modifyUserState (pushStringLiteral charsToPush) >> mkT Skip inp len
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
    showContext inp | null inp = "at end of file"
                    | null (tail inp) = "on char " ++ show (head inp) ++ " at end of line"
                    | otherwise = "on char " ++ show (head inp) ++ " before: '"
                                  ++ (trim . take 30 . tail) inp ++ "'"
    trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

-- | Keeps running a lexer until a particular output is reached. Return the list of values generated
-- up to that point.
loopUntil :: (a -> Bool) -> Alex a -> Alex [a]
loopUntil f lex = lex >>= loopOrQuit
  where
    loopOrQuit x | f x = return []
                 | otherwise = lex >>= loopOrQuit >>= \rest -> return (x : rest)

-- | Scans a token, and nicely formats the error if there is one.
scanToken :: Alex Token
scanToken = handleError lexerError alexMonadScan

-- | Scans a string and returns either an error message or the list of tokens produced by the
-- string.
scanner :: String -> Either String [Token]
scanner str = runAlex str _scanner
  where
    _scanner :: Alex [Token]
    _scanner = do
      toks <- loopUntil eof scanToken
      handleError lexerError ensureInitStartCode
      return $ filter notSkip toks
    eof = (== EOF) . tok_type
    notSkip = (/= Skip) . tok_type

-- | Scans a string for a single token, and returns either an error message or the token produced by
-- the string. If the string is not completely consumed by the scanner, an error is also returned.
--
-- This function is used by unit tests to ensure that the lexer is consuming exactly what is
-- expected.
scanExactlyOne :: String -> Either String Token
scanExactlyOne str = runAlex str _scanExactlyOne
  where
    _scanExactlyOne :: Alex Token
    _scanExactlyOne = scanToken >>= \tok ->
      if notSkip tok then checkExactlyOne tok else _scanExactlyOne
    checkExactlyOne tok = ensureEOF >> ensureInitStartCode >> return tok
    notSkip = (/= Skip) . tok_type
}
