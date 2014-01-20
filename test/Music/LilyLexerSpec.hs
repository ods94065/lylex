module Music.LilyLexerSpec (spec) where
import Control.Monad
import Test.Hspec
import Test.HUnit
import Music.LilyLexer

succeedsWith :: Either String Token -> (TokenType, Maybe String) -> Assertion
succeedsWith result (expectedType, expectedString) =
  either lexerFailed (checkToken expectedType expectedString) result
  where
    lexerFailed msg = assertFailure $ "Lexer failure: " ++ msg
    checkToken expectedType expectedString (Token _ actualType actualString) = do
      expectedType @=? actualType
      expectedString @=? actualString

failsToLex :: Either String Token -> Assertion
failsToLex result =
  either (\s -> return ()) lexerPassed result
  where
    lexerPassed tok@(Token _ t _) = assertFailure (
      "Lexer returned token of type " ++ show t ++ " but should have failed\n" ++
      "Token: " ++ show tok)

specGoodCase :: String -> (TokenType, Maybe String) -> Spec
specGoodCase input expectedValue =
  it ("handles " ++ show input ++ " as " ++ show expectedValue) $ do
    scanExactlyOne input `succeedsWith` expectedValue

specBadCase :: String -> Spec
specBadCase input =
  it ("fails to lex " ++ show input) $ do
    (failsToLex . scanExactlyOne) input

specScanner = context "scanner" $ do
  it "returns an empty list if empty input" $ do
    Right [] @=? scanner ""
  it "returns an empty list if only comments and whitespace" $ do
    Right [] @=? scanner " \n\t%{ blah blah blah %}\n\t "
  it "returns an error if unterminated comment" $ do
    Left "Unterminated comment at 1:8 at end of file" @=? scanner "%{ blah"
  it "returns an error if unterminated string" $ do
    Left "Unterminated string literal at 1:6 at end of file" @=? scanner "\"blah"
  context "when returning errors" $ do
    it "reports position correctly after whitespace" $ do
      Left "Illegal character at 1:4 on char 'a' before: 'bc'" @=? scanner "   abc"
    it "reports position correctly after comment" $ do
      Left "Illegal character at 1:6 on char 'a' before: 'bc'" @=? scanner "%{ %}abc"
    it "reports position correctly after string literal" $ do
      Left "Illegal character at 1:7 on char 'a' before: 'bc'" @=? scanner "\"blah\"abc"
    it "tracks newlines correctly" $ do
      Left "Illegal character at 2:1 on char 'a' before: 'bc'" @=? scanner "\nabc"
    it "reports EOL context correctly" $ do
      Left "Illegal character at 1:6 on char 'a' at end of line" @=? scanner "%{ %}a\n"

specSkippedInputs = context "skipped inputs" $ mapM_ specSkipped goodCases
  where
    goodCases = ["", " \n  \t   \r\n", "\xFEFF", "%{ abc %}", "%{%}", "%{\"%}", "%{ abc %%}"]
    specSkipped input = specGoodCase input (EOF, Nothing)

specGoodInputs = context "good inputs" $ mapM_ (uncurry specGoodCase) goodCases
  where
    goodCases = [
      ("\\a", (Command, Just "\\a")),
      ("\\A", (Command, Just "\\A")),
      ("\\a_b", (Command, Just "\\a_b")),
      ("\\a-b", (Command, Just "\\a-b")),
      ("\"\"", (StringLit, Just "")),
      ("\"abc\"", (StringLit, Just "abc")),
      ("\"\\n\"", (StringLit, Just "\n")),
      ("\"\\t\"", (StringLit, Just "\t")),
      ("\"\\\"\"", (StringLit, Just "\"")),
      ("\"\\'\"", (StringLit, Just "'")),
      ("\"\\\\\"", (StringLit, Just "\\")),
      ("\"%{\"", (StringLit, Just "%{"))]

specBadInputs = context "bad inputs" $ mapM_ specBadCase badCases
  where
    badCases = ["\\", "\n\xFEFF", "\\_a", "\\a_", "\\-a", "\\a-", "\\a1", "%{", "\""]

spec = do
  describe "LilyLexer" $ do
    specScanner
    specSkippedInputs
    specGoodInputs
    specBadInputs

main :: IO ()
main = hspec spec
