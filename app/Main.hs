{-# LANGUAGE OverloadedStrings #-}

import Data.List
import Data.Semigroup ((<>))
import Data.Void (Void)
import Options.Applicative ((<**>))
import qualified Options.Applicative as CP
import Text.Megaparsec
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

data Bencode
  = BInt Integer
  | BString String
  | BList [Bencode]
  deriving (Show, Eq)

data Options = Options
  { comArg :: String,
    contentArg :: String
  }

bInt :: Parser Bencode
bInt = BInt <$> (char 'i' *> L.signed space L.decimal <* char 'e')

bString :: Parser Bencode
bString = do
  len <- L.decimal <* char ':'
  BString <$> count len anySingle

bList :: Parser Bencode
bList = BList <$> (char 'l' *> many bencode <* char 'e')

bencode :: Parser Bencode
bencode = try bInt <|> bString <|> bList

-- Define the command line parser for `Options`
optionsParser :: CP.Parser Options
optionsParser =
  Options
    <$> CP.argument
      CP.str
      ( CP.metavar "COMMAND"
          <> CP.help "Decode command"
      )
    <*> CP.argument
      CP.str
      ( CP.metavar "CONTENT"
          <> CP.help "Content to decode"
      )

-- Info for the command line parser
parserInfo :: CP.ParserInfo Options
parserInfo =
  CP.info
    (optionsParser <**> CP.helper)
    ( CP.fullDesc
        <> CP.progDesc "Decode CONTENT based on the provided options"
        <> CP.header "decoder - a command line decoder tool"
    )

printContent :: Bencode -> IO ()
printContent bencode = putStrLn $ formatBencode bencode

formatBencode :: Bencode -> String
formatBencode (BInt n) = show n
formatBencode (BString s) = "\"" ++ s ++ "\""
formatBencode (BList list) = "[" ++ intercalate ", " (map formatBencode list) ++ "]"

main :: IO ()
main = do
  opts <- CP.execParser parserInfo
  let com = comArg opts
  let content = contentArg opts
  case com of
    "decode" -> case parse bencode "" content of
      Left bundle -> putStrLn $ "Error decoding content: " ++ errorBundlePretty bundle
      Right bencodedData -> printContent bencodedData
    _ -> putStrLn "Unrecognized command."
