{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char (ord)
import Data.List
import Data.Semigroup ((<>))
import Data.Void (Void)
import Options.Applicative ((<**>))
import qualified Options.Applicative as CP
import System.IO (readFile)
import Text.Megaparsec
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- Convert ByteString to String
convertToString :: BSC.ByteString -> String
convertToString = BSC.unpack

-- Convert String to ByteString
convertToByteString :: String -> BSC.ByteString
convertToByteString = BSC.pack

parseInfoFile :: String -> IO ()
parseInfoFile fileName = do
  content <- BS.readFile fileName
  let contentStr = BSC.unpack content -- Convert ByteString to String
  case parse bencode "" contentStr of
    Left bundle -> putStrLn $ "Error parsing file: " ++ errorBundlePretty bundle
    Right bencodedData -> extractAndPrintInfo bencodedData

extractAndPrintInfo :: Bencode -> IO ()
extractAndPrintInfo (BDictionary dict) = do
  let maybeAnnounce = lookup "announce" dict >>= extractString
  let maybeInfo = lookup "info" dict >>= extractDict
  let maybeLength = maybeInfo >>= lookup "length" >>= extractInt
  let maybeInfoHash = lookup "pieces" dict >>= extractString
  putStrLn $ "Tracker URL: " ++ maybe "<not found>" id maybeAnnounce
  putStrLn $ "Length: " ++ maybe "<not found>" show maybeLength
  putStrLn $ "Hash: " ++ maybe "<not found>" show maybeInfoHash
  where
    extractString (BString s) = Just s
    extractString _ = Nothing

    extractInt (BInt n) = Just n
    extractInt _ = Nothing

    extractDict (BDictionary d) = Just d
    extractDict _ = Nothing

type Parser = Parsec Void String

data Bencode
  = BInt Integer
  | BString String
  | BList [Bencode]
  | BDictionary [(String, Bencode)]
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
  content <- count len anySingle -- Directly captures binary data without interpretation
  return $ BString content

bList :: Parser Bencode
bList = BList <$> (char 'l' *> many bencode <* char 'e')

bDictionary :: Parser Bencode
bDictionary = BDictionary <$> (char 'd' *> many pair <* char 'e')
  where
    pair = do
      key <- bString
      value <- bencode
      case key of
        BString s -> return (s, value)
        _ -> fail "Dictionary keys must be bencoded strings."

bencode :: Parser Bencode
bencode = try bInt <|> bString <|> bList <|> bDictionary

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
formatBencode (BDictionary dict) = "{" ++ intercalate ", " (map formatPair dict) ++ "}"
  where
    formatPair (k, v) = "\"" ++ k ++ "\": " ++ formatBencode v

main :: IO ()
main = do
  opts <- CP.execParser parserInfo
  let com = comArg opts
  let content = contentArg opts
  case com of
    "info" -> parseInfoFile content
    "decode" -> case parse bencode "" content of
      Left bundle -> putStrLn $ "Error decoding content: " ++ errorBundlePretty bundle
      Right bencodedData -> printContent bencodedData
    _ -> putStrLn "Unrecognized command."
