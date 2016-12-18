{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      :  ExportPass
-- Copyright   :  Benjamin Jones 2016
-- License     :  MIT
--
-- Maintainer  :  benjaminfjones@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- This program converts user/password/info records in a certain format to CSV
-- format suitable for importing to LastPass.
--
-- The format of a single entry is:
--
--     name: test name
--     type: test type
--     username: test username
--     password: test password
--     hostname: test hostname
--     extra: test extra
--     grouping: test grouping
--     <blank line>
--
-- All of the fields are optional, but there must be at least one field
-- present per entry and at least one blank line between entries.
--
-- Note: in the CSV file, "hostname" becomes "url" and the "hostname" CSV
-- column is left blank. This behavior can be changed by modifying 'ppEntry'
-- below.

module ExportPass where

import Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Foldable
import Data.List (intercalate)
import Data.Maybe (fromMaybe)


data Entry = Entry
  { name     :: Maybe ByteString
  , etype    :: Maybe ByteString
  , username :: Maybe ByteString
  , password :: Maybe ByteString
  , hostname :: Maybe ByteString
  , extra    :: Maybe ByteString
  , grouping :: Maybe ByteString
  } deriving (Eq, Show)

defaultEntry = Entry Nothing Nothing Nothing Nothing Nothing Nothing Nothing

newtype EntryXformer = EntryXformer { unXform :: Entry -> Entry }

instance Monoid EntryXformer where
  mempty = EntryXformer id
  x `mappend` y = EntryXformer (unXform x . unXform y)


-- Parsing Entries -------------------------------------------------------------

blankLineP :: Parser ()
blankLineP = skipWhile (inClass " \t") >> endOfLine

entries :: Parser [Entry]
entries = do
  many' (entry <* skipMany blankLineP)

entry :: Parser Entry
entry = do
  ps <- many1 (choice [ nameP , etypeP, usernameP, passwordP
                      , hostnameP, extraP, groupingP ] <* endOfLine)
  endOfLine  -- blankLineP
  return $ (unXform (fold ps)) defaultEntry

nameP :: Parser EntryXformer
nameP = do
  n <- "name: " *> A.takeWhile (/= '\n')
  return $ EntryXformer $ \e -> e { name = Just n }

etypeP :: Parser EntryXformer
etypeP = do
  n <- "type: " *> A.takeWhile (/= '\n')
  return $ EntryXformer $ \e -> e { etype = Just n }

usernameP :: Parser EntryXformer
usernameP = do
  n <- "username: " *> A.takeWhile (/= '\n')
  return $ EntryXformer $ \e -> e { username = Just n }

passwordP :: Parser EntryXformer
passwordP = do
  n <- "password: " *> A.takeWhile (/= '\n')
  return $ EntryXformer $ \e -> e { password = Just n }

hostnameP :: Parser EntryXformer
hostnameP = do
  n <- "hostname: " *> A.takeWhile (/= '\n')
  return $ EntryXformer $ \e -> e { hostname = Just n }

extraP :: Parser EntryXformer
extraP = do
  n <- "extra: " *> A.takeWhile (/= '\n')
  return $ EntryXformer $ \e -> e { extra = Just n }

groupingP :: Parser EntryXformer
groupingP = do
  n <- "grouping: " *> A.takeWhile (/= '\n')
  return $ EntryXformer $ \e -> e { grouping = Just n }


-- Export to CSV ---------------------------------------------------------------

header :: String
header = "url, type, username, password, hostname, extra, name, grouping"

ppEntry :: Entry -> String
ppEntry e = intercalate "," cols
  where    -- note: hostname is listed under "URL"
    cols :: [String]
    cols = map sanitize  -- take case of commas
         . map (BS.unpack)
         . map (fromMaybe BS.empty)
         $ [ hostname e, etype e, username e, password e
           , Nothing, extra e, name e, grouping e ]
    sanitize = map (\c -> if c == ',' then ';' else c)

ppEntries :: [Entry] -> String
ppEntries es = unlines (header : map ppEntry es)


-- MAIN ------------------------------------------------------------------------

main :: IO ()
main = do
  res <- parseOnly entries <$> BS.getContents
  case res of
    Left err -> error $ "error: failed to parse input: " ++ err
    Right es -> putStrLn $ ppEntries es
