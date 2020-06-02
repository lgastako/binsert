{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main ( main ) where

import Prelude

import Control.Exception ( bracket )
import Control.Monad     ( unless
                         , when
                         )
import Data.Char         ( toUpper )
import Options.Generic
import System.Directory  ( doesDirectoryExist
                         , doesFileExist
                         )
import System.Exit       ( die )
import System.IO         ( BufferMode( NoBuffering )
                         , hGetBuffering
                         , hSetBuffering
                         , stdout
                         )
import Text.Read         ( readMaybe )

data Options = Options
  { showBefore :: Bool
  , showAfter  :: Bool
  , path       :: FilePath
  } deriving (Generic, Show)

instance ParseRecord Options

main :: IO ()
main = getRecord "binsert" >>= insertIntoFile

insertIntoFile :: Options -> IO ()
insertIntoFile Options {..} = do
  ensureExists path
  content <- readFile path
  when showBefore $ putStr content
  withStdoutMode NoBuffering $
    writeFile path . unlines =<< insertIntoItems (lines content) =<< itemPrompt
  when showAfter $ putStr =<< readFile path
  where
    itemPrompt = putStr "Enter new item: " >> getLine

withStdoutMode :: BufferMode -> IO a -> IO a
withStdoutMode n action = bracket
  (hGetBuffering stdout <* hSetBuffering stdout n)
  (hSetBuffering stdout)
  (const action)

-- withStdoutMode n action = (\o -> set n *> action <* set o) =<< hGetBuffering stdout
--   where
--     set = hSetBuffering stdout

ensureExists :: FilePath -> IO ()
ensureExists path = do
  d <- doesDirectoryExist path
  when d . die $ "Path " ++ show path ++ " exists but is a directory, not a file!"
  f <- doesFileExist path
  unless f . writeFile path $ ""

data Choice = A | B | Q
  deriving (Eq, Read, Show)

insertIntoItems :: [String] -> String -> IO [String]
insertIntoItems []    item = pure [item]
insertIntoItems items item = go ([], items, [])
  where
    go (a, mid, c) = do
      putStr . unlines $
        [ "Select whether the item should go [A]bove or [B]elow the target"
        , "(Or [Q]uit to abort)."
        , "    item: " ++ show item
        , "  target: " ++ show target
        ]
      getChoice >>= \case
        A ->  (a ++) . (++ (target:suf      ++ c)) <$> insertIntoItems pre item
        B -> ((a ++ pre ++ [target]) ++) . (++ c)  <$> insertIntoItems suf item
        Q -> die "Aborted."
      where
        (pre, suf, target) = case splitAt (length mid `div` 2) mid of
          ([], [])   -> ([],      [item], []     )
          (ps, s:ss) -> (ps,      ss,     s      )
          (ps, [])   -> (init ps, [],     last ps)

    getChoice = maybe choices pure =<< readMaybe . map toUpper <$> prompt
      where
        prompt  = putStr "> " >> getLine
        choices = putStrLn "Pease enter 'a', 'b' or 'q'." >> getChoice
