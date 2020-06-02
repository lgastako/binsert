{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main ( main ) where

import Control.Monad      ( unless
                          , when
                          )
import Data.Char          ( toUpper )
import Options.Generic
import System.Directory   ( doesDirectoryExist
                          , doesFileExist
                          )
import System.Environment ( getArgs )
import System.Exit        ( die )
import System.IO          ( BufferMode( NoBuffering )
                          , hSetBuffering
                          , stdout
                          )
import Text.Read          ( readMaybe )

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
  hSetBuffering stdout NoBuffering
  content <- readFile path
  when showBefore $ putStr content
  item <- putStr "Enter new item: " >> getLine
  writeFile path . unlines =<< insertIntoItems item (lines content)
  when showAfter $ putStr =<< readFile path

ensureExists :: FilePath -> IO ()
ensureExists path = do
  d <- doesDirectoryExist path
  when d . die $ "Path " ++ show path ++ " exists but is a directory, not a file!"
  f <- doesFileExist path
  unless f . writeFile path $ ""

data Choice = A | B | Q
  deriving (Eq, Read, Show)

insertIntoItems :: String -> [String] -> IO [String]
insertIntoItems item []    = pure [item]
insertIntoItems item items = go ([], items, [])
  where
    -- go (a, [] , c) = pure $ a ++ item:c
    go (a, mid, c) = do
      putStr . unlines $
        [ "Select whether the item should go [A]bove or [B]elow the target"
        , "(Or [Q]uit to abort)."
        , "    item: " ++ show item
        , "  target: " ++ show target
        ]
      getChoice >>= \case
        A -> (a ++) . (++ (target:suf ++ c)) <$> insertIntoItems item pre
        B -> ((a ++ pre ++ [target]) ++) . (++ c) <$> insertIntoItems item suf
        Q -> die "Aborted."
      where
        (pre, suf, target) = case splitAt (length mid `div` 2) mid of
          ([], [])   -> ([], [item], [])
          (ps, s:ss) -> (ps, ss, s)
          (ps, [])   -> (init ps, [], last ps)

    getChoice = do
      s <- readMaybe . map toUpper <$> (putStr "> " >> getLine)
      maybe (putStrLn "Pease enter 'a', 'b' or 'q'." >> getChoice) pure s
