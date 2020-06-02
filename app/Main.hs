{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad      ( unless
                          , when
                          )
import Data.Char          ( toUpper )
import Data.List          ( splitAt )
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

main :: IO ()
main = getArgs >>= \case
  [path] -> insertIntoFile path
  _      -> die "Please enter exactly one file on the command line."

insertIntoFile :: FilePath -> IO ()
insertIntoFile path = do
  ensureExists path
  hSetBuffering stdout NoBuffering
  entries <- lines <$> readFile path
  item <- putStr "Enter new item: " >> getLine
  writeFile path . unlines =<< insertIntoItems item entries

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
    go (before, []    , after) = pure $ before ++ item:after
    go (before, during, after) = do
      let idx        = length during `div` 2
          (pre, suf) = splitAt idx during
          (pre', suf', target) = case (pre, suf) of
            ([], [])   -> error "how do this occur?"
            (ps, s:ss) -> (ps, ss, s)
            (ps, [])   -> (init ps, [], last ps)
      putStrLn $ "Select whether the item should go [A]bove or [B]elow the target"
      putStrLn $ "(Or [Q]uit to abort)."
      putStrLn $ "    item: " ++ show item
      putStrLn $ "  target: " ++ show target
      getChoice >>= \case
        A -> (before ++) . (++ (target:suf' ++ after)) <$> insertIntoItems item pre'
        B -> ((before ++ pre' ++ [target]) ++) . (++ after) <$> insertIntoItems item suf'
        Q -> die "Aborted."

    getChoice = do
      putStr $ "> "
      s <- map toUpper <$> getLine
      maybe (putStrLn "Pease enter 'a', 'b' or 'q'." >> getChoice) pure $ readMaybe s
