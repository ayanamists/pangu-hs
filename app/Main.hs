module Main (main) where

import Data.Text.Pangu
import qualified Data.Text as T

main :: IO ()
main = do
  line <- T.pack <$> getLine
  print $ panguParse line
