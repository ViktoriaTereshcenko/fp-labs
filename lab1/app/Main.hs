{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception (try, SomeException, bracket)
import           Control.Monad (when)

-- Інші файли проєкта
import           App.DB (createConn, runMigrations)
import           App.CLI (loop)

-- Env/IO
import           System.IO (stdout)

-- ANSI (кольори/стиль)
import           System.Console.ANSI (hSupportsANSI, setSGR,
                 SGR(..), ColorIntensity(..), ConsoleIntensity(..),
                 ConsoleLayer(..), Color(..))

-- PostgreSQL
import           Database.PostgreSQL.Simple (close)

--------------------------------------------------------------------------------
-- ТОЧКА ВХОДУ ТА ІНІЦІАЛІЗАЦІЯ
--------------------------------------------------------------------------------

main :: IO ()
main = do
  res <- try $ bracket createConn close $ \conn -> do
    ansi <- hSupportsANSI stdout
    when ansi (setSGR [SetColor Foreground Vivid Green, SetConsoleIntensity BoldIntensity])
    putStrLn "\nСпорт на факультеті"
    when ansi (setSGR [Reset])
    runMigrations conn
    loop conn
  case (res :: Either SomeException ()) of
    Left e  -> putStrLn $ "\nПомилка БД/IO: " ++ show e
    Right _ -> pure ()
