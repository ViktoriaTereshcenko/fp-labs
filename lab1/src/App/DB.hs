{-# LANGUAGE OverloadedStrings #-}

module App.DB (createConn, runMigrations) where

import           Control.Exception (try, SomeException)
import           Control.Monad (void, when)
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as SBS

-- Env/IO
import           System.Directory (doesFileExist)
import           System.Environment (lookupEnv)
import           Data.Word (Word16)
import           Text.Read (readMaybe)

-- PostgreSQL
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Transaction (withTransaction)
import           Database.PostgreSQL.Simple.Types (Query(..))

--------------------------------------------------------------------------------
-- ПІДКЛЮЧЕННЯ ДО PostgreSQL
--------------------------------------------------------------------------------

readEnv :: String -> String -> IO String
readEnv key def = fromMaybe def <$> lookupEnv key

readEnvPort :: String -> Word16 -> IO Word16
readEnvPort key def = do
  mv <- lookupEnv key
  pure $ maybe def (\s -> maybe def fromIntegral (readMaybe s :: Maybe Int)) mv

createConn :: IO Connection
createConn = do
  host <- readEnv "PGHOST"     "localhost"
  port <- readEnvPort "PGPORT" 6969
  user <- readEnv "PGUSER"     "postgres"
  pass <- readEnv "PGPASSWORD" "22112004"
  db   <- readEnv "PGDATABASE" "faculty_sport"
  connect defaultConnectInfo
    { connectHost     = host
    , connectPort     = port
    , connectUser     = user
    , connectPassword = pass
    , connectDatabase = db
    }

--------------------------------------------------------------------------------
-- ІНІЦІАЛІЗАЦІЯ БД
--------------------------------------------------------------------------------

runMigrations :: Connection -> IO ()
runMigrations conn = do
  -- приглушимо NOTICE-и
  void (execute_ conn "SET client_min_messages = WARNING")
  ok <- doesFileExist "db.sql"
  when ok $ do
    raw <- SBS.readFile "db.sql"
    let txt = TE.decodeUtf8 raw
        -- прибираємо все, що не можна всередині транзакції
        isForbidden s =
          let s' = T.toCaseFold (T.strip s)
          in  T.isPrefixOf "create database" s'
           || T.isPrefixOf "\\connect" (T.strip s)
           || T.isPrefixOf "\\c"       (T.strip s)
           || s' == "begin;"
           || s' == "commit;"
        cleaned = T.unlines (filter (not . isForbidden) (T.lines txt))
    when (not (T.null (T.strip cleaned))) $ do
      res <- try (withTransaction conn $ void (execute_ conn (Query (TE.encodeUtf8 cleaned))))
      case (res :: Either SomeException ()) of
        Left e  -> putStrLn $ "Помилка міграції: " ++ show e
        Right _ -> pure ()
