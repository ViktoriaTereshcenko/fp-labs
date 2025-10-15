{-# LANGUAGE OverloadedStrings #-}

module App.UI (dash, tMaybe, fmtTime, fmtDay, sayChanged,
              sayEmptyIf, withHeader, printTable, confirm,
              prettyStudent, prettyInstructor, prettySection,
              prettyScheduleRow, prettyMembershipRow,
              prettyCompetitionRow, prettyCPRow) where

import           Control.Monad (when)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Time (Day, defaultTimeLocale, formatTime)
import           Data.Time.LocalTime (TimeOfDay(..))

-- Інші файли проєкта
import           App.Types (Student(..), Instructor(..), Section(..))

-- Env/IO
import           System.IO (hFlush, stdout)

-- ANSI (кольори/стиль)
import           System.Console.ANSI (hSupportsANSI, setSGR,
                 SGR(..), ColorIntensity(..), ConsoleIntensity(..),
                 ConsoleLayer(..), Color(..))

--------------------------------------------------------------------------------
-- УТИЛІТИ ДЛЯ ПРИНТУ / UI
--------------------------------------------------------------------------------

dash :: Text
dash = "—"

tMaybe :: Maybe Text -> Text
tMaybe = fromMaybe dash

fmtTime :: TimeOfDay -> Text
fmtTime (TimeOfDay h m _) =
  let two n = if n < 10 then '0':show n else show n
  in T.pack (two h <> ":" <> two m)

fmtDay :: Day -> Text
fmtDay d = T.pack (formatTime defaultTimeLocale "%F" d)

sayChanged :: String -> Integer -> IO ()
sayChanged action n =
  putStrLn $ (if n == 0 then "Нічого не змінено (" else action ++ ": ")
          ++ show n ++ (if n==0 then ")" else "")

sayEmptyIf :: Bool -> IO ()
sayEmptyIf isEmpty = when isEmpty (putStrLn "(порожньо)")

withHeader :: Text -> IO a -> IO a
withHeader title action = do
  ansi <- hSupportsANSI stdout
  when ansi (setSGR [SetColor Foreground Vivid Cyan, SetConsoleIntensity BoldIntensity])
  TIO.putStrLn ("\n" <> title)
  when ansi (setSGR [Reset])
  action

padR :: Int -> Text -> Text
padR w t =
  let len = T.length t
  in if len >= w then t else t <> T.replicate (w - len) " "

truncateEll :: Int -> Text -> Text
truncateEll w t = if T.length t <= w then t else T.take (max 1 (w-1)) t <> "…"

fitCols :: Int -> [Text] -> [Text]
fitCols n xs = take n (xs ++ repeat "")

printTable :: [Text] -> [[Text]] -> IO ()
printTable headers rows = do
  let nCols   = length headers
      rows'   = map (fitCols nCols) rows
      initW   = map T.length headers
      grow ws xs = zipWith max ws (map T.length xs)
      widths  = foldl grow initW rows'
      fitRow xs = zipWith (\w x -> padR w (truncateEll w x)) widths xs
      sep     = T.intercalate "-+-" [ T.replicate w "-" | w <- widths ]
  -- header
  ansi <- hSupportsANSI stdout
  when ansi (setSGR [SetColor Foreground Vivid Cyan, SetConsoleIntensity BoldIntensity])
  TIO.putStrLn (T.intercalate " | " (fitRow headers))
  when ansi (setSGR [Reset])
  TIO.putStrLn sep
  -- rows
  mapM_ (TIO.putStrLn . T.intercalate " | " . fitRow) rows'

confirm :: String -> IO Bool
confirm msg = do
  putStr (msg ++ " [y/yes/т/так, default=N]: ") >> hFlush stdout
  ans <- fmap (map toLower . dropWhile (==' ') . reverse . dropWhile (==' ') . reverse) getLine
  pure (ans `elem` ["y","yes","т","так"])
  where
    toLower c = if 'A' <= c && c <= 'Z' then toEnum (fromEnum c + 32) else c

--------------------------------------------------------------------------------
-- PRETTY-ВИВІД (строкові представлення)
--------------------------------------------------------------------------------

prettyStudent :: Student -> [Text]
prettyStudent s =
  [ maybe "-" (T.pack . show) (stId s)
  , stLastName s <> " " <> stFirstName s
  , stGroup s
  , fmtDay (stBirth s)
  , tMaybe (stPhone s)
  ]

prettyInstructor :: Instructor -> [Text]
prettyInstructor i =
  [ maybe "-" (T.pack . show) (insId i)
  , insLastName i <> " " <> insFirstName i
  , insDept i
  , tMaybe (insPhone i)
  ]

prettySection :: Section -> [Text]
prettySection s =
  [ maybe "-" (T.pack . show) (secId s)
  , secName s
  , secLevel s
  , maybe "-" (T.pack . show) (secInstructor s)
  ]

prettyScheduleRow :: (Int, Text, Text, Text, Text, Text) -> [Text]
prettyScheduleRow (sid, sname, w, st, en, loc) =
  [ T.pack (show sid)
  , sname
  , w
  , st <> "–" <> en
  , loc
  ]

prettyMembershipRow :: (Int, Text, Text, Day) -> [Text]
prettyMembershipRow (mid, sname, secname, d) =
  [ T.pack (show mid), sname, secname, fmtDay d ]

prettyCompetitionRow :: (Int, Text, Day, Text, Text) -> [Text]
prettyCompetitionRow (cid, title, heldOn, venue, secName) =
  [ T.pack (show cid), title, fmtDay heldOn, venue, secName ]

prettyCPRow :: (Int, Text, Text, Maybe Text) -> [Text]
prettyCPRow (cid, ctitle, sname, notes) =
  [ T.pack (show cid), ctitle, sname, tMaybe notes ]
