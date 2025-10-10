{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Control.Exception (bracket, try, SomeException)
import           Control.Monad (void, when)
import           Data.Char (toLower)
import           Data.Int (Int64)
import           Data.Maybe (fromMaybe)
import           Data.Time (Day, defaultTimeLocale, parseTimeM, formatTime)
import           Data.Time.Calendar (fromGregorian)
import           Data.Time.LocalTime (TimeOfDay(..))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.ByteString as SBS
import           GHC.Generics (Generic)

-- Env/IO
import           System.IO (hFlush, stdout)
import           System.Directory (doesFileExist)
import           System.Environment (lookupEnv)
import           Data.Word (Word16)
import           Text.Read (readMaybe)

-- ANSI (кольори/стиль)
import           System.Console.ANSI (hSupportsANSI, setSGR,
                 SGR(SetColor, SetConsoleIntensity, Reset),
                 ColorIntensity(Vivid), ConsoleIntensity(BoldIntensity),
                 ConsoleLayer(Foreground), Color(Cyan, Green))

-- PostgreSQL
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToRow
import           Database.PostgreSQL.Simple.ToField (ToField, toField)
import           Database.PostgreSQL.Simple.Transaction (withTransaction)
import           Database.PostgreSQL.Simple.Types (Query(..))

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

sayChanged :: String -> Int64 -> IO ()
sayChanged action n =
  putStrLn $ (if n == 0 then "Нічого не змінено (" else action ++ ": ") ++ show n ++ (if n==0 then ")" else "")

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

--------------------------------------------------------------------------------
-- МОДЕЛІ
--------------------------------------------------------------------------------

data Student = Student
  { stId        :: Maybe Int
  , stFirstName :: Text
  , stLastName  :: Text
  , stGroup     :: Text
  , stBirth     :: Day
  , stPhone     :: Maybe Text
  } deriving (Show, Generic)

instance FromRow Student where
  fromRow = Student <$> (Just <$> field)
                    <*> field <*> field <*> field <*> field <*> field

instance ToRow Student where
  toRow (Student _ f l g b p) = toRow (f, l, g, b, p)

data Instructor = Instructor
  { insId        :: Maybe Int
  , insFirstName :: Text
  , insLastName  :: Text
  , insDept      :: Text
  , insPhone     :: Maybe Text
  } deriving (Show, Generic)

instance FromRow Instructor where
  fromRow = Instructor <$> (Just <$> field)
                       <*> field <*> field <*> field <*> field

instance ToRow Instructor where
  toRow (Instructor _ f l d p) = toRow (f, l, d, p)

data Section = Section
  { secId         :: Maybe Int
  , secName       :: Text
  , secLevel      :: Text
  , secInstructor :: Maybe Int
  } deriving (Show, Generic)

instance FromRow Section where
  fromRow = Section <$> (Just <$> field)
                    <*> field <*> field <*> field

instance ToRow Section where
  toRow (Section _ n lvl mid) = toRow (n, lvl, mid)

data Schedule = Schedule
  { schId      :: Maybe Int
  , schSection :: Int
  , schWeekday :: Text
  , schStart   :: TimeOfDay
  , schEnd     :: TimeOfDay
  , schLoc     :: Text
  } deriving (Show, Generic)

instance FromRow Schedule where
  fromRow = Schedule <$> (Just <$> field)
                     <*> field <*> field <*> field <*> field <*> field

instance ToRow Schedule where
  toRow (Schedule _ s w st en loc) = toRow (s,w,st,en,loc)

data Membership = Membership
  { memId      :: Maybe Int
  , memStudent :: Int
  , memSection :: Int
  , memJoined  :: Day
  } deriving (Show, Generic)

instance FromRow Membership where
  fromRow = Membership <$> (Just <$> field) <*> field <*> field <*> field

instance ToRow Membership where
  toRow (Membership _ s c j) = toRow (s,c,j)

data Competition = Competition
  { cmpId       :: Maybe Int
  , cmpTitle    :: Text
  , cmpHeldOn   :: Day
  , cmpVenue    :: Text
  , cmpSection  :: Maybe Int
  } deriving (Show, Generic)

instance FromRow Competition where
  fromRow = Competition <$> (Just <$> field) <*> field <*> field <*> field <*> field

instance ToRow Competition where
  toRow (Competition _ t d v ms) = toRow (t,d,v,ms)

data CompetitionParticipant = CompetitionParticipant
  { cpId          :: Maybe Int
  , cpCompetition :: Int
  , cpStudent     :: Int
  , cpResultNotes :: Maybe Text
  } deriving (Show, Generic)

instance FromRow CompetitionParticipant where
  fromRow = CompetitionParticipant <$> (Just <$> field)
                                   <*> field <*> field <*> field

instance ToRow CompetitionParticipant where
  toRow (CompetitionParticipant _ cid sid notes) = toRow (cid, sid, notes)

--------------------------------------------------------------------------------
-- TYPECLASS ДЛЯ УНІФІКОВАНОГО CRUD
--------------------------------------------------------------------------------

class Persist a where
  type Key a
  tableName    :: a -> Query
  insertSQL    :: a -> Query
  updateSQL    :: a -> Query
  selectAllSQL :: a -> Query
  deleteSQL    :: a -> Query
  getKey       :: a -> Maybe (Key a)

-- Student
instance Persist Student where
  type Key Student = Int
  tableName _     = "students"
  insertSQL _     = "INSERT INTO students (first_name,last_name,group_code,birth_date,phone) VALUES (?,?,?,?,?)"
  updateSQL _     = "UPDATE students SET first_name=?, last_name=?, group_code=?, birth_date=?, phone=? WHERE student_id=?"
  selectAllSQL _  = "SELECT student_id, first_name, last_name, group_code, birth_date, phone FROM students ORDER BY last_name, first_name"
  deleteSQL _     = "DELETE FROM students WHERE student_id=?"
  getKey          = stId

-- Instructor
instance Persist Instructor where
  type Key Instructor = Int
  tableName _     = "instructors"
  insertSQL _     = "INSERT INTO instructors (first_name,last_name,department,phone) VALUES (?,?,?,?)"
  updateSQL _     = "UPDATE instructors SET first_name=?, last_name=?, department=?, phone=? WHERE instructor_id=?"
  selectAllSQL _  = "SELECT instructor_id, first_name, last_name, department, phone FROM instructors ORDER BY last_name, first_name"
  deleteSQL _     = "DELETE FROM instructors WHERE instructor_id=?"
  getKey          = insId

-- Section
instance Persist Section where
  type Key Section = Int
  tableName _     = "sections"
  insertSQL _     = "INSERT INTO sections (name, level, instructor_id) VALUES (?,?,?)"
  updateSQL _     = "UPDATE sections SET name=?, level=?, instructor_id=? WHERE section_id=?"
  selectAllSQL _  = "SELECT section_id, name, level::text, instructor_id FROM sections ORDER BY name"
  deleteSQL _     = "DELETE FROM sections WHERE section_id=?"
  getKey          = secId

-- Schedule
instance Persist Schedule where
  type Key Schedule = Int
  tableName _     = "section_schedule"
  insertSQL _     = "INSERT INTO section_schedule (section_id, weekday, start_time, end_time, location) VALUES (?,?,?,?,?)"
  updateSQL _     = "UPDATE section_schedule SET section_id=?, weekday=?, start_time=?, end_time=?, location=? WHERE schedule_id=?"
  selectAllSQL _  =
    "SELECT schedule_id, section_id, weekday::text, start_time, end_time, location \
    \FROM section_schedule \
    \ORDER BY section_id, \
    \CASE weekday WHEN 'Пн' THEN 1 WHEN 'Вт' THEN 2 WHEN 'Ср' THEN 3 \
    \WHEN 'Чт' THEN 4 WHEN 'Пт' THEN 5 WHEN 'Сб' THEN 6 WHEN 'Нд' THEN 7 END, \
    \start_time"
  deleteSQL _     = "DELETE FROM section_schedule WHERE schedule_id=?"
  getKey          = schId

-- Membership
instance Persist Membership where
  type Key Membership = Int
  tableName _     = "memberships"
  insertSQL _     = "INSERT INTO memberships (student_id, section_id, joined_at) VALUES (?,?,?)"
  updateSQL _     = "UPDATE memberships SET student_id=?, section_id=?, joined_at=? WHERE membership_id=?"
  selectAllSQL _  = "SELECT membership_id, student_id, section_id, joined_at FROM memberships ORDER BY joined_at DESC"
  deleteSQL _     = "DELETE FROM memberships WHERE membership_id=?"
  getKey          = memId

-- Competition
instance Persist Competition where
  type Key Competition = Int
  tableName _     = "competitions"
  insertSQL _     = "INSERT INTO competitions (title, held_on, venue, section_id) VALUES (?,?,?,?)"
  updateSQL _     = "UPDATE competitions SET title=?, held_on=?, venue=?, section_id=? WHERE competition_id=?"
  selectAllSQL _  = "SELECT competition_id, title, held_on, venue, section_id FROM competitions ORDER BY held_on DESC"
  deleteSQL _     = "DELETE FROM competitions WHERE competition_id=?"
  getKey          = cmpId

-- CompetitionParticipant
instance Persist CompetitionParticipant where
  type Key CompetitionParticipant = Int
  tableName _     = "competition_participants"
  insertSQL _     = "INSERT INTO competition_participants (competition_id, student_id, result_notes) VALUES (?,?,?)"
  updateSQL _     = "UPDATE competition_participants SET competition_id=?, student_id=?, result_notes=? WHERE cp_id=?"
  selectAllSQL _  = "SELECT cp_id, competition_id, student_id, result_notes FROM competition_participants ORDER BY cp_id"
  deleteSQL _     = "DELETE FROM competition_participants WHERE cp_id=?"
  getKey          = cpId

--------------------------------------------------------------------------------
-- CRUD-хелпери
--------------------------------------------------------------------------------

insertEntity :: (Persist a, ToRow a) => Connection -> a -> IO Int64
insertEntity conn x = execute conn (insertSQL x) (toRow x)

updateEntity :: (Persist a, ToRow a, ToField (Key a)) => Connection -> a -> IO Int64
updateEntity conn x = case getKey x of
  Nothing -> pure 0
  Just k  -> execute conn (updateSQL x) (toRow x ++ [toField k])

deleteEntity :: (Persist a, ToField (Key a)) => Connection -> a -> IO Int64
deleteEntity conn x = case getKey x of
  Nothing -> pure 0
  Just k  -> execute conn (deleteSQL x) (Only k)

selectAll :: (Persist a, FromRow a) => Connection -> a -> IO [a]
selectAll conn proxy = query_ conn (selectAllSQL proxy)

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

--------------------------------------------------------------------------------
-- СПЕЦІАЛЬНІ ЗАПИТИ (JOIN-и)
--------------------------------------------------------------------------------

-- 1) Студент -> секції
listStudentSections :: Connection -> IO [(Text, Text)]
listStudentSections conn = query_ conn
  "SELECT (s.last_name || ' ' || s.first_name) AS student, sec.name \
  \FROM memberships m \
  \JOIN students s ON s.student_id=m.student_id \
  \JOIN sections sec ON sec.section_id=m.section_id \
  \ORDER BY student, sec.name"

-- 2) Розклад конкретної секції
getSectionSchedule :: Connection -> Int -> IO [(Text, Text, Text)]
getSectionSchedule conn sid = query conn
  "SELECT weekday::text, to_char(start_time,'HH24:MI'), to_char(end_time,'HH24:MI') \
  \FROM section_schedule \
  \WHERE section_id=? \
  \ORDER BY CASE weekday WHEN 'Пн' THEN 1 WHEN 'Вт' THEN 2 WHEN 'Ср' THEN 3 \
  \WHEN 'Чт' THEN 4 WHEN 'Пт' THEN 5 WHEN 'Сб' THEN 6 WHEN 'Нд' THEN 7 END, start_time"
  (Only sid)

-- 3) Учасники змагання
listCompetitionParticipants :: Connection -> Int -> IO [(Text, Text)]
listCompetitionParticipants conn cid = query conn
  "SELECT c.title, (s.last_name || ' ' || s.first_name) AS student \
  \FROM competition_participants cp \
  \JOIN competitions c ON c.competition_id = cp.competition_id \
  \JOIN students s ON s.student_id = cp.student_id \
  \WHERE cp.competition_id = ? \
  \ORDER BY student" (Only cid)

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
      res <- try (withTransaction conn $
                    void (execute_ conn (Query (TE.encodeUtf8 cleaned)))
                 ) :: IO (Either SomeException ())
      case res of
        Left e  -> putStrLn $ "Помилка міграції: " ++ show e
        Right _ -> pure ()

--------------------------------------------------------------------------------
-- ПІДКЛЮЧЕННЯ ДО PostgreSQL
--------------------------------------------------------------------------------

readEnv :: String -> String -> IO String
readEnv key def = fromMaybe def <$> lookupEnv key

readEnvPort :: String -> Word16 -> IO Word16
readEnvPort key def = do
  mv <- lookupEnv key
  pure $ maybe def (fromIntegral . fromMaybe (fromIntegral def) . readMaybe) mv

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
-- ВВІД/ВАЛІДАЦІЯ
--------------------------------------------------------------------------------

prompt :: String -> IO String
prompt msg = putStr msg >> putStr ": " >> hFlush stdout >> getLine

promptInt :: String -> IO Int
promptInt label = do
  s <- prompt label
  case readMaybe s of
    Just n  -> pure n
    Nothing -> putStrLn "Невірне число. Спробуй ще." >> promptInt label

promptMaybeInt :: String -> IO (Maybe Int)
promptMaybeInt msg = do
  s <- prompt (msg ++ " (Enter = NULL)")
  case s of
    "" -> pure Nothing
    _  -> case readMaybe s of
            Just n  -> pure (Just n)
            Nothing -> putStrLn "Невірне число. Спробуй ще." >> promptMaybeInt msg

promptDay :: String -> IO Day
promptDay label = do
  s <- prompt (label ++ " (YYYY-MM-DD)")
  case parseTimeM True defaultTimeLocale "%F" s of
    Just d  -> pure d
    Nothing -> putStrLn "Невірна дата. Приклад: 2024-09-30" >> promptDay label

promptFromOptionsCI :: String -> [Text] -> IO Text
promptFromOptionsCI label options = do
  let lowerMap = [(T.toLower o, o) | o <- options]
  inp <- T.pack <$> prompt (label ++ " " ++ show (map T.unpack options))
  case lookup (T.toLower inp) lowerMap of
    Just canon -> pure canon
    Nothing    -> putStrLn "Невірне значення. Спробуй ще." >> promptFromOptionsCI label options

promptLevel :: IO Text
promptLevel = promptFromOptionsCI "Рівень" ["початковий","середній","просунутий"]

promptWeekday :: IO Text
promptWeekday = promptFromOptionsCI "День тижня" ["Пн","Вт","Ср","Чт","Пт","Сб","Нд"]

readTimeOfDay :: String -> IO TimeOfDay
readTimeOfDay label = do
  s <- prompt (label ++ " (HH:MM)")
  case break (==':') s of
    (hh, ':' : mm)
      | all (`elem` ("0123456789" :: String)) (hh ++ mm)
      , not (null hh), not (null mm) ->
          let mh  = readMaybe hh :: Maybe Int
              mm' = readMaybe mm :: Maybe Int
          in case (mh, mm') of
               (Just h, Just m) | h>=0 && h<24 && m>=0 && m<60
                 -> pure (TimeOfDay h m 0)
               _ -> retry
    _ -> retry
  where
    retry = putStrLn "Невірний час. Приклад: 14:30" >> readTimeOfDay label

--------------------------------------------------------------------------------
-- CLI-ОПЕРАЦІЇ
--------------------------------------------------------------------------------

dummyDay :: Day
dummyDay = fromGregorian 2000 1 1

-- STUDENT
addStudent :: Connection -> IO ()
addStudent conn = do
  f  <- T.pack <$> prompt "Ім'я"
  l  <- T.pack <$> prompt "Прізвище"
  g  <- T.pack <$> prompt "Група (напр. КН-21)"
  b  <- promptDay "Дата народження"
  pS <- prompt "Телефон (Enter = NULL)"
  let p = if null pS then Nothing else Just (T.pack pS)
  n <- insertEntity conn (Student Nothing f l g b p)
  sayChanged "Додано студентів" n

viewStudents :: Connection -> IO ()
viewStudents conn = do
  xs <- selectAll conn (Student Nothing "" "" "" dummyDay Nothing)
  withHeader "Студенти (відсортовано за прізвищем, ім’ям)" $ do
    sayEmptyIf (null xs)
    let headers = ["ID","ПІБ","Група","Нар.","Тел"]
        rows    = map (\s -> [ maybe "-" (T.pack . show) (stId s)
                             , stLastName s <> " " <> stFirstName s
                             , stGroup s
                             , T.pack (show (stBirth s))
                             , maybe "—" id (stPhone s)
                             ]) xs
    printTable headers rows

updateStudent :: Connection -> IO ()
updateStudent conn = do
  sid <- promptInt "Вкажіть ID студента для оновлення"
  f   <- T.pack <$> prompt "Нове ім'я"
  l   <- T.pack <$> prompt "Нове прізвище"
  g   <- T.pack <$> prompt "Нова група"
  b   <- promptDay "Нова дата народження"
  pS  <- prompt "Новий телефон (Enter = NULL)"
  let p = if null pS then Nothing else Just (T.pack pS)
  n <- updateEntity conn (Student (Just sid) f l g b p)
  sayChanged "Оновлено записів" n

deleteStudent :: Connection -> IO ()
deleteStudent conn = do
  sid <- promptInt "Вкажіть ID студента для видалення"
  ok <- confirm ("Видалити студента #" <> show sid <> "?")
  if ok
    then do n <- deleteEntity conn (Student (Just sid) "" "" "" dummyDay Nothing)
            sayChanged "Видалено записів" n
    else putStrLn "Скасовано."

-- INSTRUCTOR
addInstructor :: Connection -> IO ()
addInstructor conn = do
  f  <- T.pack <$> prompt "Ім'я викладача"
  l  <- T.pack <$> prompt "Прізвище викладача"
  d  <- T.pack <$> prompt "Кафедра/відділ"
  pS <- prompt "Телефон (Enter = NULL)"
  let p = if null pS then Nothing else Just (T.pack pS)
  n <- insertEntity conn (Instructor Nothing f l d p)
  sayChanged "Додано викладачів" n

viewInstructors :: Connection -> IO ()
viewInstructors conn = do
  xs <- selectAll conn (Instructor Nothing "" "" "" Nothing)
  withHeader "Викладачі (відсортовано за прізвищем, ім’ям)" $ do
    sayEmptyIf (null xs)
    let headers = ["ID","ПІБ","Кафедра","Тел"]
        rows    = map (\i -> [ maybe "-" (T.pack . show) (insId i)
                             , insLastName i <> " " <> insFirstName i
                             , insDept i
                             , maybe "—" id (insPhone i)
                             ]) xs
    printTable headers rows

updateInstructor :: Connection -> IO ()
updateInstructor conn = do
  iid <- promptInt "Вкажіть ID викладача для оновлення"
  f   <- T.pack <$> prompt "Нове ім'я"
  l   <- T.pack <$> prompt "Нове прізвище"
  d   <- T.pack <$> prompt "Нова кафедра/відділ"
  pS  <- prompt "Новий телефон (Enter = NULL)"
  let p = if null pS then Nothing else Just (T.pack pS)
  n <- updateEntity conn (Instructor (Just iid) f l d p)
  sayChanged "Оновлено" n

deleteInstructor :: Connection -> IO ()
deleteInstructor conn = do
  iid <- promptInt "Вкажіть ID викладача для видалення"
  ok <- confirm ("Видалити викладача #" <> show iid <> "?")
  if ok
    then do n <- deleteEntity conn (Instructor (Just iid) "" "" "" Nothing)
            sayChanged "Видалено" n
    else putStrLn "Скасовано."

-- SECTION
addSection :: Connection -> IO ()
addSection conn = do
  n   <- T.pack <$> prompt "Назва секції (напр. Баскетбол)"
  lvl <- promptLevel
  mid <- promptMaybeInt "Вкажіть ID викладача (Enter = NULL)"
  nIns <- insertEntity conn (Section Nothing n lvl mid)
  sayChanged "Додано секцій" nIns

viewSections :: Connection -> IO ()
viewSections conn = do
  xs <- selectAll conn (Section Nothing "" "" Nothing)
  withHeader "Секції (відсортовано за назвою)" $ do
    sayEmptyIf (null xs)
    let headers = ["ID","Назва","Рівень","Викл.ID"]
        rows    = map (\s -> [ maybe "-" (T.pack . show) (secId s)
                             , secName s
                             , secLevel s
                             , maybe "-" (T.pack . show) (secInstructor s)
                             ]) xs
    printTable headers rows

updateSection :: Connection -> IO ()
updateSection conn = do
  sid <- promptInt "Вкажіть ID секції для оновлення"
  n   <- T.pack <$> prompt "Нова назва"
  lvl <- promptLevel
  mid <- promptMaybeInt "Новий ID викладача (Enter = NULL)"
  let s = Section (Just sid) n lvl mid
  n' <- updateEntity conn s
  sayChanged "Оновлено" n'

deleteSection :: Connection -> IO ()
deleteSection conn = do
  sid <- promptInt "Вкажіть ID секції для видалення"
  ok <- confirm ("Видалити секцію #" <> show sid <> "?")
  if ok
    then do n <- deleteEntity conn (Section (Just sid) "" "" Nothing)
            sayChanged "Видалено" n
    else putStrLn "Скасовано."

-- SCHEDULE
addSchedule :: Connection -> IO ()
addSchedule conn = do
  sec  <- promptInt "Вкажіть ID секції"
  w    <- promptWeekday
  st   <- readTimeOfDay "Початок"
  en   <- readTimeOfDay "Кінець"
  loc  <- T.pack <$> prompt "Локація"
  n <- insertEntity conn (Schedule Nothing sec w st en loc)
  sayChanged "Додано рядків розкладу" n

viewSchedules :: Connection -> IO ()
viewSchedules conn = do
  rows <- query_ conn
    "SELECT ss.schedule_id, sec.name, ss.weekday::text, \
    \       to_char(ss.start_time,'HH24:MI'), to_char(ss.end_time,'HH24:MI'), ss.location \
    \FROM section_schedule ss \
    \JOIN sections sec ON sec.section_id = ss.section_id \
    \ORDER BY sec.name, \
    \CASE ss.weekday WHEN 'Пн' THEN 1 WHEN 'Вт' THEN 2 WHEN 'Ср' THEN 3 \
    \WHEN 'Чт' THEN 4 WHEN 'Пт' THEN 5 WHEN 'Сб' THEN 6 WHEN 'Нд' THEN 7 END, \
    \ss.start_time"
      :: IO [(Int, Text, Text, Text, Text, Text)]
  withHeader "Розклад (сорт: секція → день → час)" $ do
    sayEmptyIf (null rows)
    let headers = ["ID","Секція","День","Час","Локація"]
        rows'   = [ [ T.pack (show sid)
                    , sname
                    , w
                    , st <> "–" <> en
                    , loc
                    ]
                  | (sid, sname, w, st, en, loc) <- rows
                  ]
    printTable headers rows'

updateSchedule :: Connection -> IO ()
updateSchedule conn = do
  sid <- promptInt "Вкажіть ID запису розкладу для оновлення"
  sec <- promptInt "Новий ID секції"
  w   <- promptWeekday
  st  <- readTimeOfDay "Новий початок"
  en  <- readTimeOfDay "Новий кінець"
  loc <- T.pack <$> prompt "Нова локація"
  n <- updateEntity conn (Schedule (Just sid) sec w st en loc)
  sayChanged "Оновлено" n

deleteSchedule :: Connection -> IO ()
deleteSchedule conn = do
  sid <- promptInt "Вкажіть ID запису розкладу для видалення"
  ok <- confirm ("Видалити запис розкладу #" <> show sid <> "?")
  if ok
    then do n <- deleteEntity conn (Schedule (Just sid) 0 "" (TimeOfDay 0 0 0) (TimeOfDay 0 0 0) "")
            sayChanged "Видалено" n
    else putStrLn "Скасовано."

-- MEMBERSHIP
addMembership :: Connection -> IO ()
addMembership conn = do
  sid <- promptInt "Вкажіть ID студента"
  sc  <- promptInt "Вкажіть ID секції"
  jd  <- promptDay "Дата вступу"
  n <- insertEntity conn (Membership Nothing sid sc jd)
  sayChanged "Додано членств" n

viewMemberships :: Connection -> IO ()
viewMemberships conn = do
  rows <- query_ conn
    "SELECT m.membership_id, \
    \       (s.last_name || ' ' || s.first_name) AS student_name, \
    \       sec.name AS section_name, \
    \       m.joined_at \
    \FROM memberships m \
    \JOIN students   s   ON s.student_id = m.student_id \
    \JOIN sections   sec ON sec.section_id = m.section_id \
    \ORDER BY m.membership_id DESC"
      :: IO [(Int, Text, Text, Day)]
  withHeader "Членства (новіші зверху)" $ do
    sayEmptyIf (null rows)
    let headers = ["ID","Студент","Секція","Вступ"]
        rows'   = [ [ T.pack (show mid), sname, secname, T.pack (show d) ]
                  | (mid, sname, secname, d) <- rows
                  ]
    printTable headers rows'

updateMembership :: Connection -> IO ()
updateMembership conn = do
  mid <- promptInt "Вкажіть ID членства для оновлення"
  sid <- promptInt "Новий ID студента"
  sc  <- promptInt "Новий ID секції"
  jd  <- promptDay "Нова дата вступу"
  n <- updateEntity conn (Membership (Just mid) sid sc jd)
  sayChanged "Оновлено" n

deleteMembership :: Connection -> IO ()
deleteMembership conn = do
  mid <- promptInt "Вкажіть ID членства для видалення"
  ok <- confirm ("Видалити членство #" <> show mid <> "?")
  if ok
    then do n <- deleteEntity conn (Membership (Just mid) 0 0 dummyDay)
            sayChanged "Видалено" n
    else putStrLn "Скасовано."

-- COMPETITION
addCompetition :: Connection -> IO ()
addCompetition conn = do
  t  <- T.pack <$> prompt "Назва змагань"
  d  <- promptDay "Дата проведення"
  v  <- T.pack <$> prompt "Місце проведення"
  ms <- promptMaybeInt "ID секції (Enter = NULL)"
  n <- insertEntity conn (Competition Nothing t d v ms)
  sayChanged "Додано змагань" n

viewCompetitions :: Connection -> IO ()
viewCompetitions conn = do
  rows <- query_ conn
    "SELECT c.competition_id, c.title, c.held_on, c.venue, \
    \       COALESCE(sec.name, '—') AS section_name \
    \FROM competitions c \
    \LEFT JOIN sections sec ON sec.section_id = c.section_id \
    \ORDER BY c.held_on DESC"
      :: IO [(Int, Text, Day, Text, Text)]
  withHeader "Змагання (новіші зверху)" $ do
    sayEmptyIf (null rows)
    let headers = ["ID","Назва","Дата","Місце","Секція"]
        rows'   = [ [ T.pack (show cid), title, T.pack (show heldOn), venue, secName ]
                  | (cid, title, heldOn, venue, secName) <- rows
                  ]
    printTable headers rows'

updateCompetition :: Connection -> IO ()
updateCompetition conn = do
  cid <- promptInt "Вкажіть ID змагань для оновлення"
  t   <- T.pack <$> prompt "Нова назва"
  d   <- promptDay "Нова дата"
  v   <- T.pack <$> prompt "Нове місце"
  ms  <- promptMaybeInt "Новий ID секції (Enter = NULL)"
  n <- updateEntity conn (Competition (Just cid) t d v ms)
  sayChanged "Оновлено" n

deleteCompetition :: Connection -> IO ()
deleteCompetition conn = do
  cid <- promptInt "Вкажіть ID змагань для видалення"
  ok <- confirm ("Видалити змагання #" <> show cid <> "?")
  if ok
    then do n <- deleteEntity conn (Competition (Just cid) "" dummyDay "" Nothing)
            sayChanged "Видалено" n
    else putStrLn "Скасовано."

-- COMPETITION PARTICIPANTS
addCompetitionParticipant :: Connection -> IO ()
addCompetitionParticipant conn = do
  cid   <- promptInt "Вкажіть ID змагань"
  sid   <- promptInt "Вкажіть ID студента"
  notes <- T.pack <$> prompt "Примітки до результату (Enter = NULL)"
  let nVal = if T.null notes then Nothing else Just notes
  n <- insertEntity conn (CompetitionParticipant Nothing cid sid nVal)
  sayChanged "Додано учасників" n

viewCompetitionParticipants :: Connection -> IO ()
viewCompetitionParticipants conn = do
  rows <- query_ conn
    "SELECT cp.cp_id, \
    \       c.title AS competition_title, \
    \       (s.last_name || ' ' || s.first_name) AS student_name, \
    \       cp.result_notes \
    \FROM competition_participants cp \
    \JOIN competitions c ON c.competition_id = cp.competition_id \
    \JOIN students     s ON s.student_id     = cp.student_id \
    \ORDER BY cp.cp_id"
      :: IO [(Int, Text, Text, Maybe Text)]
  withHeader "Учасники змагань" $ do
    sayEmptyIf (null rows)
    let headers = ["ID","Змагання","Студент","Нотатки"]
        rows'   = [ [ T.pack (show cid), ctitle, sname, maybe "—" id notes ]
                  | (cid, ctitle, sname, notes) <- rows
                  ]
    printTable headers rows'

updateCompetitionParticipant :: Connection -> IO ()
updateCompetitionParticipant conn = do
  cpid  <- promptInt "Вкажіть ID учасника"
  cid   <- promptInt "Новий ID змагань"
  sid   <- promptInt "Новий ID студента"
  notes <- T.pack <$> prompt "Нові примітки (Enter = NULL)"
  let nVal = if T.null notes then Nothing else Just notes
  n <- updateEntity conn (CompetitionParticipant (Just cpid) cid sid nVal)
  sayChanged "Оновлено" n

deleteCompetitionParticipant :: Connection -> IO ()
deleteCompetitionParticipant conn = do
  cpid <- promptInt "Вкажіть ID учасника для видалення"
  ok <- confirm ("Видалити учасника #" <> show cpid <> "?")
  if ok
    then do n <- deleteEntity conn (CompetitionParticipant (Just cpid) 0 0 Nothing)
            sayChanged "Видалено" n
    else putStrLn "Скасовано."

showCompetitionParticipants :: Connection -> IO ()
showCompetitionParticipants conn = do
  cid <- promptInt "Вкажіть ID змагань"
  rows <- listCompetitionParticipants conn cid
  withHeader ("Учасники змагання · ID = " <> T.pack (show cid)) $ do
    sayEmptyIf (null rows)
    mapM_ (\(title, student) -> TIO.putStrLn (title <> " ← " <> student)) rows

--------------------------------------------------------------------------------
-- CLI-МЕНЮ
--------------------------------------------------------------------------------

main :: IO ()
main = do
  res <- try $ bracket createConn close $ \conn -> do
    ansi <- hSupportsANSI stdout
    when ansi (setSGR [SetColor Foreground Vivid Green, SetConsoleIntensity BoldIntensity])
    putStrLn "Спорт на факультеті"
    when ansi (setSGR [Reset])
    runMigrations conn
    loop conn
  case (res :: Either SomeException ()) of
    Left e  -> putStrLn $ "Помилка БД/IO: " ++ show e
    Right _ -> pure ()

loop :: Connection -> IO ()
loop conn = do
  putStrLn "\nМеню:"
  putStrLn " 1) Додати студента             2) Переглянути студентів"
  putStrLn " 3) Додати викладача            4) Переглянути викладачів"
  putStrLn " 5) Додати секцію               6) Переглянути секції"
  putStrLn " 7) Додати розклад              8) Переглянути розклад"
  putStrLn " 9) Додати членство            10) Переглянути членства"
  putStrLn "11) Додати змагання            12) Переглянути змагання"
  putStrLn "13) Оновити студента           14) Видалити студента"
  putStrLn "15) Оновити викладача          16) Видалити викладача"
  putStrLn "17) Оновити секцію             18) Видалити секцію"
  putStrLn "19) Оновити розклад            20) Видалити розклад"
  putStrLn "21) Оновити членство           22) Видалити членство"
  putStrLn "23) Оновити змагання           24) Видалити змагання"
  putStrLn "25) Показати студент → секції"
  putStrLn "26) Показати розклад секції (за ID секції)"
  putStrLn "27) Додати учасника змагань    28) Переглянути учасників"
  putStrLn "29) Оновити учасника           30) Видалити учасника"
  putStrLn "31) Показати учасників змагання (за ID змагання)"
  putStrLn "0)  Вихід"
  putStr   "Ваш вибір: " >> hFlush stdout
  ch <- getLine
  case ch of
    "1"  -> addStudent conn >> loop conn
    "2"  -> viewStudents conn >> loop conn
    "3"  -> addInstructor conn >> loop conn
    "4"  -> viewInstructors conn >> loop conn
    "5"  -> addSection conn >> loop conn
    "6"  -> viewSections conn >> loop conn
    "7"  -> addSchedule conn >> loop conn
    "8"  -> viewSchedules conn >> loop conn
    "9"  -> addMembership conn >> loop conn
    "10" -> viewMemberships conn >> loop conn
    "11" -> addCompetition conn >> loop conn
    "12" -> viewCompetitions conn >> loop conn
    "13" -> updateStudent conn >> loop conn
    "14" -> deleteStudent conn >> loop conn
    "15" -> updateInstructor conn >> loop conn
    "16" -> deleteInstructor conn >> loop conn
    "17" -> updateSection conn >> loop conn
    "18" -> deleteSection conn >> loop conn
    "19" -> updateSchedule conn >> loop conn
    "20" -> deleteSchedule conn >> loop conn
    "21" -> updateMembership conn >> loop conn
    "22" -> deleteMembership conn >> loop conn
    "23" -> updateCompetition conn >> loop conn
    "24" -> deleteCompetition conn >> loop conn
    "25" -> do
              pairs <- listStudentSections conn
              withHeader "Студент → Секції" $ do
                sayEmptyIf (null pairs)
                let headers = ["Студент","Секція"]
                    rows    = [ [s, sec] | (s, sec) <- pairs ]
                printTable headers rows
              loop conn
    "26" -> do
              sid <- promptInt "Вкажіть ID секції"
              rows <- getSectionSchedule conn sid
              withHeader ("Розклад секції #" <> T.pack (show sid)) $ do
                sayEmptyIf (null rows)
                let headers = ["День","Початок","Кінець"]
                    rows'   = [ [w, st, en] | (w,st,en) <- rows ]
                printTable headers rows'
              loop conn
    "27" -> addCompetitionParticipant conn >> loop conn
    "28" -> viewCompetitionParticipants conn >> loop conn
    "29" -> updateCompetitionParticipant conn >> loop conn
    "30" -> deleteCompetitionParticipant conn >> loop conn
    "31" -> showCompetitionParticipants conn >> loop conn
    "0"  -> putStrLn "До побачення!"
    _    -> putStrLn "Невірний вибір (тисни 0–31)." >> loop conn
