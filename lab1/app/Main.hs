{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Control.Monad (void, when)
import           Data.Int (Int64)
import           Data.Maybe (fromMaybe)
import           Data.Time (Day, defaultTimeLocale, parseTimeM)
import           Data.Time.LocalTime (TimeOfDay(..))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString as SBS
import           GHC.Generics (Generic)

-- Env/IO
import           System.IO (hFlush, stdout)
import           System.Directory (doesFileExist)
import           System.Environment (lookupEnv)
import           Data.Word (Word16)
import           Text.Read (readMaybe)

-- PostgreSQL
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToRow
import           Database.PostgreSQL.Simple.ToField (ToField, toField)
import           Database.PostgreSQL.Simple.Transaction (withTransaction)
import           Database.PostgreSQL.Simple.Types (Query(..))

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

instance Persist Student where
  type Key Student = Int
  tableName _     = "students"
  insertSQL _     = "INSERT INTO students (first_name,last_name,group_code,birth_date,phone) VALUES (?,?,?,?,?)"
  updateSQL _     = "UPDATE students SET first_name=?, last_name=?, group_code=?, birth_date=?, phone=? WHERE student_id=?"
  selectAllSQL _  = "SELECT student_id, first_name, last_name, group_code, birth_date, phone FROM students ORDER BY last_name, first_name"
  deleteSQL _     = "DELETE FROM students WHERE student_id=?"
  getKey          = stId

instance Persist Instructor where
  type Key Instructor = Int
  tableName _     = "instructors"
  insertSQL _     = "INSERT INTO instructors (first_name,last_name,department,phone) VALUES (?,?,?,?)"
  updateSQL _     = "UPDATE instructors SET first_name=?, last_name=?, department=?, phone=? WHERE instructor_id=?"
  selectAllSQL _  = "SELECT instructor_id, first_name, last_name, department, phone FROM instructors ORDER BY last_name, first_name"
  deleteSQL _     = "DELETE FROM instructors WHERE instructor_id=?"
  getKey          = insId

instance Persist Section where
  type Key Section = Int
  tableName _     = "sections"
  insertSQL _     = "INSERT INTO sections (name, level, instructor_id) VALUES (?,?,?)"
  updateSQL _     = "UPDATE sections SET name=?, level=?, instructor_id=? WHERE section_id=?"
  selectAllSQL _  = "SELECT section_id, name, level, instructor_id FROM sections ORDER BY name"
  deleteSQL _     = "DELETE FROM sections WHERE section_id=?"
  getKey          = secId

instance Persist Schedule where
  type Key Schedule = Int
  tableName _     = "section_schedule"
  insertSQL _     = "INSERT INTO section_schedule (section_id, weekday, start_time, end_time, location) VALUES (?,?,?,?,?)"
  updateSQL _     = "UPDATE section_schedule SET section_id=?, weekday=?, start_time=?, end_time=?, location=? WHERE schedule_id=?"
  selectAllSQL _  =
    "SELECT schedule_id, section_id, weekday, start_time, end_time, location \
    \FROM section_schedule \
    \ORDER BY section_id, \
    \CASE weekday WHEN 'Mon' THEN 1 WHEN 'Tue' THEN 2 WHEN 'Wed' THEN 3 \
    \WHEN 'Thu' THEN 4 WHEN 'Fri' THEN 5 WHEN 'Sat' THEN 6 WHEN 'Sun' THEN 7 END, \
    \start_time"
  deleteSQL _     = "DELETE FROM section_schedule WHERE schedule_id=?"
  getKey          = schId

instance Persist Membership where
  type Key Membership = Int
  tableName _     = "memberships"
  insertSQL _     = "INSERT INTO memberships (student_id, section_id, joined_at) VALUES (?,?,?)"
  updateSQL _     = "UPDATE memberships SET student_id=?, section_id=?, joined_at=? WHERE membership_id=?"
  selectAllSQL _  = "SELECT membership_id, student_id, section_id, joined_at FROM memberships ORDER BY joined_at DESC"
  deleteSQL _     = "DELETE FROM memberships WHERE membership_id=?"
  getKey          = memId

instance Persist Competition where
  type Key Competition = Int
  tableName _     = "competitions"
  insertSQL _     = "INSERT INTO competitions (title, held_on, venue, section_id) VALUES (?,?,?,?)"
  updateSQL _     = "UPDATE competitions SET title=?, held_on=?, venue=?, section_id=? WHERE competition_id=?"
  selectAllSQL _  = "SELECT competition_id, title, held_on, venue, section_id FROM competitions ORDER BY held_on DESC"
  deleteSQL _     = "DELETE FROM competitions WHERE competition_id=?"
  getKey          = cmpId

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
-- СПЕЦІАЛЬНІ ЗАПИТИ (JOIN-и)
--------------------------------------------------------------------------------

-- 1) Студент -> секції
listStudentSections :: Connection -> IO [(Text, Text)]
listStudentSections conn = query_ conn
  "SELECT (s.first_name || ' ' || s.last_name) AS student, sec.name \
  \FROM memberships m \
  \JOIN students s ON s.student_id=m.student_id \
  \JOIN sections sec ON sec.section_id=m.section_id \
  \ORDER BY student, sec.name"

-- 2) Розклад конкретної секції (Mon..Sun)
getSectionSchedule :: Connection -> Int -> IO [(Text, Text, Text)]
getSectionSchedule conn sid = query conn
  "SELECT weekday::text, to_char(start_time,'HH24:MI'), to_char(end_time,'HH24:MI') \
  \FROM section_schedule \
  \WHERE section_id=? \
  \ORDER BY CASE weekday WHEN 'Mon' THEN 1 WHEN 'Tue' THEN 2 WHEN 'Wed' THEN 3 \
  \WHEN 'Thu' THEN 4 WHEN 'Fri' THEN 5 WHEN 'Sat' THEN 6 WHEN 'Sun' THEN 7 END, start_time"
  (Only sid)

-- 3) Учасники змагання
listCompetitionParticipants :: Connection -> Int -> IO [(Text, Text)]
listCompetitionParticipants conn cid = query conn
  "SELECT c.title, (s.first_name || ' ' || s.last_name) AS student \
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
  ok <- doesFileExist "db.sql"
  when ok $ do
      sql <- SBS.readFile "db.sql"
      withTransaction conn $ void (execute_ conn (Query sql))

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
  host <- readEnv "PGHOST"     "127.0.0.1"
  port <- readEnvPort "PGPORT" 5432
  user <- readEnv "PGUSER"     "$(whoami)"
  pass <- readEnv "PGPASSWORD" "password"
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
promptLevel = promptFromOptionsCI "Рівень" ["beginner","intermediate","advanced"]

promptWeekday :: IO Text
promptWeekday = promptFromOptionsCI "День тижня" ["Mon","Tue","Wed","Thu","Fri","Sat","Sun"]

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

-- STUDENT
addStudent :: Connection -> IO ()
addStudent conn = do
  f  <- T.pack <$> prompt "Ім'я"
  l  <- T.pack <$> prompt "Прізвище"
  g  <- T.pack <$> prompt "Група (напр. КН-21)"
  b  <- promptDay "Дата народження"
  pS <- prompt "Телефон (необов'язково, Enter=порожньо)"
  let p = if null pS then Nothing else Just (T.pack pS)
  let st = Student Nothing f l g b p
  n <- insertEntity conn st
  putStrLn $ "Додано студентів: " ++ show n

viewStudents :: Connection -> IO ()
viewStudents conn = do
  xs <- selectAll conn (Student Nothing "" "" "" (read "2000-01-01") Nothing)
  mapM_ print xs

updateStudent :: Connection -> IO ()
updateStudent conn = do
  sid <- promptInt "ID студента для оновлення"
  f   <- T.pack <$> prompt "Нове ім'я"
  l   <- T.pack <$> prompt "Нове прізвище"
  g   <- T.pack <$> prompt "Нова група"
  b   <- promptDay "Нова дата народження"
  pS  <- prompt "Новий телефон (Enter=порожньо)"
  let p = if null pS then Nothing else Just (T.pack pS)
  n <- updateEntity conn (Student (Just sid) f l g b p)
  putStrLn $ "Оновлено записів: " ++ show n

deleteStudent :: Connection -> IO ()
deleteStudent conn = do
  sid <- promptInt "ID студента для видалення"
  n <- deleteEntity conn (Student (Just sid) "" "" "" (read "2000-01-01") Nothing)
  putStrLn $ "Видалено записів: " ++ show n

-- INSTRUCTOR
addInstructor :: Connection -> IO ()
addInstructor conn = do
  f  <- T.pack <$> prompt "Ім'я викладача"
  l  <- T.pack <$> prompt "Прізвище викладача"
  d  <- T.pack <$> prompt "Кафедра/відділ"
  pS <- prompt "Телефон (Enter=порожньо)"
  let p = if null pS then Nothing else Just (T.pack pS)
  n <- insertEntity conn (Instructor Nothing f l d p)
  putStrLn $ "Додано викладачів: " ++ show n

viewInstructors :: Connection -> IO ()
viewInstructors conn = do
  xs <- selectAll conn (Instructor Nothing "" "" "" Nothing)
  mapM_ print xs

updateInstructor :: Connection -> IO ()
updateInstructor conn = do
  iid <- promptInt "ID викладача для оновлення"
  f   <- T.pack <$> prompt "Нове ім'я"
  l   <- T.pack <$> prompt "Нове прізвище"
  d   <- T.pack <$> prompt "Нова кафедра/відділ"
  pS  <- prompt "Новий телефон (Enter=порожньо)"
  let p = if null pS then Nothing else Just (T.pack pS)
  n <- updateEntity conn (Instructor (Just iid) f l d p)
  putStrLn $ "Оновлено: " ++ show n

deleteInstructor :: Connection -> IO ()
deleteInstructor conn = do
  iid <- promptInt "ID викладача для видалення"
  n <- deleteEntity conn (Instructor (Just iid) "" "" "" Nothing)
  putStrLn $ "Видалено: " ++ show n

-- SECTION
addSection :: Connection -> IO ()
addSection conn = do
  n   <- T.pack <$> prompt "Назва секції (напр. Basketball)"
  lvl <- promptLevel
  mid <- promptMaybeInt "ID інструктора"
  nIns <- insertEntity conn (Section Nothing n lvl mid)
  putStrLn $ "Додано секцій: " ++ show nIns

viewSections :: Connection -> IO ()
viewSections conn = do
  xs <- selectAll conn (Section Nothing "" "" Nothing)
  mapM_ print xs

updateSection :: Connection -> IO ()
updateSection conn = do
  sid <- promptInt "ID секції для оновлення"
  n   <- T.pack <$> prompt "Нова назва"
  lvl <- promptLevel
  mid <- promptMaybeInt "Новий ID інструктора"
  let s = Section (Just sid) n lvl mid
  n' <- updateEntity conn s
  putStrLn $ "Оновлено: " ++ show n'

deleteSection :: Connection -> IO ()
deleteSection conn = do
  sid <- promptInt "ID секції для видалення"
  n <- deleteEntity conn (Section (Just sid) "" "" Nothing)
  putStrLn $ "Видалено: " ++ show n

-- SCHEDULE
addSchedule :: Connection -> IO ()
addSchedule conn = do
  sec  <- promptInt "section_id"
  w    <- promptWeekday
  st   <- readTimeOfDay "Початок (start_time)"
  en   <- readTimeOfDay "Кінець (end_time)"
  loc  <- T.pack <$> prompt "Локація"
  n <- insertEntity conn (Schedule Nothing sec w st en loc)
  putStrLn $ "Додано рядків розкладу: " ++ show n

viewSchedules :: Connection -> IO ()
viewSchedules conn = do
  xs <- selectAll conn (Schedule Nothing 0 "" (TimeOfDay 0 0 0) (TimeOfDay 0 0 0) "")
  mapM_ print xs

updateSchedule :: Connection -> IO ()
updateSchedule conn = do
  sid <- promptInt "ID запису розкладу для оновлення"
  sec <- promptInt "Новий section_id"
  w   <- promptWeekday
  st  <- readTimeOfDay "Новий початок (start_time)"
  en  <- readTimeOfDay "Новий кінець (end_time)"
  loc <- T.pack <$> prompt "Нова локація"
  n <- updateEntity conn (Schedule (Just sid) sec w st en loc)
  putStrLn $ "Оновлено: " ++ show n

deleteSchedule :: Connection -> IO ()
deleteSchedule conn = do
  sid <- promptInt "ID запису розкладу для видалення"
  n <- deleteEntity conn (Schedule (Just sid) 0 "" (TimeOfDay 0 0 0) (TimeOfDay 0 0 0) "")
  putStrLn $ "Видалено: " ++ show n

-- MEMBERSHIP
addMembership :: Connection -> IO ()
addMembership conn = do
  sid <- promptInt "student_id"
  sc  <- promptInt "section_id"
  jd  <- promptDay "Дата вступу"
  n <- insertEntity conn (Membership Nothing sid sc jd)
  putStrLn $ "Додано членств: " ++ show n

viewMemberships :: Connection -> IO ()
viewMemberships conn = do
  xs <- selectAll conn (Membership Nothing 0 0 (read "2000-01-01"))
  mapM_ print xs

updateMembership :: Connection -> IO ()
updateMembership conn = do
  mid <- promptInt "ID членства для оновлення"
  sid <- promptInt "Новий student_id"
  sc  <- promptInt "Новий section_id"
  jd  <- promptDay "Нова дата вступу"
  n <- updateEntity conn (Membership (Just mid) sid sc jd)
  putStrLn $ "Оновлено: " ++ show n

deleteMembership :: Connection -> IO ()
deleteMembership conn = do
  mid <- promptInt "ID членства для видалення"
  n <- deleteEntity conn (Membership (Just mid) 0 0 (read "2000-01-01"))
  putStrLn $ "Видалено: " ++ show n

-- COMPETITION
addCompetition :: Connection -> IO ()
addCompetition conn = do
  t  <- T.pack <$> prompt "Назва змагань"
  d  <- promptDay "Дата проведення"
  v  <- T.pack <$> prompt "Місце проведення"
  ms <- promptMaybeInt "section_id"
  n <- insertEntity conn (Competition Nothing t d v ms)
  putStrLn $ "Додано змагань: " ++ show n

viewCompetitions :: Connection -> IO ()
viewCompetitions conn = do
  xs <- selectAll conn (Competition Nothing "" (read "2000-01-01") "" Nothing)
  mapM_ print xs

updateCompetition :: Connection -> IO ()
updateCompetition conn = do
  cid <- promptInt "ID змагань для оновлення"
  t   <- T.pack <$> prompt "Нова назва"
  d   <- promptDay "Нова дата"
  v   <- T.pack <$> prompt "Нове місце"
  ms  <- promptMaybeInt "Новий section_id"
  n <- updateEntity conn (Competition (Just cid) t d v ms)
  putStrLn $ "Оновлено: " ++ show n

deleteCompetition :: Connection -> IO ()
deleteCompetition conn = do
  cid <- promptInt "ID змагань для видалення"
  n <- deleteEntity conn (Competition (Just cid) "" (read "2000-01-01") "" Nothing)
  putStrLn $ "Видалено: " ++ show n

-- COMPETITION PARTICIPANTS
addCompetitionParticipant :: Connection -> IO ()
addCompetitionParticipant conn = do
  cid   <- promptInt "competition_id"
  sid   <- promptInt "student_id"
  notes <- T.pack <$> prompt "Примітки до результату (Enter=порожньо)"
  let nVal = if T.null notes then Nothing else Just notes
  n <- insertEntity conn (CompetitionParticipant Nothing cid sid nVal)
  putStrLn $ "Додано учасників: " ++ show n

viewCompetitionParticipants :: Connection -> IO ()
viewCompetitionParticipants conn = do
  xs <- selectAll conn (CompetitionParticipant Nothing 0 0 Nothing)
  mapM_ print xs

updateCompetitionParticipant :: Connection -> IO ()
updateCompetitionParticipant conn = do
  cpid  <- promptInt "ID (cp_id) учасника для оновлення"
  cid   <- promptInt "Новий competition_id"
  sid   <- promptInt "Новий student_id"
  notes <- T.pack <$> prompt "Нові примітки (Enter=порожньо)"
  let nVal = if T.null notes then Nothing else Just notes
  n <- updateEntity conn (CompetitionParticipant (Just cpid) cid sid nVal)
  putStrLn $ "Оновлено: " ++ show n

deleteCompetitionParticipant :: Connection -> IO ()
deleteCompetitionParticipant conn = do
  cpid <- promptInt "ID (cp_id) учасника для видалення"
  n <- deleteEntity conn (CompetitionParticipant (Just cpid) 0 0 Nothing)
  putStrLn $ "Видалено: " ++ show n

showCompetitionParticipantsPretty :: Connection -> IO ()
showCompetitionParticipantsPretty conn = do
  cid <- promptInt "competition_id"
  rows <- listCompetitionParticipants conn cid
  mapM_ (\(title, student) ->
          putStrLn (T.unpack title ++ " <- " ++ T.unpack student)) rows

--------------------------------------------------------------------------------
-- CLI-МЕНЮ
--------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "Спорт на факультеті"
  conn <- createConn
  runMigrations conn
  loop conn

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
  putStrLn "25) Показати студент -> секції"
  putStrLn "26) Показати розклад секції (ID)"
  putStrLn "27) Додати учасника змагань    28) Переглянути учасників"
  putStrLn "29) Оновити учасника           30) Видалити учасника"
  putStrLn "31) Показати учасників змагання (за competition_id)"
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
              mapM_ (\(s,sec) -> putStrLn (T.unpack s ++ " -> " ++ T.unpack sec)) pairs
              loop conn
    "26" -> do
              sid <- promptInt "section_id"
              rows <- getSectionSchedule conn sid
              mapM_ (\(w,st,en) -> putStrLn (T.unpack w ++ " " ++ T.unpack st ++ "-" ++ T.unpack en)) rows
              loop conn
    "27" -> addCompetitionParticipant conn >> loop conn
    "28" -> viewCompetitionParticipants conn >> loop conn
    "29" -> updateCompetitionParticipant conn >> loop conn
    "30" -> deleteCompetitionParticipant conn >> loop conn
    "31" -> showCompetitionParticipantsPretty conn >> loop conn
    "0"  -> putStrLn "До побачення!"
    _    -> putStrLn "Невірний вибір" >> loop conn
