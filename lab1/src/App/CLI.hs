{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module App.CLI (loop) where

import           Control.Monad (when)
import           Data.Int (Int64)
import           Data.Time (Day)
import           Data.Time.Calendar (fromGregorian)
import qualified Data.Time as Data.Time
import           Data.Time.LocalTime (TimeOfDay(..))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- Інші файли проєкта
import           App.Types
import           App.UI
import           App.Queries

-- Env/IO
import           System.IO (hFlush, stdout)
import           Text.Read (readMaybe)

-- ANSI (кольори/стиль)
import           System.Console.ANSI (hSupportsANSI, setSGR,
                 SGR(..), ColorIntensity(..), ConsoleIntensity(..),
                 ConsoleLayer(..), Color(..))

-- PostgreSQL
import           Database.PostgreSQL.Simple (Connection, execute, query, query_, Only(..))
import           Database.PostgreSQL.Simple.FromRow (FromRow)
import           Database.PostgreSQL.Simple.ToRow   (ToRow, toRow)
import           Database.PostgreSQL.Simple.ToField (ToField, toField)

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
  case parse s of
    Just d  -> pure d
    Nothing -> putStrLn "Невірна дата. Приклад: 2024-09-30" >> promptDay label
  where
    parse = Data.Time.parseTimeM True Data.Time.defaultTimeLocale "%F"

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
  sayChanged "Додано студентів" (fromIntegral n)

viewStudents :: Connection -> IO ()
viewStudents conn = do
  xs <- selectAll conn (Student Nothing "" "" "" dummyDay Nothing)
  withHeader "Студенти (відсортовано за прізвищем, ім’ям)" $ do
    sayEmptyIf (null xs)
    let headers = ["ID","ПІБ","Група","Нар.","Тел"]
        rows    = map (\s -> [ maybe "-" (T.pack . show) (stId s)
                             , stLastName s <> " " <> stFirstName s
                             , stGroup s
                             , fmtDay (stBirth s)
                             , tMaybe (stPhone s)
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
  sayChanged "Оновлено записів" (fromIntegral n)

deleteStudent :: Connection -> IO ()
deleteStudent conn = do
  sid <- promptInt "Вкажіть ID студента для видалення"
  ok <- confirm ("Видалити студента #" <> show sid <> "?")
  if ok
    then do n <- deleteEntity conn (Student (Just sid) "" "" "" dummyDay Nothing)
            sayChanged "Видалено записів" (fromIntegral n)
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
  sayChanged "Додано викладачів" (fromIntegral n)

viewInstructors :: Connection -> IO ()
viewInstructors conn = do
  xs <- selectAll conn (Instructor Nothing "" "" "" Nothing)
  withHeader "Викладачі (відсортовано за прізвищем, ім’ям)" $ do
    sayEmptyIf (null xs)
    let headers = ["ID","ПІБ","Кафедра","Тел"]
        rows    = map (\i -> [ maybe "-" (T.pack . show) (insId i)
                             , insLastName i <> " " <> insFirstName i
                             , insDept i
                             , tMaybe (insPhone i)
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
  sayChanged "Оновлено" (fromIntegral n)

deleteInstructor :: Connection -> IO ()
deleteInstructor conn = do
  iid <- promptInt "Вкажіть ID викладача для видалення"
  ok <- confirm ("Видалити викладача #" <> show iid <> "?")
  if ok
    then do n <- deleteEntity conn (Instructor (Just iid) "" "" "" Nothing)
            sayChanged "Видалено" (fromIntegral n)
    else putStrLn "Скасовано."

-- SECTION
addSection :: Connection -> IO ()
addSection conn = do
  n   <- T.pack <$> prompt "Назва секції (напр. Баскетбол)"
  lvl <- promptLevel
  mid <- promptMaybeInt "Вкажіть ID викладача (Enter = NULL)"
  nIns <- insertEntity conn (Section Nothing n lvl mid)
  sayChanged "Додано секцій" (fromIntegral nIns)

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
  sayChanged "Оновлено" (fromIntegral n')

deleteSection :: Connection -> IO ()
deleteSection conn = do
  sid <- promptInt "Вкажіть ID секції для видалення"
  ok <- confirm ("Видалити секцію #" <> show sid <> "?")
  if ok
    then do n <- deleteEntity conn (Section (Just sid) "" "" Nothing)
            sayChanged "Видалено" (fromIntegral n)
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
  sayChanged "Додано рядків розкладу" (fromIntegral n)

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
  sayChanged "Оновлено" (fromIntegral n)

deleteSchedule :: Connection -> IO ()
deleteSchedule conn = do
  sid <- promptInt "Вкажіть ID запису розкладу для видалення"
  ok <- confirm ("Видалити запис розкладу #" <> show sid <> "?")
  if ok
    then do n <- deleteEntity conn (Schedule (Just sid) 0 "" (TimeOfDay 0 0 0) (TimeOfDay 0 0 0) "")
            sayChanged "Видалено" (fromIntegral n)
    else putStrLn "Скасовано."

-- MEMBERSHIP
addMembership :: Connection -> IO ()
addMembership conn = do
  sid <- promptInt "Вкажіть ID студента"
  sc  <- promptInt "Вкажіть ID секції"
  jd  <- promptDay "Дата вступу"
  n <- insertEntity conn (Membership Nothing sid sc jd)
  sayChanged "Додано членств" (fromIntegral n)

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
        rows'   = [ [ T.pack (show mid), sname, secname, fmtDay d ]
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
  sayChanged "Оновлено" (fromIntegral n)

deleteMembership :: Connection -> IO ()
deleteMembership conn = do
  mid <- promptInt "Вкажіть ID членства для видалення"
  ok <- confirm ("Видалити членство #" <> show mid <> "?")
  if ok
    then do n <- deleteEntity conn (Membership (Just mid) 0 0 dummyDay)
            sayChanged "Видалено" (fromIntegral n)
    else putStrLn "Скасовано."

-- COMPETITION
addCompetition :: Connection -> IO ()
addCompetition conn = do
  t  <- T.pack <$> prompt "Назва змагань"
  d  <- promptDay "Дата проведення"
  v  <- T.pack <$> prompt "Місце проведення"
  ms <- promptMaybeInt "ID секції (Enter = NULL)"
  n <- insertEntity conn (Competition Nothing t d v ms)
  sayChanged "Додано змагань" (fromIntegral n)

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
        rows'   = [ [ T.pack (show cid), title, fmtDay heldOn, venue, secName ]
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
  sayChanged "Оновлено" (fromIntegral n)

deleteCompetition :: Connection -> IO ()
deleteCompetition conn = do
  cid <- promptInt "Вкажіть ID змагань для видалення"
  ok <- confirm ("Видалити змагання #" <> show cid <> "?")
  if ok
    then do n <- deleteEntity conn (Competition (Just cid) "" dummyDay "" Nothing)
            sayChanged "Видалено" (fromIntegral n)
    else putStrLn "Скасовано."

-- COMPETITION PARTICIPANTS
addCompetitionParticipant :: Connection -> IO ()
addCompetitionParticipant conn = do
  cid   <- promptInt "Вкажіть ID змагань"
  sid   <- promptInt "Вкажіть ID студента"
  notes <- T.pack <$> prompt "Примітки до результату (Enter = NULL)"
  let nVal = if T.null notes then Nothing else Just notes
  n <- insertEntity conn (CompetitionParticipant Nothing cid sid nVal)
  sayChanged "Додано учасників" (fromIntegral n)

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
        rows'   = [ [ T.pack (show cid), ctitle, sname, tMaybe notes ]
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
  sayChanged "Оновлено" (fromIntegral n)

deleteCompetitionParticipant :: Connection -> IO ()
deleteCompetitionParticipant conn = do
  cpid <- promptInt "Вкажіть ID учасника для видалення"
  ok <- confirm ("Видалити учасника #" <> show cpid <> "?")
  if ok
    then do n <- deleteEntity conn (CompetitionParticipant (Just cpid) 0 0 Nothing)
            sayChanged "Видалено" (fromIntegral n)
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

loop :: Connection -> IO ()
loop conn = do
  ansi <- hSupportsANSI stdout
  when ansi (setSGR [SetColor Foreground Vivid Green, SetConsoleIntensity BoldIntensity])
  putStrLn "\nМеню:"
  when ansi (setSGR [Reset])

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

  ansi <- hSupportsANSI stdout
  when ansi (setSGR [SetColor Foreground Vivid Green, SetConsoleIntensity BoldIntensity])
  putStr   "Ваш вибір: " >> hFlush stdout
  when ansi (setSGR [Reset])
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
