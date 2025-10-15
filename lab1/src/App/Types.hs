{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module App.Types (Student(..), Instructor(..), Section(..),
                 Schedule(..), Membership(..), Competition(..),
                 CompetitionParticipant(..), Persist(..)) where

import           Data.Time (Day)
import           Data.Text (Text)
import           Data.Time.LocalTime (TimeOfDay(..))
import           GHC.Generics (Generic)

-- PostgreSQL
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToRow
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
  } deriving (Show, Eq, Generic)

instance FromRow Student where
  fromRow = Student <$> (Just <$> field) <*> field <*> field <*> field <*> field <*> field

instance ToRow Student where
  toRow (Student _ f l g b p) = toRow (f, l, g, b, p)

data Instructor = Instructor
  { insId        :: Maybe Int
  , insFirstName :: Text
  , insLastName  :: Text
  , insDept      :: Text
  , insPhone     :: Maybe Text
  } deriving (Show, Eq, Generic)

instance FromRow Instructor where
  fromRow = Instructor <$> (Just <$> field) <*> field <*> field <*> field <*> field

instance ToRow Instructor where
  toRow (Instructor _ f l d p) = toRow (f, l, d, p)

data Section = Section
  { secId         :: Maybe Int
  , secName       :: Text
  , secLevel      :: Text
  , secInstructor :: Maybe Int
  } deriving (Show, Eq, Generic)

instance FromRow Section where
  fromRow = Section <$> (Just <$> field) <*> field <*> field <*> field

instance ToRow Section where
  toRow (Section _ n lvl mid) = toRow (n, lvl, mid)

data Schedule = Schedule
  { schId      :: Maybe Int
  , schSection :: Int
  , schWeekday :: Text
  , schStart   :: TimeOfDay
  , schEnd     :: TimeOfDay
  , schLoc     :: Text
  } deriving (Show, Eq, Generic)

instance FromRow Schedule where
  fromRow = Schedule <$> (Just <$> field) <*> field <*> field <*> field <*> field <*> field

instance ToRow Schedule where
  toRow (Schedule _ s w st en loc) = toRow (s,w,st,en,loc)

data Membership = Membership
  { memId      :: Maybe Int
  , memStudent :: Int
  , memSection :: Int
  , memJoined  :: Day
  } deriving (Show, Eq, Generic)

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
  } deriving (Show, Eq, Generic)

instance FromRow Competition where
  fromRow = Competition <$> (Just <$> field) <*> field <*> field <*> field <*> field

instance ToRow Competition where
  toRow (Competition _ t d v ms) = toRow (t,d,v,ms)

data CompetitionParticipant = CompetitionParticipant
  { cpId          :: Maybe Int
  , cpCompetition :: Int
  , cpStudent     :: Int
  , cpResultNotes :: Maybe Text
  } deriving (Show, Eq, Generic)

instance FromRow CompetitionParticipant where
  fromRow = CompetitionParticipant <$> (Just <$> field) <*> field <*> field <*> field

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
