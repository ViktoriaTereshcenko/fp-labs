{-# LANGUAGE OverloadedStrings #-}

module App.Queries (listStudentSections, getSectionSchedule,
                   listCompetitionParticipants) where

import           Data.Text (Text)

-- PostgreSQL
import           Database.PostgreSQL.Simple (Connection, query, query_, Only(..))

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
