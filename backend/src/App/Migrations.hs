{-# LANGUAGE OverloadedStrings #-}
module App.Migrations
  ( runMigrations
  ) where

import App.Models (migrateAll)
import Database.Persist.Sql (SqlPersistT, runMigration)

runMigrations :: SqlPersistT IO ()
runMigrations = runMigration migrateAll
