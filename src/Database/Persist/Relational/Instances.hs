{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database.Persist.Relational.Instances
       where

import Database.Persist.Relational.TH
import Database.Record
import Database.Record.Persistable (unsafePersistableSqlTypeFromNull)
import Database.Persist

instance PersistableType PersistValue  where
    persistableType = unsafePersistableSqlTypeFromNull PersistNull

derivePersistableInstancesFromPersistFieldInstances ["SomePersistField"]
