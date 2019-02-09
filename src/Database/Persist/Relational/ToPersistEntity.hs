{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Persist.Relational.ToPersistEntity
    ( ToPersistEntity(..)
    ) where

import Database.Record.FromSql (RecordFromSql, recordFromSql)
import Database.Persist (PersistValue)

class ToPersistEntity a b | a -> b, b -> a where
    recordFromSql' :: RecordFromSql PersistValue b

instance ToPersistEntity () () where
    recordFromSql' = recordFromSql

instance ( ToPersistEntity a pa
         , ToPersistEntity b pb
         ) =>  ToPersistEntity (a, b) (pa, pb) where
    recordFromSql' = (,) <$> r <*> r

instance ( ToPersistEntity a pa
         , ToPersistEntity b pb
         , ToPersistEntity c pc
         ) =>  ToPersistEntity (a, b, c) (pa, pb, pc) where
    recordFromSql' = (,,) <$> r <*> r <*> r

instance ( ToPersistEntity a pa
         , ToPersistEntity b pb
         , ToPersistEntity c pc
         , ToPersistEntity d pd
         ) =>  ToPersistEntity (a, b, c, d) (pa, pb, pc, pd) where
    recordFromSql' = (,,,) <$> r <*> r <*> r <*> r

instance ( ToPersistEntity a pa
         , ToPersistEntity b pb
         , ToPersistEntity c pc
         , ToPersistEntity d pd
         , ToPersistEntity e pe
         ) =>  ToPersistEntity (a, b, c, d, e) (pa, pb, pc, pd, pe) where
    recordFromSql' = (,,,,) <$> r <*> r <*> r <*> r <*> r

instance ( ToPersistEntity a pa
         , ToPersistEntity b pb
         , ToPersistEntity c pc
         , ToPersistEntity d pd
         , ToPersistEntity e pe
         , ToPersistEntity f pf
         ) =>  ToPersistEntity (a, b, c, d, e, f) (pa, pb, pc, pd, pe, pf) where
    recordFromSql' = (,,,,,) <$> r <*> r <*> r <*> r <*> r <*> r

instance ( ToPersistEntity a pa
         , ToPersistEntity b pb
         , ToPersistEntity c pc
         , ToPersistEntity d pd
         , ToPersistEntity e pe
         , ToPersistEntity f pf
         , ToPersistEntity g pg
         ) =>  ToPersistEntity (a, b, c, d, e, f, g) (pa, pb, pc, pd, pe, pf, pg) where
    recordFromSql' = (,,,,,,) <$> r <*> r <*> r <*> r <*> r <*> r <*> r

instance ( ToPersistEntity a pa
         , ToPersistEntity b pb
         , ToPersistEntity c pc
         , ToPersistEntity d pd
         , ToPersistEntity e pe
         , ToPersistEntity f pf
         , ToPersistEntity g pg
         , ToPersistEntity h ph
         ) =>  ToPersistEntity (a, b, c, d, e, f, g, h) (pa, pb, pc, pd, pe, pf, pg, ph) where
    recordFromSql' = (,,,,,,,) <$> r <*> r <*> r <*> r <*> r <*> r <*> r <*> r

instance ( ToPersistEntity a pa
         , ToPersistEntity b pb
         , ToPersistEntity c pc
         , ToPersistEntity d pd
         , ToPersistEntity e pe
         , ToPersistEntity f pf
         , ToPersistEntity g pg
         , ToPersistEntity h ph
         , ToPersistEntity i pi
         ) =>  ToPersistEntity (a, b, c, d, e, f, g, h, i) (pa, pb, pc, pd, pe, pf, pg, ph, pi) where
    recordFromSql' = (,,,,,,,,) <$> r <*> r <*> r <*> r <*> r <*> r <*> r <*> r <*> r

instance ( ToPersistEntity a pa
         , ToPersistEntity b pb
         , ToPersistEntity c pc
         , ToPersistEntity d pd
         , ToPersistEntity e pe
         , ToPersistEntity f pf
         , ToPersistEntity g pg
         , ToPersistEntity h ph
         , ToPersistEntity i pi
         , ToPersistEntity j pj
         ) =>  ToPersistEntity (a, b, c, d, e, f, g, h, i, j) (pa, pb, pc, pd, pe, pf, pg, ph, pi, pj) where
    recordFromSql' = (,,,,,,,,,) <$> r <*> r <*> r <*> r <*> r <*> r <*> r <*> r <*> r <*> r

instance ( ToPersistEntity a pa
         , ToPersistEntity b pb
         , ToPersistEntity c pc
         , ToPersistEntity d pd
         , ToPersistEntity e pe
         , ToPersistEntity f pf
         , ToPersistEntity g pg
         , ToPersistEntity h ph
         , ToPersistEntity i pi
         , ToPersistEntity j pj
         , ToPersistEntity k pk
         ) =>  ToPersistEntity (a, b, c, d, e, f, g, h, i, j, k) (pa, pb, pc, pd, pe, pf, pg, ph, pi, pj, pk) where
    recordFromSql' = (,,,,,,,,,,) <$> r <*> r <*> r <*> r <*> r <*> r <*> r <*> r <*> r <*> r <*> r

instance ( ToPersistEntity a pa
         , ToPersistEntity b pb
         , ToPersistEntity c pc
         , ToPersistEntity d pd
         , ToPersistEntity e pe
         , ToPersistEntity f pf
         , ToPersistEntity g pg
         , ToPersistEntity h ph
         , ToPersistEntity i pi
         , ToPersistEntity j pj
         , ToPersistEntity k pk
         , ToPersistEntity l pl
         ) =>  ToPersistEntity (a, b, c, d, e, f, g, h, i, j, k, l) (pa, pb, pc, pd, pe, pf, pg, ph, pi, pj, pk, pl) where
    recordFromSql' = (,,,,,,,,,,,) <$> r <*> r <*> r <*> r <*> r <*> r <*> r <*> r <*> r <*> r <*> r <*> r

instance ( ToPersistEntity a pa
         , ToPersistEntity b pb
         , ToPersistEntity c pc
         , ToPersistEntity d pd
         , ToPersistEntity e pe
         , ToPersistEntity f pf
         , ToPersistEntity g pg
         , ToPersistEntity h ph
         , ToPersistEntity i pi
         , ToPersistEntity j pj
         , ToPersistEntity k pk
         , ToPersistEntity l pl
         , ToPersistEntity m pm
         ) =>  ToPersistEntity (a, b, c, d, e, f, g, h, i, j, k, l, m) (pa, pb, pc, pd, pe, pf, pg, ph, pi, pj, pk, pl, pm) where
    recordFromSql' = (,,,,,,,,,,,,) <$> r <*> r <*> r <*> r <*> r <*> r <*> r <*> r <*> r <*> r <*> r <*> r <*> r

instance ( ToPersistEntity a pa
         , ToPersistEntity b pb
         , ToPersistEntity c pc
         , ToPersistEntity d pd
         , ToPersistEntity e pe
         , ToPersistEntity f pf
         , ToPersistEntity g pg
         , ToPersistEntity h ph
         , ToPersistEntity i pi
         , ToPersistEntity j pj
         , ToPersistEntity k pk
         , ToPersistEntity l pl
         , ToPersistEntity m pm
         , ToPersistEntity n pn
         ) =>  ToPersistEntity (a, b, c, d, e, f, g, h, i, j, k, l, m, n) (pa, pb, pc, pd, pe, pf, pg, ph, pi, pj, pk, pl, pm, pn) where
    recordFromSql' = (,,,,,,,,,,,,,) <$> r <*> r <*> r <*> r <*> r <*> r <*> r <*> r <*> r <*> r <*> r <*> r <*> r <*> r

instance ( ToPersistEntity a pa
         , ToPersistEntity b pb
         , ToPersistEntity c pc
         , ToPersistEntity d pd
         , ToPersistEntity e pe
         , ToPersistEntity f pf
         , ToPersistEntity g pg
         , ToPersistEntity h ph
         , ToPersistEntity i pi
         , ToPersistEntity j pj
         , ToPersistEntity k pk
         , ToPersistEntity l pl
         , ToPersistEntity m pm
         , ToPersistEntity n pn
         , ToPersistEntity o po
         ) =>  ToPersistEntity (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) (pa, pb, pc, pd, pe, pf, pg, ph, pi, pj, pk, pl, pm, pn, po) where
    recordFromSql' = (,,,,,,,,,,,,,,) <$> r <*> r <*> r <*> r <*> r <*> r <*> r <*> r <*> r <*> r <*> r <*> r <*> r <*> r <*> r

r :: ToPersistEntity a pa => RecordFromSql PersistValue pa
r = recordFromSql'
