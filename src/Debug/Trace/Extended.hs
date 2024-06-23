module Debug.Trace.Extended (
  -- primary db functions
  db,
  dbNoLbl,
  dbTag,
  dbf,
  dbfNoLbl,
  -- monadic db functions
  db_,
  dbTag_,
  dbfNoLbl_,
  dbNoLbl_,
  -- disabled versions
  db',
  dbNoLbl',
  dbTag',
  dbfNoLbl',
  dbf',
  db_',
  dbTag_',
  dbf_',
  dbfNoLbl_',
  dbNoLbl_',
  -- bottom
  uu,
) where

import BasePrelude (
  Applicative (pure),
  Category (id, (.)),
  Monad ((>>)),
  Semigroup ((<>)),
  Show,
  error,
  traceIO,
  traceM,
  unsafePerformIO,
  void,
  ($),
 )
import Data.Text qualified as T
import Text.Show.Pretty as PP (ppShow)
import Prelude qualified as P

dbLbl :: T.Text
dbLbl = "DEBUG"

dbNoLbl :: (Show a) => a -> a
dbNoLbl = db dbLbl
{-# WARNING dbNoLbl "code includes debug function: dbNoLbl" #-}

dbTag :: T.Text -> a -> a
dbTag lbl a = unsafePerformIO $ traceIO (T.unpack ("DEBUG: " <> lbl)) >> pure a
{-# WARNING dbTag "code includes debug function: dbTag" #-}

db :: (Show a) => T.Text -> a -> a
db lbl = dbf lbl id
{-# WARNING db "code includes debug function: db" #-}

dbfNoLbl :: (Show b) => (a -> b) -> a -> a
dbfNoLbl = dbf dbLbl
{-# WARNING dbfNoLbl "code includes debug function: dbfNoLbl" #-}

dbf :: (Show b) => T.Text -> (a -> b) -> a -> a
dbf lbl f a =
  let
    lst = T.lines . T.pack . ppShow $ f a
   in
    unsafePerformIO $ do
      case lst of
        [] -> traceIO $ T.unpack lbl
        [x] -> traceIO . T.unpack $ lbl <> ": " <> x
        ls@(x : xs) -> P.mapM_ (traceIO . T.unpack) ("--- " <> lbl <> " ---" : ls)
      pure a
{-# WARNING dbf "code includes debug function: dbf" #-}

dbTag_ :: (Applicative f) => T.Text -> f ()
dbTag_ = traceM . T.unpack
{-# WARNING dbTag_ "code includes debug function: dbTag_" #-}

dbf_ :: (Applicative f, Show b) => T.Text -> (a -> b) -> a -> f ()
dbf_ lbl f = void . pure . dbf lbl f
{-# WARNING dbf_ "code includes debug function: dbf_" #-}

dbfNoLbl_ :: (Applicative f, Show b) => (a -> b) -> a -> f ()
dbfNoLbl_ = dbf_ dbLbl
{-# WARNING dbfNoLbl_ "code includes debug function: dbfNoLbl_" #-}

dbNoLbl_ :: (Show a, Applicative f) => a -> f ()
dbNoLbl_ = dbfNoLbl_ id
{-# WARNING dbNoLbl_ "code includes debug function: dbNoLbl_" #-}

db_ :: (Show a, Applicative f) => T.Text -> a -> f ()
db_ lbl = dbf_ lbl id
{-# WARNING db_ "code includes debug function: db_" #-}

-- id functions to enable temporary disabling disabling db messages --

dbNoLbl' :: a -> a
dbNoLbl' a = a
{-# WARNING dbNoLbl' "code includes debug function: dbNoLbl'" #-}

dbTag' :: T.Text -> a -> a
dbTag' _lbl a = a
{-# WARNING dbTag' "code includes debug function: dbTag'" #-}

db' :: T.Text -> a -> a
db' lbl a = a
{-# WARNING db' "code includes debug function: db'" #-}

dbfNoLbl' :: (a -> b) -> a -> a
dbfNoLbl' _f a = a
{-# WARNING dbfNoLbl' "code includes debug function: dbfNoLbl'" #-}

dbf' :: T.Text -> (a -> b) -> a -> a
dbf' lbl f a = a
{-# WARNING dbf' "code includes debug function: dbf'" #-}

dbTag_' :: (Applicative f) => T.Text -> f ()
dbTag_' _lbl = pure ()
{-# WARNING dbTag_' "code includes debug function: dbTag_'" #-}

dbf_' :: (Applicative f) => T.Text -> (a -> b) -> a -> f ()
dbf_' _lbl _f a = pure ()
{-# WARNING dbf_' "code includes debug function: dbf_'" #-}

dbfNoLbl_' :: (Applicative f) => (a -> b) -> a -> f ()
dbfNoLbl_' _f _val = pure ()
{-# WARNING dbfNoLbl_' "code includes debug function: dbfNoLbl_'" #-}

dbNoLbl_' :: (Applicative f) => a -> f ()
dbNoLbl_' _a = pure ()
{-# WARNING dbNoLbl_' "code includes debug function: dbNoLbl_'" #-}

db_' :: (Applicative f) => T.Text -> a -> f ()
db_' _lbl _a = pure ()
{-# WARNING db_' "code includes debug function: db_'" #-}

uu :: forall a. a
uu = error "Not implemented"
{-# WARNING uu "code includes debug function: uu" #-}
