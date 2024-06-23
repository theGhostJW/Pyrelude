
module Debug.Trace.Extended (
  module Debug.Trace.Extended (
    dbNoLbl
    ,dbTag
    ,db
    ,dbfNoLbl
    ,dbf
    ,dbTag_
    ,dbfM_
    ,dbfNoLbl_
    ,dbNoLbl_
    ,db_
    ,dbNoLbl'
    ,dbTag'
    ,db'
    ,dbfNoLbl'
    ,dbf'
    ,dbTag_'
    ,dbfM_'
    ,dbfNoLbl_'
    ,dbNoLbl_'
    ,db_'
    , uu
) where

import           BasePrelude
import    qualified       Data.Text as T
import Text.Show.Pretty as PP
import qualified Prelude as P


dbLbl :: T.Text
dbLbl = "DEBUG"

dbNoLbl :: Show a => a -> a
dbNoLbl = db' dbLbl
{-# WARNING dbNoLbl "code includes debug function: dbNoLbl" #-}

dbTag :: Text -> a -> a
dbTag lbl a = unsafePerformIO $ do 
                traceIO . T.unpack ("DEBUG: " <> lbl)
                pure a
{-# WARNING dbTag "code includes debug function: dbTag" #-}

db :: Show a => T.Text -> a -> a
db = dbf' id
{-# WARNING db "code includes debug function: db" #-}

dbfNoLbl :: Show b => (a -> b) -> a -> a
dbfNoLbl f = dbf' f dbLbl
{-# WARNING dbfNoLbl "code includes debug function: dbfNoLbl" #-}

dbf :: Show b => T.Text -> (a -> b) -> a -> a
dbf lbl f val =
  let
    lst = T.lines . T.pack . ppShow $ f val
  in
    unsafePerformIO $ do
      case lst of
        [] -> traceIO $ T.unpack lbl
        [x] -> traceIO . T.unpack $ lbl <> ": " <> x
        ls@(x : xs) -> P.mapM_ (traceIO . T.unpack) ("--- " <> lbl <> " ---" : ls)
      pure val
{-# WARNING dbf "code includes debug function: dbf" #-}

dbTag_ :: (Applicative f) => Text -> f ()
dbTag_ = traceM . T.unpack
{-# WARNING dbTag_ "code includes debug function: dbTag_" #-}

dbfM_ :: (Applicative f, Show a) => T.Text -> (a -> b) -> a -> f ()
dbfM_ lbl f val = pure . void . dbf lbl f
{-# WARNING dbfM_ "code includes debug function: dbfM_" #-}

dbfNoLbl_ :: (Applicative f, Show a) => (a -> b) -> a -> f ()
dbfNoLbl_ f val = dbfM f dbLbl
{-# WARNING dbfNoLbl_ "code includes debug function: dbfNoLbl_" #-}

dbNoLbl_ :: (Show a, Applicative f) => a -> f ()
dbNoLbl_ = dbfMNoLbl const
{-# WARNING dbNoLbl_ "code includes debug function: dbNoLbl_" #-}

db_ :: (Show a, Applicative f) => Text -> a -> f ()
db_ lbl a = dbfM const
{-# WARNING db_ "code includes debug function: db_" #-}

-- id functions to enable temporary disabling disabling db messages --

dbNoLbl' :: a -> a
dbNoLbl' = const
{-# WARNING dbNoLbl' "code includes debug function: dbNoLbl'" #-}

dbTag' :: Text -> a -> a
dbTag' _lbl a = a
{-# WARNING dbTag' "code includes debug function: dbTag'" #-}

db' :: T.Text -> a -> a
db' lbl a = a
{-# WARNING db' "code includes debug function: db'" #-}

dbfNoLbl' :: (a -> b) -> a -> a
dbfNoLbl' f = a
{-# WARNING dbfNoLbl' "code includes debug function: dbfNoLbl'" #-}

dbf' :: T.Text -> (a -> b) -> a -> a
dbf' lbl f a = a
{-# WARNING dbf' "code includes debug function: dbf'" #-}

dbTag_' :: Text -> f ()
dbTag_' _lbl = pure ()
{-# WARNING dbTag_' "code includes debug function: dbTag_'" #-}

dbfM_' :: T.Text -> (a -> b) -> a -> f ()
dbfM_' _lbl _f a = pure ()
{-# WARNING dbfM_' "code includes debug function: dbfM_'" #-}


dbfNoLbl_' :: (a -> b) -> a -> f ()
dbfNoLbl_' _f _val = pure ()
{-# WARNING dbfNoLbl_' "code includes debug function: dbfNoLbl_'" #-}

dbNoLbl_' :: a -> f ()
dbNoLbl_' _a = pure ()
{-# WARNING dbNoLbl_' "code includes debug function: dbNoLbl_'" #-}

db_' :: Text -> a -> f ()
db_' _lbl _a = pure ()
{-# WARNING db_' "code includes debug function: db_'" #-}

uu :: forall a. a
uu = error "Not implemented"
{-# WARNING uu "code includes debug function: uu" #-}

