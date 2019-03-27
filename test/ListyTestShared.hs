
module ListyTestShared where

import Pyrelude as P
import           Pyrelude.Test as T

chk1 :: (Eq e, Show e) => e -> a -> (a -> e)-> IO ()
chk1 e a f = e ... f a

chk2 :: (Eq e, Show e) => e -> a -> b -> (a -> b -> e)-> IO ()
chk2 e a b f = e ... f a b
