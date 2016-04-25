-- This is my Pretty module

-- It's beautiful af
{-
OHAI LOL
-}
module Pretty
       ( id
       , const
       , -- A class that shows some things
         class Show
       , Maybe(Just)
       , Maybe(..)
       , Maybe()
       , Maybe
       ) where

import Prelude
import Data.Array (filter, filterM, (<+>))
import Data.List hiding (filter)

data Maybe a = Just a | Nothing | Lots | Of | Constructors String Text ROFL
data MaybeE (a :: *) = Just a | Nothing

newtype Text = Text String
newtype Person = Person { name :: String
                        , age :: Int
                        }
type Carl a (b :: !) = XD

id :: forall a. a -> a
id x = x

const :: forall a b. a -> b -> a
const x _ = x
