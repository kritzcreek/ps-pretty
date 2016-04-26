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

gcd n
  | n > m = gcd n m
  | n < m = gcd m n
  | otherwise = 1

foreign import sin :: Number -> String

foreign import data CONSOLE :: !

infixr 9 compose as ∘
infix 9 lol as *#*

class Show a where
  show ∷ a → String

class (Eq a) <= Ord a where
  (<=) :: a -> a -> Eq

instance showString :: Show String where
  show s = s

instance showMaybe :: (Show a) => Show (Maybe a) where
  show (Just a) = "Just " <> show a
  show Nothing = "Nothing"

derive instance genericMyRecord :: Generic MyRecord
