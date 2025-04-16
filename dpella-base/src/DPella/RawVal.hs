module DPella.RawVal where

import Data.Text (Text)
-- | Raw values for storage in the database. A bit smarter than SQLData, as it can store Booleans
--  as well.
data RawVal
  = RawBool Bool
  | RawDouble Double
  | RawInt Int
  | RawText Text

data RawType
  = RBoolTy
  | RDoubleTy
  | RIntTy
  | RTextTy
  deriving (Eq, Show)
