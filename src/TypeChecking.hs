module TypeChecking ( Type(..)
                    ) where

import Data.Maybe
import qualified Data.LCA.Online as LCA

data Type = Char
          | Int32
          | Float
          | Bool
          | Void
          | Array { dimensions :: (Int, Int)
                  , stype :: Type }
          | Record { rfields :: [(String, Type)] }
          | Union { rfields :: [(String, Type)] }
          | Enum { efields :: [String] }
          | ReferenceTo Type
          | Function { domain :: [Type],
                       range :: Type }
          | Range { start :: Int
                  , end :: Int
                  , step :: Int }
          deriving (Eq, Ord, Show, Read)

path :: Type -> LCA.Path Type
-- Float tree.
path Float = LCA.cons 0 Float LCA.empty

-- Integer tree
path Int32 = LCA.cons 1 Int32 (path Float)
path Char = LCA.cons 2 Char (path Int32)

-- Bool is a disjoint tree.
path Bool = LCA.cons 3 Bool LCA.empty

-- We can also implicitly convert Enum variables into int32.
path t@(Enum _) = LCA.cons 4 t (path Int32)

-- Everything else is handled by hand
path t = LCA.cons 7 t LCA.empty

common :: LCA.Path a -> a
common  = snd . head . LCA.toList

boperator :: String -> Type -> Type -> Maybe Type

-- TODO:
-- references!
-- toperator ".."
-- boperator "."

-- Define what can be casted explicitly.
boperator "AS" Int32 Float = Just Float
boperator "AS" Int32 Char = Just Char
boperator "AS" Int32 Bool = Just Bool
boperator "AS" Float Int32 = Just Int32
boperator "AS" Float Char = Just Char
boperator "AS" Float Bool = Just Bool
boperator "AS" Bool Int32 = Just Int32
boperator "AS" Bool Float = Just Float
boperator "AS" Bool Char = Just Char
boperator "AS" Char Int32 = Just Int32
boperator "AS" Char Bool = Just Int32
boperator "AS" Char Float = Just Int32
boperator "AS" (Array (x, y) t) (Array (x', y') t') =
  if (y-x) >= (y'-x') && isJust (boperator "AS" t t')
  then Just (Array (x', y') t')
  else Nothing
boperator "AS" (Enum _) Int32 = Just Int32
boperator "AS" (Enum _) Char = Just Char
boperator "AS" (Enum _) Float = Just Float
boperator "AS" _ _ = Nothing


-- Suma!
boperator "+" Int32 Int32 = Just Int32
boperator "+" Char Char = Just Char
boperator "+" Float Float = Just Float

-- Resta!
boperator "-" Int32 Int32 = Just Int32
boperator "-" Char Char = Just Char
boperator "-" Float Float = Just Float

-- Multiplicacion!
boperator "*" Int32 Int32 = Just Int32
boperator "*" Char Char = Just Char
boperator "*" Float Float = Just Float

-- Division
boperator "/" Int32 Int32 = Just Int32
boperator "/" Char Char = Just Char
boperator "/" Float Float = Just Float

-- Exponenciacion
boperator "**" Int32 Int32 = Just Int32
boperator "**" Float Float = Just Float
boperator "**" Char Char = Just Char

-- Modulo
boperator "%" Int32 Int32 = Just Int32
boperator "%" Float Float = Just Float
boperator "%" Char Char = Just Char


-- Shifts. Explicit behavior.
boperator ">>" Int32 Int32 = Just Int32
boperator ">>" Float Int32 = Just Float
boperator ">>" Float Char = Just Float
boperator ">>" Char Char = Just Char
boperator ">>" Char Int32 = Just Char

boperator "<<" Int32 Int32 = Just Int32
boperator "<<" Float Int32 = Just Float
boperator "<<" Float Char = Just Float
boperator "<<" Char Char = Just Char
boperator "<<" Char Int32 = Just Char

-- Binary OR
boperator "|" Int32 Int32 = Just Int32
boperator "|" Char Char = Just Char
boperator "|" Float Float = Just Float

-- Binary AND
boperator "&" Int32 Int32 = Just Int32
boperator "&" Char Char = Just Char
boperator "&" Float Float = Just Float

-- Array indexing. Coerce floats to ints...
boperator "[]" (Array _ t) Int32 = Just t
boperator "[]" (Array _ t) Float = Just t
boperator "[]" (Array _ t) Char = Just t
boperator "[]" (Array _ t) (Range x y z) = Just (Array (0, length [x,x+z..y]) t)

-- Boolean operations.
boperator "&&" Bool Bool = Just Bool
boperator "||" Bool Bool = Just Bool
boperator "^" Bool Bool = Just Bool
boperator "^" Int32 Int32 = Just Int32
boperator "^" Char Char = Just Char
boperator "^" Float Float = Just Float

boperator "==" Int32 Int32 = Just Bool
boperator "==" Float Float = Just Bool
boperator "==" Char Char = Just Bool
boperator "==" Bool Bool = Just Bool

boperator "!=" Int32 Int32 = Just Bool
boperator "!=" Float Float = Just Bool
boperator "!=" Char Char = Just Bool
boperator "!=" Bool Bool = Just Bool

boperator ">" Int32 Int32 = Just Bool
boperator ">" Float Float = Just Bool
boperator ">" Char Char = Just Bool
boperator ">" Bool Bool = Just Bool

boperator ">=" Int32 Int32 = Just Bool
boperator ">=" Float Float = Just Bool
boperator ">=" Char Char = Just Bool
boperator ">=" Bool Bool = Just Bool


boperator "<" Int32 Int32 = Just Bool
boperator "<" Float Float = Just Bool
boperator "<" Char Char = Just Bool
boperator "<" Bool Bool = Just Bool


boperator "<=" Int32 Int32 = Just Bool
boperator "<=" Float Float = Just Bool
boperator "<=" Char Char = Just Bool
boperator "<=" Bool Bool = Just Bool


-- There was no known operation for these types on this
-- operator. Let's try to wiggle wiggle them a bit.
-- Wiggle wiggle = Try to implicitly cast to a common type.
boperator op t1 t2 = let anc = LCA.lca (path t1) (path t2)
                         cr = common anc
                     in if ((not $ LCA.null anc) && (cr /= t1 || cr /= t2))
                        then  boperator op cr cr
                        else Nothing


uoperator :: String -> Type -> Maybe Type
uoperator "#" Char = Just Int32
uoperator "#" (Array _ _) = Just Int32
uoperator "@" Int32 = Just Char
uoperator "@" (Array _ _) = Just Int32
uoperator "~" Int32 = Just Int32
uoperator "~" Float = Just Float
uoperator "~" Char = Just Float
uoperator "+" Int32 = Just Int32
uoperator "+" Float = Just Float
uoperator "+" Char = Just Char
uoperator "-" Int32 = Just Int32
uoperator "-" Char = Just Char
uoperator "-" Float = Just Float
uoperator "!" Bool = Just Bool
uoperator _ _ = Nothing
