module TypeChecking ( Type(..)
                    ) where
import Data.Tree
import qualified Data.LCA.Online as LCA

data Type = Char
          | Int16
          | Int32
          | Int64
          | Float32
          | Float64
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
          deriving (Eq, Ord, Show, Read)

path :: Type -> LCA.Path Type
-- Float tree.
path Float64 = LCA.cons 0 Float64 (path Void)
path Float32 = LCA.cons 1 Float32 (path Float64)

-- Integer tree
path Int64 = LCA.cons 2 Int64 (path Void)
path Int32 = LCA.cons 3 Int32 (path Int64)
path Int16 = LCA.cons 4 Int16 (path Int32)
path Char = LCA.cons 5 Char (path Int16)
path Bool = LCA.cons 6 Bool (path Int16)

-- We can also implicitly convert Enum variables into int32.
path (Enum _) = LCA.cons 7 (path Int32)
