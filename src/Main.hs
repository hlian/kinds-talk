{-
(ghc-init)

(defun thump () "wow" (interactive) (progn (widen) (forward-page) (narrow-to-page)))
(defun mump () "wow" (interactive) (progn (widen) (backward-page) (previous-line) (narrow-to-page)))
(define-key haskell-mode-map (kbd "C-c C-9") (lambda () (interactive) (mump)))
(define-key haskell-mode-map (kbd "C-c C-0") (lambda () (interactive) (thump)))
(global-set-key (kbd "C-c C-7") 'ghc-show-type)
-}


{- #imatypesafebaby2015 -}

{- $hao -}

module Main where



import Data.List
import Data.Map hiding (map)
import Text.Printf
import GHC.TypeLits
import GHC.Prim
import Data.Proxy
import Unsafe.Coerce

data JSON where
  JFloat :: Float -> JSON
  JString :: String -> JSON
  JArray :: [JSON] -> JSON
  JObject :: [(String, JSON)] -> JSON
  deriving Show


{-
data JSON where
   JFloat Float
 | JString String
 | JArray [JSON]
 | JObject [(String, JSON)]
 deriving (Show)
-}


class ToJSON a where
  encode :: a -> JSON

instance ToJSON Float where
  encode = JFloat

instance ToJSON [Char] where
  encode = JString

instance ToJSON a => ToJSON [a] where
  encode alist = JArray (map encode alist)

instance ToJSON v => ToJSON (Map String v) where
  encode amap = JObject [(k, encode v) | (k, v) <- toList amap]

curl :: JSON -> String
curl (JFloat f) = show f
curl (JString s) = printf "\"%s\"" s
curl (JArray jsons) = printf "[%s]" (intercalate "," (map curl jsons))
curl (JObject pairs) = printf "{%s}" (intercalate "," [printf "\"%s\": %s" k (curl v) | (k, v) <- pairs])

-- this is a function that takes a type T
-- and always returns

main :: IO ()
main = do
  let amap = fromList [("hello", 3.0 :: Float), ("there", 4.0)]
  (print . encode) amap



class Weird where
  weird :: a -> a

instance Weird where
  weird = id

waitWhat :: (Weird) => Int
waitWhat = 3



newtype Modulus s a = Modulus a deriving (Show, Eq)
newtype M s a = M a deriving (Show, Eq)
data AnyModulus a =  forall s. AnyModulus (Modulus s a)

add :: (Integral a) => Modulus s a -> M s a -> M s a -> M s a
add (Modulus m) (M a) (M b) = M (mod (a + b) m)

makeModulus :: a -> AnyModulus a
makeModulus modulus = AnyModulus (Modulus modulus)

unM (M a) = a

testModulus1 = case makeModulus 4 of AnyModulus m -> unM (add m (M 3) (M 5))

newtype MagicNat r = MagicNat (forall n. KnownNat n => Proxy n -> r)

-- | This upgraded version of 'reify' can be used to generate a 'KnownNat' suitable for use with other APIs.
--
-- /Available only on GHC 7.8+/
--
-- >>> reifyNat 4 natVal
-- 4
--
-- >>> reifyNat 4 reflect
-- 4

reifyNat :: forall r. Integer -> (forall n. KnownNat n => Proxy n -> r) -> r
reifyNat n k = f Proxy
  where
    f :: Proxy Any -> r
    f = g n
    g :: Integer -> Proxy Any -> r
    g = unsafeCoerce (MagicNat k :: MagicNat r)
