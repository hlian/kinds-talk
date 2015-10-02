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



import qualified Data.IntMap
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
curl (JObject pairs) = printf "{%s}" (intercalate "," [printf "\"%s\": %s" k (curl v) | (k, v) <- pairs] :: String)

-- this is a function that takes a type T
-- and always returns

encodeMe :: Map String Float
encodeMe = fromList [("hello", 30), ("there", 4.0)]

main :: IO ()
main = do
  (print . encode) encodeMe



class Weird where
  weird :: a -> a

instance Weird where
  weird = id

waitWhat :: (Weird) => Int
waitWhat = 3



lg :: Proxy base -> Proxy (base ^ pow) -> Proxy pow
lg _ _ = Proxy

seven = natVal (Proxy :: Proxy (3 + 4))
woah = natVal (lg (Proxy :: Proxy 2) (Proxy :: Proxy 8))



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


type L = [Int, Bool]
type N = String ': L



class GMapKey k where
  data GMap k :: * -> *
  empty       :: GMap k v
  lookup      :: k -> GMap k v -> Maybe v
  insert      :: k -> v -> GMap k v -> GMap k v

data Foo a = Foo a

instance GMapKey Int where
  data GMap Int v        = GMapInt (Data.IntMap.IntMap v)
  empty                  = GMapInt Data.IntMap.empty
  lookup k   (GMapInt m) = Data.IntMap.lookup k m
  insert k v (GMapInt m) = GMapInt (Data.IntMap.insert k v m)
