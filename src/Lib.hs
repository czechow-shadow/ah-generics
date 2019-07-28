{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}

module Lib where

import Protolude
import GHC.Generics
import qualified Data.Generics as G
import Data.Type.Equality (type (==))


import qualified Data.Set as S
import qualified Data.Map.Strict as M


data ConfigF a b c = Config { clients :: [ClientF a]
                            , services :: [ServiceF a b]
                            , key :: KeyF c
                            } deriving (Show, G.Data)

data ClientF a = Client { passwd :: a } deriving (Show, G.Data)
data ServiceF a b = Service { passwd :: a
                            , url :: b
                            } deriving (Show, G.Data)
data KeyF c = Key { key :: c } deriving (Show, G.Data)

newtype Password = Password { unPassword :: Text } deriving (Show, G.Data)
newtype Url = Url { unUrl :: Text } deriving (Show, G.Data)
newtype EncrText = EncrText { unEncrText :: Text } deriving (Show, G.Data)

type ConfigResolved = ConfigF Password Url EncrText

type ConfigInitial = ConfigF Text Text Text

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

class TypeTraversable s t a b where
  typeTraverse :: Applicative g => (a -> g b) -> s -> g t

instance (Generic s, Generic t, GTypeTraversable (Rep s) (Rep t) a b)
      => TypeTraversable s t a b where
  typeTraverse f s = to <$> gTypeTraverse f (from s)


class GTypeTraversable s t a b where
  gTypeTraverse :: forall g (x :: Type) . Applicative g
                => (a -> g b) -> s x -> g (t x)

instance (GTypeTraversable l l' a b, GTypeTraversable r r' a b)
      => GTypeTraversable (l :*: r) (l' :*: r') a b where
  gTypeTraverse f (l :*: r) = (:*:) <$> gTypeTraverse f l <*> gTypeTraverse f r

instance (GTypeTraversable l l' a b, GTypeTraversable r r' a b)
      => GTypeTraversable (l :+: r) (l' :+: r') a b where
  gTypeTraverse f (L1 l) = L1 <$> gTypeTraverse f l
  gTypeTraverse f (R1 r) = R1 <$> gTypeTraverse f r

instance GTypeTraversable s t a b
      => GTypeTraversable (M1 m meta s) (M1 m meta t) a b where
  gTypeTraverse f (M1 x) = M1 <$> gTypeTraverse f x

instance GTypeTraversable U1 U1 a b where
  gTypeTraverse _ _ = pure U1

instance {-# OVERLAPPING #-}
         TypeTraversable (f3 (f2 (f1 a))) (g3 (g2 (g1 b))) a b
      => GTypeTraversable (K1 R (f3 (f2 (f1 a)))) (K1 R (g3 (g2 (g1 b)))) a b where

  gTypeTraverse f (K1 xs) = K1 <$> typeTraverse f xs

instance {-# OVERLAPPING #-}
     TypeTraversable (f2 (f1 a)) (g2 (g1 b)) a b
  => GTypeTraversable (K1 R (f2 (f1 a))) (K1 R (g2 (g1 b))) a b where

  gTypeTraverse f (K1 xs) = K1 <$> typeTraverse f xs

instance {-# OVERLAPPING #-}
         TypeTraversable (f a) (g b) a b
      => GTypeTraversable (K1 R (f a)) (K1 R (g b)) a b where
  
  gTypeTraverse f (K1 xs) = K1 <$> typeTraverse f xs

instance {-# OVERLAPPING #-}
         (teq ~ (a == a'), GTypeTraversableRec teq (K1 R a) (K1 R b) a' b')
      => GTypeTraversable (K1 R a) (K1 R b) a' b' where
  gTypeTraverse f v = gTypeTraverseRec (Proxy :: Proxy teq) f v

class GTypeTraversableRec (p :: Bool) s t a b where
  gTypeTraverseRec :: forall g (x :: Type) . Applicative g
                   => Proxy p -> (a -> g b) -> s x -> g (t x)

instance GTypeTraversableRec 'True (K1 R a) (K1 R b) a b where
  gTypeTraverseRec _ f (K1 x) = K1 <$> f x

instance GTypeTraversableRec 'False s s a b where
  gTypeTraverseRec _ _ s = pure s

------------------------------------------------------------------------------- 

newtype MyText = MyText { unText :: Text } deriving Show
instance Monoid MyText where
  mempty = MyText mempty
  MyText t1 `mappend` MyText t2 = MyText $ t1 <> t2

-- data MT a b = MT [[a]] b deriving (Show, Generic)

-- mt :: MT Int MyText
-- mt = MT [[1..3], [31 .. 35]] (MyText "Wania")
-- mt = MT (S.singleton 1) (MyText "Wania")

-- -- data MT a = MT [[a]] deriving (Show, Generic)
-- -- data MT a = MT [[Maybe a]] [[a]] a [Text] deriving (Show, Generic)
data MT a b = MT { aofa :: [[Maybe a]]
                 , as :: [[a]]
                 , v :: a
                 , name :: [Text]
                 , myText :: MyText
                 , mp :: Map Text a
                 , bParam :: b
                 } deriving (Show, Generic)
mt :: MT Int [Text]
-- mt = MT [[1 ..3 ], [10..11]]
mt = MT
  [[Just 1, Just 10, Nothing]]
  [[1 ..3], [30 ..35]]
  997
  ["xxx"]
  (MyText "Wania")
  (M.fromList [("k1", 1313)])
  []



go3 :: IO ()
go3 = do
  -- let res :: IO (MT Text) =
  --       -- (traverseOf (typeTraverse @_ @_ @MyText) (\x -> pure $ (MyText "==> ") <> x)) <=<
  --        (traverseOf typeTraverse incM) $ mt
  -- res' <- res
  -- putText $ show res'
  -- -- let res'' :: IO (MT Int) =
  -- --       (traverseOf (typeTraverse @_ @_ @MyText) (\x -> pure $ (MyText "==> ") <> x)) res'

  -- -- putText . show =<< res''
  -- let res4 :: IO (MT Text) = (traverseOf typeTraverse chgeMyTextM) $ res'
  -- putText . show =<< res4

  --let z = conv2 <=< conv1 $ mt
  let z = conv1 $ mt

  putText . show =<< z

-- conv1 :: MT Int a -> IO (MT Text a)
conv1 :: MT Int [Text] -> IO (MT Text [Text])
conv1 = traverseOf typeTraverse incM

conv2 :: MT Text MyText -> IO (MT Text MyText)
conv2 = traverseOf typeTraverse chgeMyTextM

incM :: Int -> IO Text
incM = pure . show . succ

chgeMyTextM :: MyText -> IO MyText
chgeMyTextM x = pure $ MyText "==> " <> x <> MyText " <=="

-- -- data MT a = MT [[a]] deriving (Show, Generic)
-- -- data MT a = MT [[Maybe a]] [[a]] a [Text] deriving (Show, Generic)
-- data MT a b = MT { aofa :: [[Maybe a]]
--                  , as :: [[a]]
--                  , v :: a
--                  , name :: [Text]
--                  , myText :: MyText
--                  } deriving (Show, Generic)

-- mt :: MT Int [Text]
-- -- mt = MT [[1 ..3 ], [10..11]]
-- mt = MT [[Just 1, Just 10, Nothing]] [[1 ..3], [30 ..35]] 997 ["xxx"] (MyText "Wania")


traverseOf :: a -> a
traverseOf = identity

data Nested a = Nested { setOfA :: Map Text a } deriving (Functor, Foldable, Traversable)

instance Ord k => Generic (Map k v) where
  type Rep (Map k v) = Rep [(k, v)]
  from = from . M.toList
  to = M.fromList . to
  
instance Ord a => Generic (Set a) where
  type Rep (Set a) = Rep [a]
  from = from . S.toList
  to = S.fromList . to

