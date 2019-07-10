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

module Lib where

import Protolude
import GHC.Generics
-- import qualified Data.Text as T
import qualified Data.Generics as G


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

change :: ConfigResolved -> ConfigResolved
change c = G.everywhere (G.mkT (chgPwd "...")) .
           G.everywhere (G.mkT (chgUrl "---")) $ c

changeM :: MonadState Int m => ConfigResolved -> m ConfigResolved
changeM c = do
  G.everywhereM (G.mkM (chgPwdM "---")) $ c

chgPwd :: Text -> Password -> Password
chgPwd text (Password x) = Password $ text <> x

chgPwdM :: MonadState Int m => Text -> Password -> m Password
chgPwdM text (Password x) = do
  modify (+1)
  v <- get
  pure $ Password $ text <> " " <> show v <> " " <> x

chgUrl :: Text -> Url -> Url
chgUrl text (Url x) = Url $ text <> x

config :: ConfigResolved
config = Config { clients =
                  [ Client $ Password "pwd1"
                  ]
                , services = [ Service (Password "pwd2") (Url "url1")]
                , key = Key $ EncrText "klucz"
                }


data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Generic, Show, G.Data)

type TreeInt = Tree Int

t :: TreeInt
t = Leaf 3

go :: IO ()
go = do
  putText $ show $ G.everywhere (\x -> x) t
  putText "End"


  putText "===> End"

incrV :: Int -> TreeInt -> TreeInt
incrV x = G.everywhere (G.mkT (incL x))

incL :: Int -> TreeInt -> TreeInt
incL v (Leaf x) = Leaf $ v + x
incL v (Node l r) = Node (incL v l) (incL v r)

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

class DoJob s a where
  doJob :: Applicative g => (a -> g a) -> s -> g s

instance ( Generic s
         , GDoJob (Rep s) a
         ) => DoJob s a where
  doJob f s = to <$> gDoJob f (from s)
  -- param = trace "confusing" $ confusing (\f s -> toN <$> gparam @n f (fromN s))

class GDoJob s a where
  gDoJob :: forall g (x :: Type) . Applicative g => (a -> g a) -> s x -> g (s x)

instance (GDoJob l a, GDoJob r a) => GDoJob (l :*: r) a where
  gDoJob f (l :*: r) = (:*:) <$> gDoJob f l <*> gDoJob f r

instance (GDoJob l a, GDoJob r a) => GDoJob (l :+: r) a where
  gDoJob f (L1 l) = L1 <$> gDoJob f l
  gDoJob f (R1 r) = R1 <$> gDoJob f r

instance GDoJob s a => GDoJob (M1 m meta s) a where
  gDoJob f (M1 x) = M1 <$> gDoJob f x

instance GDoJob U1 a where
  gDoJob _ x = pure x

instance {-# OVERLAPPING #-} (Show a, DoJob (h (g (f a))) a)
      => GDoJob (K1 R (h (g (f a)))) a where
  gDoJob f (K1 xs) = K1 <$> doJob f xs

instance {-# OVERLAPPING #-} (Show a, DoJob (g (f a)) a) => GDoJob (K1 R (g (f a))) a where
  gDoJob f (K1 xs) = trace (("Found [[a]] in " :: Text)) $ -- <> show xs) $
                     K1 <$> doJob f xs

-- WITH SHOW:
-- instance {-# OVERLAPPABLE #-} (Show a, DoJob [a] a) => GDoJob (K1 R [a]) a where
--   gDoJob f (K1 xs) = trace (("Found [a] in " :: Text) <> show xs) $
--     let z = doJob f xs
--         z' = K1 <$> z
--     in z'
instance {-# OVERLAPPING #-} (Show (f a), DoJob (f a) a)
      => GDoJob (K1 R (f a)) a where
  gDoJob f (K1 xs) = trace (("Found [a] in [" :: Text) <> show xs) $
                     K1 <$> doJob f xs

type family IfTypesEq (t1 :: Type) (t2 :: Type) :: Bool where
  IfTypesEq a a = 'True
  IfTypesEq _ _ = 'False


-- instance {-# OVERLAPPING #-} (Show a) => GDoJob (K1 R a) b where
--   gDoJob f (K1 x) = K1 <$> pure x -- K1 <$> f x
instance {-# OVERLAPPING #-} (Show a, teq ~ (IfTypesEq a b),  GDoJobRec teq (K1 R a) b)
       => GDoJob (K1 R a) b where
  -- gDoJob f (K1 x) = K1 <$> pure x -- K1 <$> f x
  gDoJob f (K1 x) = gDoJobRec (Proxy :: Proxy teq) f (K1 x)
    -- panic $ "Found int type equal type in " <> show x

class GDoJobRec (t :: Bool) s a where
  gDoJobRec :: forall g (x :: Type) . Applicative g => Proxy t -> (a -> g a) -> s x -> g (s x)

instance (a ~ b) => GDoJobRec 'True (K1 R a) b where
  gDoJobRec _ f (K1 x) = K1 <$> f x

instance GDoJobRec 'False (K1 R a) b where
  gDoJobRec _ _ (K1 x) = K1 <$> pure x

newtype MyText = MyText { unText :: Text } deriving Show
instance Monoid MyText where
  mempty = MyText mempty
  MyText t1 `mappend` MyText t2 = MyText $ t1 <> t2

go2 :: IO ()
go2 = do
  let res = (traverseOf (doJob @_ @MyText) (\x -> pure $ (MyText "==> ") <> x) <=<
             traverseOf (doJob @_ @Int) (pure . succ)) mt
  putText . show =<< res

-- data MT a = MT [[a]] deriving (Show, Generic)
-- data MT a = MT [[Maybe a]] [[a]] a [Text] deriving (Show, Generic)
data MT a b = MT { aofa :: [[Maybe a]]
                 , as :: [[a]]
                 , v :: a
                 , name :: [Text]
                 , myText :: MyText
                 } deriving (Show, Generic)

mt :: MT Int [Text]
-- mt = MT [[1 ..3 ], [10..11]]
mt = MT [[Just 1, Just 10, Nothing]] [[1 ..3], [30 ..35]] 997 ["xxx"] (MyText "Wania")


traverseOf :: a -> a
traverseOf = identity
