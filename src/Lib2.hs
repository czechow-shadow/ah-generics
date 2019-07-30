{-# LANGUAGE DefaultSignatures #-}
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
{-# LANGUAGE TypeApplications #-}
-- {-# LANGUAGE PolyKinds #-}

module Lib2 where

import Protolude
import GHC.Generics
import qualified Data.Generics as G
import qualified Data.Aeson as A

-- import Data.Type.Equality (type (==))
-- import GHC.TypeLits

-- import qualified Data.Set as S
-- import qualified Data.Map.Strict as M


-- Plain
-- we want to dissect the data
opts :: A.Options
opts = A.defaultOptions { A.unwrapUnaryRecords = True }

data MyN1 = MnnC1 { wania :: Text
                  , bania :: Int
                  } deriving Generic

data MyNested = MnC1 { f1 :: Maybe Int
                     , f2 :: Maybe [Text]
                     , config :: [ConfigF Int Text Text]
                     }
               | MnC2 { f3 :: Double }
               | MnC3 { nested :: MyN1 }
              deriving (Generic)

newtype CName = CName { unCName :: Text } deriving (Generic, Show)
instance A.ToJSON CName where toJSON = A.genericToJSON opts
newtype FName = FName { unFName :: Text } deriving (Generic, Show)
instance A.ToJSON FName where toJSON = A.genericToJSON opts
newtype Typ = Typ { unType :: Text } deriving (Generic, Show)
instance A.ToJSON Typ where toJSON = A.genericToJSON opts

-- Perhaps product of products...
data SOP = SOP { typ :: Typ
               , pop:: [SProd]
               } deriving (Generic, Show)
instance A.ToJSON SOP

data SProd = SProd { cname :: CName
                   , fields :: [SField]
                   } deriving (Generic, Show)
instance A.ToJSON SProd
data SField = SField { name :: FName
                     -- , modifier :: Modifier -- FIXME: type modifier
                     , typ :: SFieldType
                     } deriving (Generic, Show)
instance A.ToJSON SField

data SFieldType = STypeSimple { modifier :: Modifier
                              , typ :: Typ
                              }
                | STypeSOP { modifier :: Modifier
                           , sop :: SOP
                           }
                deriving (Generic, Show)
instance A.ToJSON SFieldType

data Modifier = MdNone
              | MdList
              | MdMaybe
              | MdSet
              | MdMap
              | MdIxset
              | CompModifier [Modifier]
              deriving (Generic, Show)
instance A.ToJSON Modifier

instance Monoid Modifier where
  mempty = MdNone
  MdNone `mappend` x = x
  x `mappend` MdNone = x
  CompModifier xs `mappend` CompModifier ys = CompModifier $ xs `mappend` ys
  x `mappend` CompModifier xs = CompModifier $ pure x <> xs
  CompModifier xs `mappend` x = CompModifier $ xs <> pure x
  x `mappend` y = CompModifier [x, y]

calcModifierDescr :: Modifier -> Text
calcModifierDescr _ = ""



toSOP :: forall a. (Generic a, ToSOP' (Proxy (Rep a))) => Proxy a -> SOP
toSOP _ = let x = Proxy :: Proxy (Rep a)
          in toSOP' x

class ToSOP' (f :: Type) where
  toSOP' :: f -> SOP

instance (KnownSymbol t, ToSum' (Proxy f))
      => ToSOP' (Proxy (D1 ('MetaData t x y z) f)) where
  toSOP' _ = let typ = Typ $ toS $ symbolVal (Proxy @t)
                 rest = toSum' (Proxy :: Proxy f)
             in SOP typ rest

-- class ToProds
class ToSum' (f :: Type) where
  toSum' :: f -> [SProd]

instance (ToSum' (Proxy f), ToSum' (Proxy g)) => ToSum' (Proxy (f :+: g)) where
 toSum' _ = toSum' (Proxy :: Proxy f) ++ toSum' (Proxy :: Proxy g)

instance (KnownSymbol t, ToFields (Proxy f))
      => ToSum' (Proxy (C1 ('MetaCons t x y) f)) where
  toSum' _ = let name = CName $ toS $ symbolVal (Proxy @t)
                 fields = toFields (Proxy @f)
             in pure $ SProd name fields

class ToFields (f :: Type) where
  toFields :: f -> [SField]

-- instance ToFields (Proxy U1) where
--   toFields _ = []

instance (ToFields (Proxy f), ToFields (Proxy g)) => ToFields (Proxy (f :*: g)) where
  toFields _ = toFields (Proxy @f) ++ toFields (Proxy @g)

instance (KnownSymbol fn, ToFieldType' (Proxy f))
      => ToFields (Proxy (S1 ('MetaSel ('Just fn) x y z) f)) where
  toFields _ = let name = FName $ toS $ symbolVal (Proxy @fn)
                   type' = toFieldType' (Proxy @f)
               in pure $ SField name type'

instance (ToFieldType' (Proxy f))
      => ToFields (Proxy (S1 ('MetaSel 'Nothing x y z) f)) where
  toFields _ = let name = FName $ "<unspecified>" -- toS $ symbolVal (Proxy @fn)
                   type' = toFieldType' (Proxy @f)
               in pure $ SField name type'


-- instance ( KnownSymbol fn
--          , ToFieldType' (Proxy a) isSimple
--          , f ~ K1 R a, isSimple ~ IsSimpleType a)
--       => ToFields (Proxy (S1 ('MetaSel ('Just fn) x y z) f)) where
--   toFields _ = let name = FName $ toS $ symbolVal (Proxy @fn)
--                    type' = toFieldType' (Proxy @isSimple) (Proxy @a)
--                in pure $ SField name type'

-- instance ( ToFieldType' (Proxy a) isSimple
--          , f ~ K1 R a, isSimple ~ IsSimpleType a)
--       => ToFields (Proxy (S1 ('MetaSel 'Nothing x y z) f)) where
--   toFields _ = let name = FName $ "Dummy"
--                    type' = toFieldType' (Proxy @isSimple) (Proxy @a)
--                in pure $ SField name type'

-- instance (ToFields (Proxy (S1 ('MetaSel 'Nothing x y z) f))
--                         , f ~ K1 R a
--                         ) -- ( KnownSymbol fn)
--          -- , ToFieldType' (Proxy a) False
--          -- , f ~ K1 R [a], isSimple ~ IsSimpleType a)
--       => ToFields (Proxy (S1 ('MetaSel 'Nothing x y z) (K1 r [a]))) where
--   toFields _ = toFields (Proxy @(S1 ('MetaSel 'Nothing x y z) f))

class ToFieldType' (f :: Type) where
  toFieldType' :: f -> SFieldType

-- class ToFieldType' (f :: Type) (isSimple :: Bool) where
--   toFieldType' :: Proxy isSimple -> f -> SFieldType

-- instance Typeable a => ToFieldType' (Proxy a) 'True where
--   toFieldType' _ _ = STypeSimple $ Typ $ show $ typeRep (Proxy @a)

-- instance ToFieldType' (Proxy a) 'False where
--   toFieldType' _ _ = STypeSimple $ Typ "xxxx"

-- instance {-# OVERLAPPABLE #-}
--          (Generic a, ToSOP' (Proxy (Rep a)))
--       => ToFieldType' (Proxy a) 'False where
--   toFieldType' _ _ = STypeSOP $ toSOP (Proxy @a)
instance {-# OVERLAPPABLE #-}
         (Generic a, ToSOP' (Proxy (Rep a)))
      => ToFieldType' (Proxy a) where
  toFieldType' _ = STypeSOP MdNone $ toSOP (Proxy @a)

-- instance (res ~ IsSimpleType a) => ToFieldType' (Proxy (K1 R a)) where
--   toFieldType' _ = in STypeSimple $ Typ "Int"

instance (res ~ IsSimpleType a) => ToFieldType' (Proxy (K1 R Int)) where
  toFieldType' _ = STypeSimple MdNone $ Typ "Int"
instance (res ~ IsSimpleType a) => ToFieldType' (Proxy (K1 R Text)) where
  toFieldType' _ = STypeSimple MdNone $ Typ "Text"
instance (res ~ IsSimpleType a) => ToFieldType' (Proxy (K1 R Double)) where
  toFieldType' _ = STypeSimple MdNone $ Typ "Double"

instance {-# OVERLAPPING #-}
         (ToFieldType' (Proxy f), f ~ K1 R a)
      => ToFieldType' (Proxy (K1 R (Maybe a))) where
  toFieldType' _ = prependModifier MdMaybe $ toFieldType' (Proxy @f)

instance {-# OVERLAPPING #-}
         (ToFieldType' (Proxy f), f ~ K1 R a)
      => ToFieldType' (Proxy (K1 R [a])) where
  toFieldType' _ = prependModifier MdList $ toFieldType' (Proxy @f)

instance {-# OVERLAPPABLE #-}
         (Generic a, ToSOP' (Proxy (Rep a)))
      => ToFieldType' (Proxy (K1 R a)) where
  toFieldType' _ = STypeSOP MdNone $ toSOP (Proxy @a)

prependModifier :: Modifier -> SFieldType -> SFieldType
prependModifier md' (STypeSimple md tp) = STypeSimple (md' <> md) tp
prependModifier md' (STypeSOP md tp) = STypeSOP (md' <> md) tp


-- FIXME: etc...
type family IsSimpleType (a :: Type) :: Bool where
  IsSimpleType Int = 'True
  IsSimpleType Text = 'True
  IsSimpleType Double = 'True
  IsSimpleType [Char] = 'True
  IsSimpleType _ = 'False



-- we may not need this one ;-)
-- class Generic a => ToSOP a where
--   toSOP :: Proxy a -> SOP
--   default toSOP :: (Generic a, ToSOP' (Rep a)) => Proxy a -> SOP
--   toSOP x = toSOP' (undefined :: Rep a)

-- class ToSOP' (f :: Type -> Type) where
--   toSOP' :: f p -> SOP

-- instance ToSSum' U1 where -- no singletons
-- instance ToSSum' V1 where
--   toSSum' _ = panic "undefined -- SSum []

-- instance (KnownSymbol t, ToSSum' f) => ToSOP' (D1 ('MetaData t x y z) f) where
--   toSOP' (M1 v) = let z = symbolVal (Proxy @t)
--                       ss = toSSum' v
--                   in SOP (Typ $ toS z) ss

-- class ToSSum' f where
--   toSSum' :: f p -> SSum

-- instance (ToSSum' f, ToSSum' g) => ToSSum' (f :+: g) where
--   toSSum' v = undefined -- toSSum' f


-- -- instance (KnownSymbol cn, ToSProd' f) => ToSSum' (C1 ('MetaCons cn x z) f) where
-- --   toSSum' (M1 v) = let constrName = symbolVal (Proxy @cn)
-- --                        prods = toSProd' v
-- --                    in SSum $ prods
-- instance (KnownSymbol cn) => ToSSum' (C1 ('MetaCons cn x z) f) where
--   toSSum' (M1 v) = let constrName = symbolVal (Proxy @cn)
--                        prods = [] -- toSProd' v
--                    in SSum $ prods

-- class ToSProd' f where
--   toSProd' :: f p -> [SProd]

-- instance (ToSProd' f, ToSProd' g) => ToSProd' (f :*: g) where
--   toSProd' (f :*: g) = toSProd' f ++ toSProd' g

-- instance (KnownSymbol rn, ToSField' f) => ToSProd' (S1 ('MetaSel ('Just rn) x y z) f) where
--   toSProd' (M1 v) = let fieldName = symbolVal (Proxy @rn)
--                         fields = toSField' v
--                     in pure $ SProd (CName $ toS fieldName) fields


-- class ToSField' f where
--   toSField' :: f p -> [SField]

-- instance ToSField' (K1 R Int) where
--   toSField' _ = undefined

-- instance ToSField' (K1 R Text) where
--   toSField' _ = undefined


data X = X deriving Generic

-- instance ToSSum' V1 where
--   toSSum' _ = SSum []


-- data ConfigF a b c = Config { clients :: [ClientF a]
--                             , services :: [ServiceF a b]
--                             , key :: KeyF c
--                             } deriving (Show, Generic, G.Data)

data ConfigF a b c = Config { clients :: ClientF a
                            , services :: ServiceF a b
                            , key :: KeyF c
                            } deriving (Show, Generic, G.Data)

data ClientF a = Client { passwd :: a } deriving (Show, Generic, G.Data)
data ServiceF a b = Service { passwd :: a
                            , url :: b
                            } deriving (Show, Generic, G.Data)

data KeyF c = Key { key :: c } deriving (Show, Generic, G.Data)

newtype Password = Password { unPassword :: Text } deriving (Show, G.Data)
newtype Url = Url { unUrl :: Text } deriving (Show, G.Data)
newtype EncrText = EncrText { unEncrText :: Text } deriving (Show, G.Data)

newtype Password2 = Password2 { unPassword2 :: Text } deriving (Show, G.Data)
newtype Url2 = Url2 { unUrl2 :: Text } deriving (Show, G.Data)
newtype EncrText2 = EncrText2 { unEncrText2 :: Text } deriving (Show, G.Data)

type ConfigResolved = ConfigF Password2 Url2 EncrText2

type ConfigInitial = ConfigF Password Url EncrText

go :: IO ()
go = do
  writeFile "res.txt" $ toS $ A.encode $ toSOP (Proxy :: Proxy MyNested)
