{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

module TIP where

{- 

   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Type-indexed products.

-}


import FakePrelude
import HListPrelude
import HArray
import HOccurs
import HTypeIndexed


{-----------------------------------------------------------------------------}

-- The newtype for type-indexed products

newtype TIP l = TIP l
        deriving (Read,Show)

mkTIP :: HTypeIndexed l => l -> TIP l
mkTIP = TIP

unTIP :: TIP l -> l
unTIP (TIP l) = l

emptyTIP :: TIP HNil
emptyTIP = mkTIP HNil



{-----------------------------------------------------------------------------}

-- Type-indexed type sequences

class HList l => HTypeIndexed l
instance HTypeIndexed HNil
instance (HOccursNot e l,HTypeIndexed l) => HTypeIndexed (HCons e l)


{-----------------------------------------------------------------------------}

-- HOccursNot lifted to TIPs

instance HOccursNot p l => HOccursNot p (TIP l)


{-----------------------------------------------------------------------------}

-- Type-indexed extension

hExtend' e (TIP l) = mkTIP (HCons e l)

{-

Valid type I

hExtend' :: (HTypeIndexed l, HOccursNot (HProxy e) l)
         => e -> TIP l -> TIP (HCons e l)  

Valid type II

*TIP> :t hExtend'
hExtend' :: forall l e.
            (HTypeIndexed (HCons e l)) =>
            e -> TIP l -> TIP (HCons e l)

-}


{-----------------------------------------------------------------------------}

-- Lift extension through HExtend

instance HTypeIndexed (HCons e l)
      => HExtend e (TIP l) (TIP (HCons e l))
 where
  hExtend = hExtend'


{-----------------------------------------------------------------------------}

-- Lifting previous operations


instance ( HAppend l l' l''
         , HTypeIndexed l''
         ) 
           => HAppend (TIP l) (TIP l') (TIP l'')
 where
  hAppend (TIP l) (TIP l') = mkTIP (hAppend l l')


instance HOccursMany e l
      => HOccursMany e (TIP l)
 where
  hOccursMany = hOccursMany . unTIP


instance HOccursMany1 e l
      => HOccursMany1 e (TIP l)
 where
  hOccursMany1 = hOccursMany1 . unTIP


instance HOccursFst e l
      => HOccursFst e (TIP l)
 where
  hOccursFst = hOccursFst . unTIP


instance HOccursOpt e l
      => HOccursOpt e (TIP l)
 where
  hOccursOpt = hOccursOpt . unTIP


instance HOccurs e l
      => HOccurs e (TIP l)
 where
  hOccurs = hOccurs . unTIP


instance HLookup e l
      => HLookup e (TIP l)
 where
  hLookup = hLookup . unTIP


{-----------------------------------------------------------------------------}

-- Shielding type-indexed operations

onTIP :: HTypeIndexed a => (b -> a) -> TIP b -> TIP a
onTIP f (TIP l) = mkTIP (f l)

tipyDelete  t p  = onTIP (flip hDeleteByProxy p) t
tipyUpdate  t e  = onTIP (flip hUpdateByType e) t
tipyProject t ps = onTIP (flip hProjectByProxies ps) t


-- Split produces two TIPs

tipySplit (TIP l) ps = (mkTIP l',mkTIP l'')
 where
  (l',l'') = hSplitByProxies l ps


{-----------------------------------------------------------------------------}

-- Complement of the HOccursNot class

class HBoundType e r
instance HBoundType e (HCons e l)
instance HBoundType e l => HBoundType e (HCons e' l)


-- Class to compute a Boolean for "free type" status

class HBool b => HOccursNotStatus e l b | e l -> b
class (HBool b, HBool b') => HOccursNotStatus' b e l b' | b e l -> b'
instance HOccursNotStatus e HNil HTrue
instance ( TypeEq e e' b
         , HOccursNotStatus' b e l b'
         )
      =>   HOccursNotStatus e (HCons e' l) b'
instance HOccursNotStatus' HTrue e l HFalse
instance HOccursNotStatus e l b
      => HOccursNotStatus' HFalse e l b


{-----------------------------------------------------------------------------}

-- Subtyping for TIPs

instance SubType (TIP l) (TIP HNil)
instance (HBoundType e l, SubType (TIP l) (TIP l'))
      =>  SubType (TIP l) (TIP (HCons e l'))


{-----------------------------------------------------------------------------}

-- Sample code

{-

Assume

myTipyCow = TIP myAnimal

animalKey :: (HOccurs Key l, SubType l (TIP Animal)) => l -> Key
animalKey = hOccurs

Session log

*TIP> :t myTipyCow
myTipyCow :: TIP Animal

*TIP> hOccurs myTipyCow :: Breed
Cow

*TIP> hExtend BSE myTipyCow
TIP (HCons BSE 
    (HCons (Key 42)
    (HCons (Name "Angus")
    (HCons Cow
    (HCons (Price 75.5)
     HNil)))))

*TIP> BSE .*. myTipyCow
--- same as before ---

*TIP> Sheep .*. myTipyCow
Type error ...

*TIP> Sheep .*. tipyDelete myTipyCow (HProxy::HProxy Breed)
TIP (HCons Sheep (HCons (Key 42) (HCons (Name "Angus") (HCons (Price 75.5) HNil))))

*TIP> tipyUpdate myTipyCow Sheep
TIP (HCons (Key 42) (HCons (Name "Angus") (HCons Sheep (HCons (Price 75.5) HNil))))

-}

{-----------------------------------------------------------------------------}
