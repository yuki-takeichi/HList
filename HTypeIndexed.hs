{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fallow-undecidable-instances #-}

{- 

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Type-indexed operations on typeful heterogeneous lists.

-}


module HTypeIndexed where

import FakePrelude
import HListPrelude
import HListGoodies
import HArray
import HOccurs


{-----------------------------------------------------------------------------}

-- Map a type to a natural

class HNat n => HType2HNat l e n | l e -> n
 where
  hType2HNat :: l -> Proxy e -> n

instance ( TypeEqBool e' e b
         , HType2HNat' b l e n
         )
           => HType2HNat (HCons e' l) e n
 where
  hType2HNat (HCons e' l) p = n
   where
    b = proxyEqBool (proxy e') p
    n = hType2HNat' b l p 


-- Helper class

class (HBool b, HNat n) => HType2HNat' b l e n | b l e -> n
 where
  hType2HNat' :: b -> l -> Proxy e -> n

instance HFreeType e l
      => HType2HNat' HTrue l e HZero
 where
  hType2HNat' _ _ _ = HZero

instance HType2HNat l e n
      => HType2HNat' HFalse l e (HSucc n)
 where
  hType2HNat' _ l p = HSucc (hType2HNat l p)


-- Map types to naturals

class HTypes2HNats l ps ns | l ps -> ns
 where
  hTypes2HNats :: l -> ps -> ns

instance HTypes2HNats l HNil HNil
 where
  hTypes2HNats _ _ = HNil

instance ( HType2HNat   l e n
         , HTypes2HNats l ps ns
         )
      =>   HTypes2HNats l (HCons (Proxy e) ps) (HCons n ns)
 where
  hTypes2HNats l (HCons p ps) = HCons (hType2HNat l p) (hTypes2HNats l ps)


{-----------------------------------------------------------------------------}

-- Define type-indexed delete in terms of the natural-based primitive

class HDeleteByProxy l e l' | l e -> l'
 where
  hDeleteByProxy :: l -> Proxy e -> l'

instance ( HType2HNat (HCons e l) e' n
         , HDeleteByHNat  (HCons e l) n  l'
         , TypeEqBool e e' b
         , HType2HNat' b l e' n
         )
      =>   HDeleteByProxy (HCons e l) e' l'
 where
  hDeleteByProxy = hDeleteByProxy'

hDeleteByProxy' l p
 =
   hDeleteByHNat l (hType2HNat l p)


{-----------------------------------------------------------------------------}

-- Define type-indexed update in terms of the natural-based update

class HUpdateByType l e
 where
  hUpdateByType :: l -> e -> l

instance ( HType2HNat (HCons e l) e' n
         , HUpdateByHNat (HCons e l) n e' (HCons e l)
         , TypeEqBool e e' b
         , HType2HNat' b l e' n
         )
      =>   HUpdateByType (HCons e l) e'
 where
  hUpdateByType = hUpdateByType'

hUpdateByType' l e
 =
   hUpdateByHNat l (hType2HNat l (proxy e)) e 


{-----------------------------------------------------------------------------}

-- Projection based on proxies

class HProjectByProxies l ps l' | l ps -> l'
 where
  hProjectByProxies :: l -> ps -> l'

instance HProjectByProxies HNil HNil HNil
 where
  hProjectByProxies HNil HNil = HNil

instance ( HTypes2HNats (HCons e l) ps ns
         , HProjectByHNats (HCons e l) ns l'
         )
      =>   HProjectByProxies (HCons e l) ps l'
 where 
  hProjectByProxies =  hProjectByProxies'

hProjectByProxies' l ps
 =
   hProjectByHNats l $ hTypes2HNats l ps


{-----------------------------------------------------------------------------}

-- Splitting based on proxies

class HSplitByProxies l ps l' l'' | l ps -> l' l''
 where
  hSplitByProxies :: l -> ps -> (l',l'')

instance HSplitByProxies HNil HNil HNil HNil
 where
  hSplitByProxies HNil HNil = (HNil,HNil)

instance ( HTypes2HNats (HCons e l) ps ns
         , ToHJust (HCons e l) (HCons (HJust e) l')
         , HSplitByHNats' (HCons (HJust e) l') ns ly ln
         , FromHJust ly l''
         , FromHJust ln l'''
         )
      =>   HSplitByProxies (HCons e l) ps l'' l'''
 where 
  hSplitByProxies =  hSplitByProxies'

hSplitByProxies' l ps
 =
   hSplitByHNats l $ hTypes2HNats l ps


{-----------------------------------------------------------------------------}

-- This example from the TIR paper challenges forgotten types.
-- Thanks to the HW 2004 reviewer who pointed out the value of this example.

tuple :: ( HOccursGrounded ex l
         , HDeleteByProxy l ex lx
         , HOccursGrounded ey lx
         , HOccursGrounded ey l
         , HDeleteByProxy l ey ly
         , HOccursGrounded ex ly
         )
      => l -> (ex,ey)
tuple l = let
              x  = hOccursGrounded l
              l' = hDeleteByProxy l (proxy x)
              y  = hOccursGrounded l'
          in (x,y)


-- A specific tuple
oneTrue :: HCons Int (HCons Bool HNil)
oneTrue =  HCons 1   (HCons True HNil)


-- A variation that propagates all involved types

tuple' :: ( HType2HNat l x n 
          , HDeleteByHNat l n l'
          , HOccursGrounded x l
          , HOccursGrounded y l'
          )
            => l -> (n,l',x,y)

tuple' l = let
   n  = hType2HNat l (proxy x)
   l' = hDeleteByHNat l n
   x  = hOccursGrounded l
   y  = hOccursGrounded l'
  in (n,l',x,y)


{-----------------------------------------------------------------------------}