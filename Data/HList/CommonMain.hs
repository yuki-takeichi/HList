{- |

   Description : import me
   Copyright   : (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   The HList library

   This module re-exports everything needed to use HList.

-}

module Data.HList.CommonMain (

 -- * Faking dependent types in Haskell
   module Data.HList.FakePrelude

 -- * Functions for all collections
 , module Data.HList.HListPrelude
 -- * Array-like access to HLists
 , module Data.HList.HArray
 -- * Result-type-driven operations
 , module Data.HList.HOccurs
 -- * Type-indexed operations
 , module Data.HList.HTypeIndexed

 -- * HList
 -- | A subset of "Data.HList.HList" is re-exported.
 , module Data.HList.HList
 , module Data.HList.HZip

 -- ** A subset of "Data.HList.HCurry"
 , HCurry'(..)
 , hCurry, hUncurry
 , hCompose

 -- * Conversions between collections
 -- $convention the foo' optic has the same type as
 -- @Control.Lens.simple . foo . Control.Lens.simple@.
 -- 'hLens'' is an exception to this rule.

 -- ** HList and Record
 -- | 'unlabeled' 'unlabeled''

  -- ** Newtype wrappers
 -- $convention these isos unwrap/wrap the newtypes 'TIP' 'TIC' and
 -- 'Record'. Names follow the pattern @fromTo :: Iso' From To@.

 -- | Overlapping instances are restricted to here
 , module Data.HList.TypeEqO

) where

import Data.HList.FakePrelude
import Data.HList.HListPrelude
import Data.HList.HArray
import Data.HList.HOccurs
import Data.HList.HTypeIndexed
-- import Data.HList.RecordOrd
import Data.HList.HList hiding (append',
                                hAppend',
                                FHCons(..),
                                hMapAux,
                                MapCar(..),
                                hMapMapCar,
                                hSequence2,
                                )
import Data.HList.HCurry
import Data.HList.TypeEqO hiding (IsKeyFN)

import Data.HList.HZip
