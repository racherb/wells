{-# LANGUAGE CPP #-}
--{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
--{-# LANGUAGE OverloadedStrings #-}


{-|
Module      : Wells.Internals.StateMachine.Types 
Description : Simple Range Data Types
Copyright   : (c) Raciel Hernández, 2019
License     : SRU
Maintainer  : racherb@protonmail.com
Stability   : experimental
Portability : Portable

The @'StateMachine' a@ type.
Internal module for types definitions for Wells.

> import Wells.Internals.StateMachine.Types 

-}
module Wells.Internal.StateMachine.Types 
  ( -- * The @Pathway@ type
    Pathway(..)
  , StateName
  , StateList
  , PathwayName
  , Limit
  , Source
  , Target
  , PathwayOfStates
  -- Conversion
  , toStateList
  , toPathway
  , toPathwayOfStates
  ) where
  
  import Prelude (String, Int, Show, Eq, Bool(..)
                 , fmap, filter, length
                 , ($), (/=))
  import qualified Data.Text     as Tx(Text, length)
  import qualified Data.Sequence as Sq(Seq, fromList)
  
  -- | Liquid Refinement Types and Predicates specification
  {-@ predicate LenGratherThan S N = (len S) >= N @-} 
  {-@ predicate NatBetween N I S 
      = (N >= 0) && (N >= I) && (N <= S)@-}
  
  {-@ type NonEmptyList  a    = {v:[a] | 0 < len v}       @-}
  {-@ type NonDuplicated a    = [a]<{\vi vj -> vi /= vj}> @-}

  {-@ type ShortName
      = {v:PathwayName | NatBetween (autolen v) 3 18} @-}

  type StateName   = Tx.Text       -- ^ State name
  type PathwayName = Tx.Text       -- ^ Pathway name
  type Source      = StateName     -- ^ State source (ini)
  type Target      = StateName     -- ^ State target (end)
  type Limit       = Int           -- ^ Limit of use for the Pathway

  type StateList = Sq.Seq StateName 
  -- ^ Definition for all valid States 
  type MainStateFlow = StateList
  -- ^ Main state sequence for regresive path 

  {-@ toStateList
      :: NonDuplicated StateName 
      -> StateList @-}
  toStateList :: [StateName] -> StateList
  toStateList x = (Sq.fromList x)::StateList

  -- | Pathway definition data type for change between one state to another
  data Pathway = Pathway 
    { concept   :: {-# UNPACK #-} !PathwayName -- ^ Conceptual name of trans
    , limit     :: {-# UNPACK #-} !Limit       -- ^ Max limit permited
    , pathway   :: {-# UNPACK #-} !(Source, Target) -- ^ A valid pathway
    } deriving (Show)

  type PathwayOfStates = Sq.Seq Pathway

  {-@ type ReasonableLimit 
      = {v:Limit | v >= 0 && v <= 20} @-}

  {-@ type NoRouteToHimself
      = {v:(_, _) | (fst v) /= (snd v)} @-}

  {-@ toPathway
      :: PathwayName
      -> ReasonableLimit
      -> NoRouteToHimself
      -> Pathway
   @-}
  toPathway 
    :: PathwayName 
    -> Limit 
    -> (Source, Target) 
    -> Pathway
  toPathway concept' limit' (source', target') 
    = Pathway concept' limit' (source', target')
  {-# INLINE toPathway #-}

  {-@ toPathwayOfStates :: v:[Pathway] -> o:PathwayOfStates @-}
  toPathwayOfStates :: [Pathway] -> PathwayOfStates
  toPathwayOfStates x = (Sq.fromList x)::PathwayOfStates
 
  type Gap = Int

  type HashCode = Tx.Text
  type BlockCode = Tx.Text
  type Level = Int

  data Journey = Journey
    { hash :: HashCode
    , block :: BlockCode
    , actual :: StateName
    , history :: StateName
    , level :: Int
    }















