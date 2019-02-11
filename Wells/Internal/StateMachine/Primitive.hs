{-# LANGUAGE CPP #-}
--{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}


{-|
Module      : Wells.Internals.StateMachine.Primitive 
Description : Internal module for primitive definitions for Wells.
Copyright   : (c) Raciel Hernández, 2019
License     : SRU
Maintainer  : racherb@protonmail.com
Stability   : experimental
Portability : Portable

The @'StateMachine' a@ type.
Internal module for primitive definitions for Wells.


> import Wells.Internals.StateMachine.Primitive 

-}
module Wells.Internal.StateMachine.Primitive
  ( -- * The @Primitive@ type
    primitiveStateList,
    primitivePathwayOfStates
  ) where

  import Wells.Internal.StateMachine.Types 
    ( StateName, StateList, Pathway, PathwayName, Limit
    , Source, Target, PathwayOfStates
    , toStateList
    , toPathway, toPathwayOfStates
    )
  
  import qualified Data.Sequence as Sq

  -- | Primitive state list defined for the state machine

  primitiveStateList :: StateList
  primitiveStateList = toStateList stateList 

  {-@ stateList :: v:{SizeEqualTo v 10} @-}
  stateList :: [StateName]
  stateList = 
     [ "NEVERSTARTED"  
       -- ^ The primitive machine has never been initialized.
     , "RUNNING"
       -- ^ The status machine is running
     , "COMPLETED" 
       -- ^ The status machine has performed all assigned tasks throughout the life cycle of the contract.
     , "PAUSED" 
       -- ^ The primitive machine is paused.
     , "SUSPENDED"
     , "HALTED"
     , "DEADEND" 
       -- ^ The state machine has reached a dead end and no return path because the contract it executes has exceeded the limit on a given Pathway.
     , "SELFHEALING" 
       -- ^ State of self-repair of the machine state before the occurrence of failures or errors of execution.
     , "SELFORDERING" 
       -- ^Self-ordering state where the state machine performs scheduler, workload, refresh and history generation tasks and data compacts.
     , "UPKEEP" 
       -- ^ Maintenance status of the status machine. In this state a human operator can execute own maintenance tasks.
     ]
  
  {-@ assume Data.String.fromString :: x:_ -> {v:_ | v ~~ x} @-}
  
  -- | State primitive PathwayOfStates
  primitivePathwayOfStates :: PathwayOfStates
  primitivePathwayOfStates =  toPathwayOfStates pwOfS

  pwOfS = fmap (\(c, l, (s, t)) -> toPathway c l (s, t)) primPathwayL

  {-@ primPathwayL :: v:{SizeEqualTo v 15} @-}
  primPathwayL :: [(PathwayName, Limit, (Source, Target))]
  primPathwayL = 
               [ ("init",     0, ("NEVERSTARTED", "RUNNING"))
               , ("finish",   0, ("RUNNING", "COMPLETED"))
               , ("dead",     1, ("RUNNING", "DEADEND"))
               , ("pause",    0, ("RUNNING", "PAUSED"))
               , ("resume",   0, ("PAUSED", "RUNNING"))
               , ("suspend",  0, ("RUNNING", "SUSPENDED"))
               , ("restart",  0, ("SUSPENDED", "RUNNING"))
               , ("order",    0, ("SUSPENDED", "SELFORDERING"))
               , ("continue", 0, ("SELFORDERING", "RUNNING"))
               , ("stop",     0, ("RUNNING", "HALTED"))
               , ("start",    0, ("HALTED", "RUNNING"))
               , ("error",    0, ("RUNNING", "SELFHEALING"))
               , ("recovery", 0, ("SELFHEALING", "RUNNING"))
               , ("support",  0, ("SELFHEALING", "UPKEEP"))
               , ("solved",   0, ("UPKEEP", "RUNNING"))
               ]
  
  {-pwOfS :: [Pathway]
  pwOfS = 
        [ toPathway "init"     0 ("NEVERSTARTED", "RUNNING")
        , toPathway "finish"   0 ("RUNNING", "COMPLETED")
        , toPathway "dead"     1 ("RUNNING", "DEADEND")
        , toPathway "pause"    0 ("RUNNING", "PAUSED")
        , toPathway "resume"   0 ("PAUSED", "RUNNING")
        , toPathway "suspend"  0 ("RUNNING", "SUSPENDED")
        , toPathway "restart"  0 ("SUSPENDED", "RUNNING")
        , toPathway "order"    0 ("SUSPENDED", "SELFORDERING")
        , toPathway "continue" 0 ("SELFORDERING", "RUNNING")
        , toPathway "stop"     0 ("RUNNING", "HALTED")
        , toPathway "start"    0 ("HALTED", "RUNNING")
        , toPathway "error"    0 ("RUNNING", "SELFHEALING")
        , toPathway "recovery" 0 ("SELFHEALING", "RUNNING")
        , toPathway "support"  0 ("SELFHEALING", "UPKEEP")
        , toPathway "solved"   0 ("UPKEEP", "RUNNING")
        ]
  -}