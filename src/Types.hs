{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types where

-- Implementing the system described in the cheatsheet of episode 9:
-- https://graphicallinearalgebra.net/2015/05/16/natural-numbers-diagrammatically/

-- Are Identity and Twist actually generators? Who cares :D
-- not sure what the definition of a generator is
data Generator
  = Identity --  ──
  | Twist    --  x
  | Add      --  =○-
  | Copy     --  -⬤=
  | Zero     --  ○-
  | Discard  --  -⬤
  deriving(Eq, Ord, Read, Show)

generators :: [ Generator ]
generators = [Identity, Twist, Add, Copy, Zero, Discard]

-- type/dimension (number of input & output ports)
ports :: Generator -> (Integer, Integer)
ports x = case x of
  Identity -> (1,1)
  Twist    -> (2,2)
  Add      -> (2,1)
  Copy     -> (1,2)
  Zero     -> (0,1)
  Discard  -> (1,0)

-- dual 
bizarroGenerator :: Generator -> Generator
bizarroGenerator g = case g of
  Identity -> Identity
  Twist    -> Twist
  Add      -> Copy
  Copy     -> Add
  Zero     -> Discard
  Discard  -> Zero

newtype Diagram a = Diagram { runDiagram :: [[a]] }
  deriving(Eq, Ord, Read, Show, Functor)
