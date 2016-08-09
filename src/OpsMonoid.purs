module OpsMonoid where

import Data.Semigroup (class Semigroup, append)
import Data.Monoid (class Monoid, mempty)

newtype I8 = I8 Int
newtype I16 = I16 Int

data Reg8
  = A
  | B
  | C
  | D
  | E
  | H
  | L
  | F

data Reg16
  = PC
  | SP
  | BC
  | DE
  | HL

-- http://clrhome.org/table/
newtype OpsMonoid =
  OpsMonoid
    (forall f. (Monoid f) =>
         {
           nop :: f,                             -- nop
           ldReg16Imm16 :: Reg16 -> I16 -> f,    -- ld bc,**
           ldMemReg16Reg8 :: Reg16 -> Reg8 -> f, -- ld (bc),a
           incReg16 :: Reg16 -> f,               -- inc bc
           incReg8 :: Reg8 -> f                  -- inc b
         }
      -> f
    )

instance semigroupOpsMonoid :: Semigroup OpsMonoid where
  append (OpsMonoid f1) (OpsMonoid f2) = OpsMonoid (\impl -> append (f1 impl) (f2 impl))

instance monoidOpsMonoid :: Monoid OpsMonoid where
  mempty = OpsMonoid (\_ -> mempty)
