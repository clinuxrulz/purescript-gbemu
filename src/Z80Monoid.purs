module Z80Monoid where

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
  | AF
  | BC
  | DE
  | HL

-- http://www.pastraiser.com/cpu/gameboy/gameboy_opcodes.html
-- left to right, top to bottom
newtype Z80Monoid =
  Z80Monoid
    (forall f. (Monoid f) =>
                                                 -- 0x

         { nop :: f                              -- NOP
         , ldReg16Imm16 :: Reg16 -> I16 -> f     -- LD BC,d16
         , ldMemReg16Reg8 :: Reg16 -> Reg8 -> f  -- LD (BC),A
         , incReg16 :: Reg16 -> f                -- INC BC

         , incReg8 :: Reg8 -> f                  -- INC B
         , decReg8 :: Reg8 -> f                  -- DEC B
         , ldReg8Imm8 :: Reg8 -> I8 -> f         -- LD B,d8
         , rlca :: f                             -- RLCA

         , ldMemReg8Reg16 :: Reg8 -> Reg16 -> f  -- LD (a16),SP
         , add16 :: Reg16 -> Reg16 -> f          -- ADD HL,BC
         , ldReg8MemReg16 :: Reg8 -> Reg16 -> f  -- LD A,(BC)
         , decReg16 :: Reg16 -> f                -- DEC BC

         -- covered already                      -- INC C
         -- covered already                      -- DEC C
         -- covered already                      -- LD C,d8
         , rrca :: Reg16 -> f                    -- RRCA
         }
      -> f
    )

instance semigroupZ80Monoid :: Semigroup Z80Monoid where
  append (Z80Monoid f1) (Z80Monoid f2) = Z80Monoid (\impl -> append (f1 impl) (f2 impl))

instance monoidZ80Monoid :: Monoid Z80Monoid where
  mempty = Z80Monoid (\_ -> mempty)
