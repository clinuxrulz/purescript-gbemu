module Z80Monoid where

import Prelude
import Data.Monoid (class Monoid)

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

data Z80MonoidResult f
  = Z80Empty
  | Z80Single f
  | Z80Append (Unit -> Z80MonoidResult f) (Unit -> Z80MonoidResult f)

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

                                                 -- 1x

         , stop :: f                             -- STOP
         -- covered already                      -- LD DE,d16
         -- covered already                      -- LD (DE),A
         -- covered already                      -- INC DE

         -- covered already                      -- INC D
         -- covered already                      -- DEC D
         -- covered already                      -- LD D,d8
         , rla :: f                              -- RLA

         , jrImm8 :: I8 -> f                     -- JR r8
         -- covered already                      -- ADD HL,DE
         -- covered already                      -- LD A,(DE)
         -- covered already                      -- DEC DE

         -- covered already                      -- INC E
         -- covered already                      -- DEC E
         -- covered already                      -- LD E,d8
         , rra :: f                              -- RRA
         }
      -> Z80MonoidResult f
    )

instance semigroupZ80Monoid :: Semigroup Z80Monoid where
  append (Z80Monoid f1) (Z80Monoid f2) = Z80Monoid (\impl -> Z80Append (\_ -> f1 impl) (\_ -> f2 impl))

instance monoidZ80Monoid :: Monoid Z80Monoid where
  mempty = Z80Monoid (\_ -> Z80Empty)

nop :: Z80Monoid
nop = Z80Monoid (\{ nop: x } -> Z80Single $ x)

ldReg16Imm16 :: Reg16 -> I16 -> Z80Monoid
ldReg16Imm16 reg16 imm16 = Z80Monoid (\{ ldReg16Imm16: x } -> Z80Single $ x reg16 imm16)

ldMemReg16Reg8 :: Reg16 -> Reg8 -> Z80Monoid
ldMemReg16Reg8 reg16 reg8 = Z80Monoid (\{ ldMemReg16Reg8: x } -> Z80Single $ x reg16 reg8)

incReg16 :: Reg16 -> Z80Monoid
incReg16 reg16 = Z80Monoid (\{ incReg16: x } -> Z80Single $ x reg16)

incReg8 :: Reg8 -> Z80Monoid
incReg8 reg8 = Z80Monoid (\{ incReg8: x } -> Z80Single $ x reg8)

decReg8 :: Reg8 -> Z80Monoid
decReg8 reg8 = Z80Monoid (\{ decReg8: x } -> Z80Single $ x reg8)

ldReg8Imm8 :: Reg8 -> I8 -> Z80Monoid
ldReg8Imm8 reg8 imm8 = Z80Monoid (\{ ldReg8Imm8: x } -> Z80Single $ x reg8 imm8)

rlca :: Z80Monoid
rlca = Z80Monoid (\{ rlca: x } -> Z80Single $ x)

stop :: Z80Monoid
stop = Z80Monoid (\{ stop: x } -> Z80Single $ x)

rla :: Z80Monoid
rla = Z80Monoid (\{ rla: x } -> Z80Single $ x)

jrImm8 :: I8 -> Z80Monoid
jrImm8 imm8 = Z80Monoid (\{ jrImm8: x } -> Z80Single $ x imm8)

rra :: Z80Monoid
rra = Z80Monoid (\{ rra: x } -> Z80Single $ x)
