module Z80Monoid where

import Prelude
import Control.Monad.Rec.Class (tailRec)
import Data.Either (Either(Left,Right))
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

data Z80MonoidResult f
  = Z80Empty
  | Z80Single f
  | Z80Append (Unit -> Z80MonoidResult f) f

data Flag
  = FZ -- Zero Flag
  | FN -- Subtract Flag
  | FH -- Half Carry Flag
  | FC -- Curry Flag

data Condition
  = Flag Flag
  | NotFlag Flag

-- http://www.pastraiser.com/cpu/gameboy/gameboy_opcodes.html
-- left to right, top to bottom
newtype Z80MonoidImpl f =
  Z80MonoidImpl
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

                                            -- 2x

    , jrCondImm8 :: Condition -> I8 -> f    -- JR NZ,r8
    -- covered alreay                       -- LD HL,d16
    , ldiMemReg16Reg8 :: Reg16 -> Reg8 -> f -- LD (HL+),A
    -- covered already                      -- INC HL

    -- covered already                      -- INC H
    -- covered already                      -- DEC H
    -- covered already                      -- LD H,d8
    , daa :: f                              -- DAA

    -- covered already                      -- JR Z,r8
    -- covered already                      -- ADD HL,HL
    , ldiReg8MemReg16 :: Reg8 -> Reg16 -> f -- LD A,(HL+)
    -- covered already                      -- DEC HL

    -- covered already                      -- INC L
    -- covered already                      -- DEC L
    -- covered already                      -- LD L,d8
    , cpl :: f                              -- CPL
    }

newtype Z80Monoid = Z80Monoid (forall f. Z80MonoidImpl f -> Z80MonoidResult f)

instance semigroupZ80Monoid :: Semigroup Z80Monoid where
  append (Z80Monoid f1) (Z80Monoid f2) = Z80Monoid (\impl -> z80ResultAppend (f1 impl) (f2 impl))

instance monoidZ80Monoid :: Monoid Z80Monoid where
  mempty = Z80Monoid (\_ -> Z80Empty)

z80ResultAppend :: forall f. Z80MonoidResult f -> Z80MonoidResult f -> Z80MonoidResult f
z80ResultAppend Z80Empty x = x
z80ResultAppend x Z80Empty = x
z80ResultAppend x (Z80Single f) = Z80Append (\_ -> x) f
z80ResultAppend x (Z80Append k f) = Z80Append (\_ -> z80ResultAppend x (k unit)) f

runZ80Monoid :: forall f. (Monoid f) => Z80Monoid -> Z80MonoidImpl f -> f
runZ80Monoid (Z80Monoid z80) impl = tailRec go (z80 impl)
  where
    go :: Z80MonoidResult f -> Either (Z80MonoidResult f) f
    go Z80Empty = Right $ mempty
    go (Z80Single f) = Right $ f
    go (Z80Append thunk f) =
      case (thunk unit) of
        Z80Empty -> Right $ f
        Z80Single f2 -> Right $ f2 <> f
        Z80Append thunk2 f2 -> Left $ Z80Append thunk2 (f2 <> f)

nop :: Z80Monoid
nop = Z80Monoid (\(Z80MonoidImpl { nop: x }) -> Z80Single $ x)

ldReg16Imm16 :: Reg16 -> I16 -> Z80Monoid
ldReg16Imm16 reg16 imm16 = Z80Monoid (\(Z80MonoidImpl { ldReg16Imm16: x }) -> Z80Single $ x reg16 imm16)

ldMemReg16Reg8 :: Reg16 -> Reg8 -> Z80Monoid
ldMemReg16Reg8 reg16 reg8 = Z80Monoid (\(Z80MonoidImpl { ldMemReg16Reg8: x }) -> Z80Single $ x reg16 reg8)

incReg16 :: Reg16 -> Z80Monoid
incReg16 reg16 = Z80Monoid (\(Z80MonoidImpl { incReg16: x }) -> Z80Single $ x reg16)

incReg8 :: Reg8 -> Z80Monoid
incReg8 reg8 = Z80Monoid (\(Z80MonoidImpl { incReg8: x }) -> Z80Single $ x reg8)

decReg8 :: Reg8 -> Z80Monoid
decReg8 reg8 = Z80Monoid (\(Z80MonoidImpl { decReg8: x }) -> Z80Single $ x reg8)

ldReg8Imm8 :: Reg8 -> I8 -> Z80Monoid
ldReg8Imm8 reg8 imm8 = Z80Monoid (\(Z80MonoidImpl { ldReg8Imm8: x }) -> Z80Single $ x reg8 imm8)

rlca :: Z80Monoid
rlca = Z80Monoid (\(Z80MonoidImpl { rlca: x }) -> Z80Single $ x)

stop :: Z80Monoid
stop = Z80Monoid (\(Z80MonoidImpl { stop: x }) -> Z80Single $ x)

rla :: Z80Monoid
rla = Z80Monoid (\(Z80MonoidImpl { rla: x }) -> Z80Single $ x)

jrImm8 :: I8 -> Z80Monoid
jrImm8 imm8 = Z80Monoid (\(Z80MonoidImpl { jrImm8: x }) -> Z80Single $ x imm8)

rra :: Z80Monoid
rra = Z80Monoid (\(Z80MonoidImpl { rra: x }) -> Z80Single $ x)

jrCondImm8 :: Condition -> I8 -> Z80Monoid
jrCondImm8 cond imm8 = Z80Monoid (\(Z80MonoidImpl { jrCondImm8: x }) -> Z80Single $ x cond imm8)

ldiMemReg16Reg8 :: Reg16 -> Reg8 -> Z80Monoid
ldiMemReg16Reg8 reg16 reg8 = Z80Monoid (\(Z80MonoidImpl { ldiMemReg16Reg8: x }) -> Z80Single $ x reg16 reg8)

daa :: Z80Monoid
daa = Z80Monoid (\(Z80MonoidImpl { daa: x }) -> Z80Single $ x)

ldiReg8MemReg16 :: Reg8 -> Reg16 -> Z80Monoid
ldiReg8MemReg16 reg8 reg16 = Z80Monoid (\(Z80MonoidImpl { ldiReg8MemReg16: x }) -> Z80Single $ x reg8 reg16)

cpl :: Z80Monoid
cpl = Z80Monoid (\(Z80MonoidImpl { cpl: x }) -> Z80Single $ x)
