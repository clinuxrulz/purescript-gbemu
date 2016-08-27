module Z80Monoid where

import Prelude
import Control.Monad.Rec.Class (tailRec)
import Data.Either (Either(Left,Right))
import Data.Monoid (class Monoid, mempty)

newtype I3 = I3 Int
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
    , addReg16Reg16 :: Reg16 -> Reg16 -> f  -- ADD HL,BC
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

                                            -- 3x

    -- covered already                      -- JR NC,r8
    -- covered already                      -- LD SP,d16
    , lddMemReg16Reg8 :: Reg16 -> Reg8 -> f -- LD (HL-),A
    -- covered already                      -- INC SP

    , incMemReg16 :: Reg16 -> f             -- INC (HL)
    , decMemReg16 :: Reg16 -> f             -- DEC (HL)
    , ldMemReg16Imm8 :: Reg16 -> I8 -> f    -- LD (HL),d8
    , scf :: f                              -- SCF

    -- covered already                      -- JR C,r8
    -- covered already                      -- ADD HL,SP
    , lddReg8MemReg16 :: Reg8 -> Reg16 -> f -- LD A,(HL-)
    -- covered already                      -- DEC SP

    -- covered already                      -- INC A
    -- covered already                      -- DEC A
    -- covered already                      -- LD A,d8
    , ccf :: f                              -- CCF

                                            -- 4x

    , ldReg8Reg8 :: Reg8 -> Reg8 -> f       -- LD B,B
    -- many covered upto 45
    , ldReg8MemReg16 :: Reg8 -> Reg16 -> f  -- LD B,(HL)
    -- many covered upto 6F

                                            -- 7x

    , ldMemReg16Reg8 :: Reg16 -> Reg8 -> f  -- LD (HL),B
    -- many covered upto 75
    , halt :: f                             -- HALT
    -- many covered upto 7F

                                            -- 8x

    , addReg8Reg8 :: Reg8 -> Reg8 -> f      -- ADD A,B
    -- many covered upto 85
    , addReg8MemReg16 :: Reg8 -> Reg16 -> f -- ADD A,(HL)
    -- covered already                      -- ADD A,A

    , adcReg8Reg8 :: Reg8 -> Reg8 -> f      -- ADC A,B
    -- many covered upto 8D
    , adcReg8MemReg16 :: Reg8 -> Reg16 -> f -- ADC A,(HL)
    -- covered already                      -- ADC A,A

                                            -- 9x

    , subReg8 :: Reg8 -> f                  -- SUB B
    -- many covered upto 95
    , subMemReg16 :: Reg16 -> f             -- SUB (HL)
    -- covered already                      -- SUB A

    , sbcReg8Reg8 :: Reg8 -> Reg8 -> f      -- SBC A,B
    -- many covered upto 9D
    , sbcReg8MemReg16 :: Reg8 -> Reg16 -> f -- SBC A,(HL)
    -- covered already                      -- SBC A,A

                                            -- Ax

    , andReg8 :: Reg8 -> f                  -- AND B
    -- many covered upto A5
    , andMemReg16 :: Reg16 -> f             -- AND (HL)
    -- covered already                      -- AND A

    , xorReg8 :: Reg8 -> f                  -- XOR B
    -- many covered upto AD
    , xorMemReg16 :: Reg16 -> f             -- XOR (HL)
    -- covered already                      -- XOR A

                                            -- Bx

    , orReg8 :: Reg8 -> f                   -- OR B
    -- many covered upto B5
    , orMemReg16 :: Reg16 -> f              -- OR (HL)
    -- covered already                      -- OR A

    , cpReg8 :: Reg8 -> f                   -- CP B
    -- many covered upto
    , cpMemReg16 :: Reg16 -> f              -- CP (HL)
    -- covered already                      -- CP A

                                            -- Cx

    , retCond :: Condition -> f             -- RET NZ
    , popReg16 :: Reg16 -> f                -- POP BC
    , jpCondImm16 :: Condition -> I16 -> f  -- JP NZ,a16
    , jpImm16 :: I16 -> f                   -- JP a16

    , callCondImm16 :: Condition -> I16 -> f -- CALL NZ,a16
    , pushReg16 :: Reg16 -> f               -- PUSH BC
    , addReg8Imm8 :: Reg8 -> I8 -> f        -- ADD A,d8
    , rstI8 :: I8 -> f                      -- RST 00H

    -- covered already                      -- RET Z
    , ret :: f                              -- RET
    -- covered already                      -- JP Z,a16
    -- PREFIX CB listed last

    -- covered already                      -- CALL Z,a16
    , callImm16 :: I16 -> f                 -- CALL a16
    , adcReg8Imm8 :: Reg8 -> I8 -> f        -- ADC A,d8
    -- covered already                      -- RST 08H

                                            -- Dx

    -- many covered upto D5

    , subImm8 :: I8 -> f                    -- SUB d8
    -- covered already                      -- RST 10H
    -- covered already                      -- RET C
    , reti :: f                             -- RETI

    -- many covered upto DD

    , sbcReg8Imm8 :: Reg8 -> I8 -> f        -- SBC A,d8
    -- covered already

                                            -- Ex

    , ldhMemImm8Reg8 :: I8 -> Reg8 -> f     -- LDH (a8),A
    -- covered already                      -- POP HL
    , ldMemImm8Reg8 :: I8 -> Reg8 -> f      -- LD (C),A
    -- blank                                --

    -- blank                                --
    -- covered already                      -- PUSH HL
    , andImm8 :: I8 -> f                    -- AND d8
    -- covered already                      -- RST 20H

    , addReg16Imm8 :: Reg16 -> I8 -> f      -- ADD SP,r8
    , jpMemReg16 :: Reg16 -> f              -- JP (HL)
    , ldMemImm16Reg8 :: I16 -> Reg8 -> f    -- LD (a16),A
    -- blank                                --

    -- blank                                --
    -- blank                                --
    , xorImm8 :: I8 -> f                    -- XOR d8
    -- covered already                      -- RST 28H

                                            -- Fx

    , ldhReg8MemImm8 :: Reg8 -> I8 -> f     -- LDH A,(a8)
    -- covered already                      -- POP AF
    , ldReg8MemReg8 :: Reg8 -> Reg8 -> f    -- LD A,(C)
    , di :: f                               -- DI

    -- blank                                --
    -- covered already                      -- PUSH AF
    , orImm8 :: I8 -> f                     -- OR d8
    -- covered already                      -- RST 30H

    , ldReg16Reg16PlusImm8 :: Reg16 -> Reg16 -> I8 -> f -- LD HL,SP+r8
    , ldReg16Reg16 :: Reg16 -> Reg16 -> f   -- LD SP,HL
    , ldReg8MemImm16 :: Reg8 -> I16 -> f    -- LD A,(a16)
    , ei :: f                               -- EI

    , cp8Imm8 :: I8 -> f                    -- CP d8
    -- covered already                      -- RST 38H

                                            -- CB 0x

    , rlcReg8 :: Reg8 -> f                  -- RLC B

    -- many covered upto CB 05

    , rlcMemReg16 :: Reg16 -> f             -- RLC (HL)
    -- covered already                      -- RLC A

    , rrcReg8 :: Reg8 -> f                  -- RRC B

    -- many covered upto CB 0D

    , rrcMemReg16 :: Reg16 -> f             -- RRC (HL)
    -- covered already                      -- RRC A

                                            -- CB 1x

    , rlReg8 :: Reg8 -> f                  -- RL B

    -- many covered upto CB 05

    , rlMemReg16 :: Reg16 -> f             -- RL (HL)
    -- covered already                      -- RL A

    , rrReg8 :: Reg8 -> f                  -- RR B

    -- many covered upto CB 1D

    , rrMemReg16 :: Reg16 -> f             -- RR (HL)
    -- covered already                      -- RR A

                                            -- CB 2x

    , slaReg8 :: Reg8 -> f                  -- SLA B

    -- many covered upto CB 25

    , slaMemReg16 :: Reg16 -> f             -- SLA (HL)
    -- covered already                      -- SLA A

    , sraReg8 :: Reg8 -> f                  -- SRA B

    -- many covered upto CB 2D

    , sraMemReg16 :: Reg16 -> f             -- SRA (HL)
    -- covered already                      -- SRA A

                                            -- CB 3x

    , swapReg8 :: Reg8 -> f                 -- SWAP B

    -- many covered upto CB 35

    , swapMemReg16 :: Reg16 -> f            -- SWAP (HL)
    -- covered already                      -- SWAP A

    , srlReg8 :: Reg8 -> f                  -- SRL B

    -- many covered upto CB 3D

    , srlMemReg16 :: Reg16 -> f             -- SRL (HL)
    -- covered already                      -- SRL A

                                            -- CB 4x

    , bitImm3Reg8 :: I3 -> Reg8 -> f        -- BIT 0,B
    , bitImm3MemReg16 :: I3 -> Reg16 -> f   -- BIT 0,(HL)

    -- many covered upto 7F

                                            -- CB 8x

    , resImm3Reg8 :: I3 -> Reg8 -> f        -- RES 0,B
    , resImm3MemReg16 :: I3 -> Reg16 -> f   -- RES 0,(HL)

    -- many covered upto BF

                                            -- CB Cx

    , setImm3Reg8 :: I3 -> Reg8 -> f        -- SET 0,B
    , setImm3MemReg16 :: I3 -> Reg16 -> f   -- SET 0,(HL)
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

lddMemReg16Reg8 :: Reg16 -> Reg8 -> Z80Monoid
lddMemReg16Reg8 reg16 reg8 = Z80Monoid (\(Z80MonoidImpl { lddMemReg16Reg8: x }) -> Z80Single $ x reg16 reg8)

incMemReg16 :: Reg16 -> Z80Monoid
incMemReg16 reg16 = Z80Monoid (\(Z80MonoidImpl { incMemReg16: x }) -> Z80Single $ x reg16)

decMemReg16 :: Reg16 -> Z80Monoid
decMemReg16 reg16 = Z80Monoid (\(Z80MonoidImpl { decMemReg16: x }) -> Z80Single $ x reg16)

ldMemReg16Imm8 :: Reg16 -> I8 -> Z80Monoid
ldMemReg16Imm8 reg16 imm8 = Z80Monoid (\(Z80MonoidImpl { ldMemReg16Imm8: x }) -> Z80Single $ x reg16 imm8)

scf :: Z80Monoid
scf = Z80Monoid (\(Z80MonoidImpl { scf: x }) -> Z80Single $ x)

lddReg8MemReg16 :: Reg8 -> Reg16 -> Z80Monoid
lddReg8MemReg16 reg8 reg16 = Z80Monoid (\(Z80MonoidImpl { lddReg8MemReg16: x }) -> Z80Single $ x reg8 reg16)

ccf :: Z80Monoid
ccf = Z80Monoid (\(Z80MonoidImpl { ccf: x }) -> Z80Single $ x)
