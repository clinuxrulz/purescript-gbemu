module Types where

import Prelude
import Data.Sequence
import Data.Array

type Z80State =
  { mem :: Mem
  , totalM :: I8
  , halt :: Boolean
  , stop :: Boolean
  }

type Mem =
  { regs :: Regs
  , mainMem :: MainMem
  , svdRegs :: SavedRegs
  }

adjMainMem :: (MainMem -> MainMem) -> Mem -> Mem
adjMainMem f m = m { mainMem = f m.mainMem }

adjRegs :: (Regs -> Regs) -> Mem -> Mem
adjRegs f m = m { regs = f m.regs }

--NOTE: consider turning into a typeclass so that you can hide the
--underlying type that is used
-- NOTE: Change all read-only memory sections to be javascript arrays.
-- Fill them at the beginning using an ST Array computation.
newtype MainMem = MainMem
  { biosMapped   :: Boolean
  , bios         :: Array I8
  , imeEnableCnt :: Int
  , ime          :: Boolean -- Interrupts master enable flag
  , intE         :: I8 -- Interrupt enable flags
  , intF         :: I8 -- Interrupt flags
  , gpu          :: Gpu -- NOTE: might want to reconsider this
  , rom          :: MemSection
  , eram         :: MemSection
  , wram         :: MemSection
  , zram         :: MemSection
  }

foreign import data MemAccess :: !
foreign import data MemSection :: *

type Gpu = 
  { mTimer    :: Int
  , dispOn    :: Boolean
  , bgOn      :: Boolean
  , bgMap1    :: Boolean
  , bgSet1    :: Boolean
  , vblFinish :: Boolean
  , currLine  :: Int
  , currPos   :: Int
  , yScroll   :: Int
  , xScroll   :: Int
  , vblIntrr  :: Boolean
  , palette   :: Palette
  , mode      :: GpuMode
  , tiles     :: Tiles
  , regs      :: MemSection
  , vram      :: MemSection
  , oam       :: MemSection
  }

type Color =
  { a :: I8
  , r :: I8
  , g :: I8
  , b :: I8
  }

newtype Tiles = Tiles MemSection

type Palette = MemSection

data Channel = Alpha
             | Red
             | Green
             | Blue

data GpuMode = HBlank
             | VBlank
             | OamScan
             | VramScan
derive instance eqGpuMode :: Eq GpuMode

newtype OpCodeMap = OpCodeMap (Array (Z80State -> Z80State))

type Regs =
  { pc  :: I16
  , sp  :: I16
  , a   :: I8
  , b   :: I8
  , c   :: I8
  , d   :: I8
  , e   :: I8
  , h   :: I8
  , l   :: I8
  , f   :: I8
  , brTkn :: Boolean
  }

type GetReg = Regs -> I8
type SetReg = I8 -> Regs -> Regs

type SavedRegs =
  { a :: I8
  , b :: I8
  , c :: I8
  , d :: I8
  , e :: I8
  , h :: I8
  , l :: I8
  , f :: I8
  }

{--pc, sp, a, b, c, d, e, h, l, f :: Regs -> Number--}
pc = _.pc
sp = _.sp
a  = _.a 
b  = _.b 
c  = _.c 
d  = _.d 
e  = _.e 
h  = _.h 
l  = _.l 
f  = _.f  

{--setA, setB, setC, setD, setE, setH, setL, setF :: Number -> Regs -> Regs--}
setA x = _ { a = x }
setB x = _ { b = x }
setC x = _ { c = x }
setD x = _ { d = x }
setE x = _ { e = x }
setH x = _ { h = x }
setL x = _ { l = x }
setF x = _ { f = x }

--NOTE:: If you make these types 'newtype's you gain compile-time safety
--at the cost of verbosity. Consider doing the change
type I8 = Int
type I16 = Int
