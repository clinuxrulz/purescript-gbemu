module MainMem where

import Prelude
import Math
import Data.Sequence as S
import Data.Maybe
import Data.Foldable
import Data.Int.Bits
import Data.Array as A
import Data.Tuple as T

import Gpu
import Types
import Utils

--NOTE: optimization:
--Most of the memory is not writable, so consider
--saving it in a javascript array.
--But let that be transparent behind the reading/writing
--functions interface, i.e. decide where to read from depending
--on the address that is given as input.

-- 8 Bit version

setRom :: Array I8 -> MainMem -> MainMem
setRom rom (MainMem mem) = MainMem $ mem { rom = rom }

--NOTE: toggle biosMapped when pc == 0x0100
rd8 :: I16 -> MainMem -> I8
rd8 addr (MainMem {biosMapped,bios,rom,eram,wram,zram, gpu = gpu@{vram,oam}}) =
  case 0xF000.&.addr of
    0x0000 -> if biosMapped && (addr < 0x0100)
             then fromMaybe 0 $ bios A.!! addr -- NOTE log error
             else fromMaybe 0 $ rom A.!! addr -- NOTE log error
    n | 0x1000 <= n && n <= 0x7000 -> fromMaybe 0 $ rom A.!! addr -- NOTE log err
    0x8000 -> vram !! (0x1FFF .&. addr)
    0x9000 -> vram !! (0x1FFF .&. addr)
    0xA000 -> eram !! (0x1FFF .&. addr)
    0xB000 -> eram !! (0x1FFF .&. addr)
    0xC000 -> wram !! (0x1FFF .&. addr)
    0xD000 -> wram !! (0x1FFF .&. addr)
    0xE000 -> wram !! (0x1FFF .&. addr)
    0xF000 ->
      case 0x0F00.&.addr of
        n | n < 0x0E00 -> wram !! (0x1FFF .&. addr)
        0x0E00 ->
          if addr < 0xFEA0 then oam !! (0xFF .&. addr) else 0
        0x0F00 ->
          case 0x00F0.&.addr of
            n | n >= 0x0080 -> zram !! (0x7F .&. addr)
            n | 0x0040 <= n && n <= 0x0070 -> gpuRd8 addr gpu
            --TODO temporary until key input is implemented
            0x00 -> if addr == 0xFF00 then 0xDF else -1
            otherwise -> -1 --NOTE log this
        otherwise -> -1 --NOTE log this
    otherwise -> -1 --NOTE log this

--NOTE: make sure the significant byte part sits at a higher address
rd16 :: I16 -> MainMem -> I16
rd16 addr mem = (h `shl` 8) + l
 where
  l = rd8 addr mem
  h = rd8 (addr + 1) mem

--NOTE: temporary implementation. Writing to parts that are not writable
--memory during execution should be treated with an error.
--The initial writing to them should be ST array computations,
--and not wr8 calls
wr8 :: I8 -> I16 -> MainMem -> MainMem
wr8 i8 addr (MainMem mem@{biosMapped,bios,rom,eram,wram,zram,gpu=gpu@{oam}}) =
  MainMem $ modifyMem mem
 where
  modifyMem =
    case 0xF000.&.addr of
      n | 0 <= n && n <= 7 -> id -- NOTE error can't write to bios or rom
      --{ vram = S.replace i8 (0x1FFF .&. addr) vram }
      0x8000 -> _ { gpu  = wrVRam i8 addr gpu }
      0x9000 -> _ { gpu  = wrVRam i8 addr gpu }
      0xA000 -> _ { eram = S.replace i8 (0x1FFF .&. addr) eram }
      0xB000 -> _ { eram = S.replace i8 (0x1FFF .&. addr) eram }
      0xC000 -> _ { wram = S.replace i8 (0x1FFF .&. addr) wram }
      0xD000 -> _ { wram = S.replace i8 (0x1FFF .&. addr) wram }
      0xE000 -> _ { wram = S.replace i8 (0x1FFF .&. addr) wram }
      0xF000 ->
        case 0x0F00.&.addr of
          n | n < 0x0E00 -> _ { wram = S.replace i8 (0x1FFF .&. addr) wram }
          0x0E00 ->
            if addr < 0xFEA0
              then _ { gpu = gpu { oam = S.replace i8 (0xFF .&. addr) oam } }
              else id --NOTE unwritable, log this
          0x0F00 ->
            case 0x00F0.&.addr of
              n | n >= 0x0080 -> _ { zram = S.replace i8 (0x7F .&. addr) zram }
              n | 0x0040 <= n && n <= 0x0070 -> _ { gpu = gpuWr8 i8 addr gpu }
              otherwise -> id --NOTE log this
          otherwise -> id --NOTE log this
      otherwise -> id --NOTE log this

wr16 :: I16 -> I16 -> MainMem -> MainMem
wr16 i16 addr mem = 
      wr8 h (addr + 1)
  <<< wr8 l addr
   $  mem
 where
  h = 255 .&. (i16 `zshr` 8)
  l = 255 .&. i16

cleanMainMem :: MainMem
cleanMainMem = initIOArea $ MainMem
  { biosMapped : false
  , rom  : A.singleton 0
  , eram : S.fromFoldable $ A.replicate 8192 0xFF
  , wram : S.fromFoldable $ A.replicate 8192  0
  , zram : S.fromFoldable $ A.replicate 128 0
  , gpu  : cleanGpu
  , bios :  [
    0x31, 0xFE, 0xFF, 0xAF, 0x21, 0xFF, 0x9F, 0x32,
    0xCB, 0x7C, 0x20, 0xFB, 0x21, 0x26, 0xFF, 0x0E,
    0x11, 0x3E, 0x80, 0x32, 0xE2, 0x0C, 0x3E, 0xF3,
    0xE2, 0x32, 0x3E, 0x77, 0x77, 0x3E, 0xFC, 0xE0,
    0x47, 0x11, 0x04, 0x01, 0x21, 0x10, 0x80, 0x1A,
    0xCD, 0x95, 0x00, 0xCD, 0x96, 0x00, 0x13, 0x7B,
    0xFE, 0x34, 0x20, 0xF3, 0x11, 0xD8, 0x00, 0x06,
    0x08, 0x1A, 0x13, 0x22, 0x23, 0x05, 0x20, 0xF9,
    0x3E, 0x19, 0xEA, 0x10, 0x99, 0x21, 0x2F, 0x99,
    0x0E, 0x0C, 0x3D, 0x28, 0x08, 0x32, 0x0D, 0x20,
    0xF9, 0x2E, 0x0F, 0x18, 0xF3, 0x67, 0x3E, 0x64,
    0x57, 0xE0, 0x42, 0x3E, 0x91, 0xE0, 0x40, 0x04,
    0x1E, 0x02, 0x0E, 0x0C, 0xF0, 0x44, 0xFE, 0x90,
    0x20, 0xFA, 0x0D, 0x20, 0xF7, 0x1D, 0x20, 0xF2,
    0x0E, 0x13, 0x24, 0x7C, 0x1E, 0x83, 0xFE, 0x62,
    0x28, 0x06, 0x1E, 0xC1, 0xFE, 0x64, 0x20, 0x06,
    0x7B, 0xE2, 0x0C, 0x3E, 0x87, 0xF2, 0xF0, 0x42,
    0x90, 0xE0, 0x42, 0x15, 0x20, 0xD2, 0x05, 0x20,
    0x4F, 0x16, 0x20, 0x18, 0xCB, 0x4F, 0x06, 0x04,
    0xC5, 0xCB, 0x11, 0x17, 0xC1, 0xCB, 0x11, 0x17,
    0x05, 0x20, 0xF5, 0x22, 0x23, 0x22, 0x23, 0xC9,
    0xCE, 0xED, 0x66, 0x66, 0xCC, 0x0D, 0x00, 0x0B,
    0x03, 0x73, 0x00, 0x83, 0x00, 0x0C, 0x00, 0x0D,
    0x00, 0x08, 0x11, 0x1F, 0x88, 0x89, 0x00, 0x0E,
    0xDC, 0xCC, 0x6E, 0xE6, 0xDD, 0xDD, 0xD9, 0x99,
    0xBB, 0xBB, 0x67, 0x63, 0x6E, 0x0E, 0xEC, 0xCC,
    0xDD, 0xDC, 0x99, 0x9F, 0xBB, 0xB9, 0x33, 0x3E,
    0x3c, 0x42, 0xB9, 0xA5, 0xB9, 0xA5, 0x42, 0x4C,
    0x21, 0x04, 0x01, 0x11, 0xA8, 0x00, 0x1A, 0x13,
    0xBE, 0x20, 0xFE, 0x23, 0x7D, 0xFE, 0x34, 0x20,
    0xF5, 0x06, 0x19, 0x78, 0x86, 0x23, 0x05, 0x20,
    0xFB, 0x86, 0x20, 0xFE, 0x3E, 0x01, 0xE0, 0x50
  ]
  }
 where
  ioMem = A.zip (0xFF00 A... 0xFF7F) [
    0xCF,0x00,0x7E,0xFF,0xAF,0x00,0x00,0xF8,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xE1,
    0x80,0xBF,0xF3,0xFF,0xBF,0xFF,0x3F,0x00,0xFF,0xBF,0x7F,0xFF,0x9F,0xFF,0xBF,0xFF,
    0xFF,0x00,0x00,0xBF,0x77,0xF3,0xF1,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
    0x06,0xFE,0x0E,0x7F,0x00,0xFF,0x58,0xDF,0x00,0xEC,0x00,0xBF,0x0C,0xED,0x03,0xF7,
    0x91,0x85,0x00,0x00,0x00,0x00,0x00,0xFC,0xFF,0xFF,0x00,0x00,0xFF,0x7E,0xFF,0xFE,
    0xFF,0x00,0x00,0x00,0x00,0xFF,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
    0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xC0,0x00,0xC1,0x00,0x00,0x00,0x00,0x00,
    0xF8,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
  ]
  initIOArea mm = foldl (\acc (T.Tuple addr b) -> wr8 b addr acc) mm ioMem

getGpu :: MainMem -> Gpu
getGpu (MainMem mm) = mm.gpu

setGpu :: Gpu -> MainMem -> MainMem
setGpu gpu' (MainMem mm) = MainMem $ mm { gpu = gpu' }


