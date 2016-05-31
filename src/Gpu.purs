module Gpu
  ( resetScreen
  , gpuStep
  , gpuRd8
  , gpuWr8
  , wrVRam
  , cleanGpu
  , module Graphics.Canvas
  ) where


import Prelude
import Control.Bind
import Control.Monad
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Data.Int
import Data.Int.Bits
import Data.Foldable
import Data.Array as A
import Data.Sequence as S
import Data.Maybe
import Graphics.Canvas

import Types
import Utils
import MemSection as M
import Debug


foreign import setCanvasPixelColor :: forall e. Int -> Int -> Int
                              -> Eff (canvas :: Canvas | e) Unit

{--foreign import setScreen :: forall e. Context2D--}
                         {---> Eff (canvas :: Canvas | e) Unit--}

foreign import setScreenArr :: forall e. Array I8 -> Context2D
                         -> Eff (canvas :: Canvas | e) Unit

modeDuration :: GpuMode -> Int
modeDuration = case _ of
  HBlank -> 51
  VBlank -> 114
  VramScan -> 43
  OamScan -> 20


cleanTile :: Tile
cleanTile = Tile $ S.fromFoldable $ A.replicate 64 0

getCanvas :: forall e. Eff (canvas :: Canvas | e) Context2D
getCanvas = do 
  Just canvasElem <- getCanvasElementById "screen"
  getContext2D canvasElem

--NOTE: could be redundant after scrBuf is gone
resetScreen :: forall e. Eff (canvas :: Canvas | e) Unit
resetScreen = setScreenArr arr =<< getCanvas
 where arr = A.replicate (160*144*4) 255

cleanGpu :: forall e. Gpu
cleanGpu =
  { mTimer : modeDuration VBlank - 16
  , dispOn : true
  , bgOn : true
  , bgMap1 : false
  , bgSet1 : true
  , scrBuf : S.empty :: S.Seq I8
  , vblFinish : true
  , currLine : pixHeight + 10 - 1
  , currPos : 0 
  , yScroll : 0
  , xScroll : 0
  , vblIntrr : false
  , palette : S.fromFoldable $ A.replicate 4 cleanColor
  , mode : VBlank
  , tiles : S.fromFoldable $ A.replicate 384 cleanTile
  , regs : M.getNew 0x40 0 --S.fromFoldable $ A.replicate 0x40 0
  , vram : M.getNew 8192 0 --S.fromFoldable $ A.replicate 8192 0
  , oam : M.getNew 160 0 --S.fromFoldable $ A.replicate 160 0
  }

cleanColor :: Color
cleanColor = {a:0,r:0,g:0,b:0}

--NOTE consider doing nothing if dispOn is false
gpuStep :: forall e. I8 -> Gpu -> Eff (canvas :: Canvas, ma :: MemAccess | e) Gpu
gpuStep _ gpu@{dispOn = false} = return gpu

gpuStep opTime gpu@{mTimer,mode,currLine,currPos,scrBuf,vblFinish} =
  map hblCurrLineEarlyReset eGpu'
 where
  mTimer' = mTimer + opTime

  eGpu' = if mTimer' < modeDuration mode
    then return gpu { mTimer = mTimer' }
    else case mode of 
      --TODO set True least significant interrupt flags bit
      HBlank -> 
        let doOnLastLine gp = if currLine /= pixHeight - 1 then return gp
              else do
                traceA "draw!"
                {--setScreen =<< getCanvas--}
                setScreenArr (seqToArray scrBuf) =<< getCanvas

                return $ gp { scrBuf = S.empty :: S.Seq I8 --psc forces me
                            , mode = VBlank
                            , vblIntrr = true
                            }
         in doOnLastLine $ gpu { mTimer = mTimer' - modeDuration HBlank
                             , currLine = currLine + 1
                             , currPos = currPos + bytesWidth
                             , mode = OamScan
                             }  
        
      VBlank -> do
        let setOnLastLine =
              if vblFinish
                -- 10 increments after last line of 143
                then _ { vblFinish = false
                       , currLine = 0
                       , currPos = 0
                       , mode = OamScan
                       }
                else id
        return $ setOnLastLine gpu { mTimer = mTimer' - modeDuration VBlank , currLine = currLine + 1 }
      OamScan -> return gpu { mTimer = mTimer' - modeDuration OamScan, mode = VramScan }
      VramScan ->
        let renderOrNot = if gpu.dispOn && gpu.bgOn then renderLine else return
        in renderOrNot gpu { mTimer = mTimer' - modeDuration VramScan, mode = HBlank }

hblCurrLineEarlyReset :: Gpu -> Gpu
hblCurrLineEarlyReset gpu@{mTimer = mTimer', currLine, mode} =
  if mode == VBlank && mTimer' >= 4 && currLine == pixHeight + 10 - 1
    then gpu { currLine = 0, vblFinish = true }
    else gpu

--Change condition of renderOrNot once object rendering is added
--And perhaps rename the current renderLine to renderBG and have renderLine
--be what calls renderBG and renderFG
{--renderLine :: Gpu -> forall e. Eff (canvas :: Canvas | e) Gpu--}
renderLine :: forall e. Gpu -> Eff (ma :: MemAccess | e) Gpu
renderLine gpu = do
  tileIx <- getTileIx gpu tileIxAddr
  foldRes <- A.foldM updateCanvas
                     { tho:tileHorizOff, scrBuf:gpu.scrBuf
                     , mho:memHorizOff, tix:tileIx }
                     (0 A... (pixWidth-1))

  return gpu {scrBuf = foldRes.scrBuf}
 where
  tileIxAddr = bgMapOff + memVertOff + memHorizOff
  --map #0 9800-9BFF, map #1 9C00-9FFF
  bgMapOff = if gpu.bgMap1 then 0x1C00 else 0x1800
  --Each tile's height is 3 bits long,
  --and each map row of tiles is 5 bits (32 tiles wide)
  memVertOff = (pixelVertOff `zshr` 3) `shl` 5
  --The map's height is 256 pixels long
  pixelVertOff = 255 .&. (gpu.currLine + gpu.yScroll)
  -- the map is 32 tiles wide
  memHorizOff = 31 .&. (gpu.xScroll `zshr` 3)
  --tile height is 8 pixels / 3 bits long
  tileVertOff = 7 .&. (gpu.currLine + gpu.yScroll)
  --tile width is 8 pixels / 3 bits wide
  tileHorizOff = 7 .&. gpu.xScroll

  updateCanvas {tho,mho,tix,scrBuf} i = do
    {--setCanvasPixelColor color.a (i*4)   gpu.currLine--}
    {--setCanvasPixelColor color.r (i*4+1) gpu.currLine--}
    {--setCanvasPixelColor color.g (i*4+2) gpu.currLine--}
    {--setCanvasPixelColor color.b (i*4+3) gpu.currLine--}
    {--return {tho:tho',mho:mho',tix:tix'}--}

    --Would've rewritten with a single 'if', if PureScript had supported
    --pattern matching in where clause
    let tho' = if lastTileRowPixel
          then 0
          else tho + 1
        mho' = if lastTileRowPixel
          then 31 .&. (1 + mho)
          else mho
    tix' <- if lastTileRowPixel
      then getTileIx gpu $ bgMapOff + memVertOff + mho'
      else return tix

    return $ {scrBuf:scrBuf', tho:tho',mho:mho',tix:tix'}
   where
    lastTileRowPixel = tho == 7

    scrBuf' = foldl S.snoc scrBuf [color.a,color.r,color.g,color.b]

    --NOTE trace error, if invalid color ix
    color = getFromSeq cleanColor colorIx gpu.palette
    colorIx = getTilePixel tho tileVertOff $ getTile tix gpu.tiles 

--NOTE: Is it necessary to check tix < 128? doesn't map tile #1 always
--use set #1? (which is from -128 to 127)
getTileIx :: forall e. Gpu -> I16 -> Eff (ma :: MemAccess | e) I8
getTileIx { vram, bgSet1 } addr = do
  tix <- vram M.!! addr
  return $ if (not bgSet1) then tix + 256 else tix

--TODO add a generic register sequence
gpuRd8 :: forall e. I16 -> Gpu -> Eff (ma :: MemAccess | e) I8
gpuRd8 addr gpu = case addr of
  0xFF40 -> return $ getCtrlFlags gpu
  0xFF42 -> return gpu.yScroll
  0xFF43 -> return gpu.xScroll
  0xFF44 -> return gpu.currLine
  0xFF47 -> return $ -1 --NOTE log this, if needed implement getPalette Seq{argb}->I8
  otherwise -> gpu.regs M.!! (addr - 0xFF40)

gpuWr8 :: forall e. I8 -> I16 -> Gpu -> Eff (ma :: MemAccess | e) Gpu
gpuWr8 i8 addr gpu = do
  setRegs
  return case addr of
    0xFF40 -> setCtrlFlags i8 gpu
    0xFF42 -> gpu { yScroll = i8 }
    0xFF43 -> gpu { xScroll = i8 }
    0xFF44 -> gpu { currLine = i8 }
    0xFF47 -> setPalette  
    otherwise -> gpu
 where
  setRegs = M.replace i8 (addr - 0xFF40) gpu.regs 
    {--_ { regs = S.replace i8 (addr - 0xFF40) gpu.regs }--}
  setPalette = gpu { palette = foldl stPl S.empty (0 A... 3) }
  --Every color is 2 bits wide
  --NOTE should this be reversed?
  stPl plt i = case (i8 `zshr` (i*2)) .&. 3 of
    0 -> S.snoc plt {a:255,r:255,g:255,b:255}
    1 -> S.snoc plt {a:192,r:192,g:192,b:255}
    2 -> S.snoc plt {a:96 ,r:96 ,g:96 ,b:255}
    3 -> S.snoc plt {a:0  ,r:0  ,g:0  ,b:255}
    otherwise -> plt -- NOTE log this

getCtrlFlags :: Gpu -> I8
getCtrlFlags gpu =  cf gpu.bgOn   0x01
                .|. cf gpu.bgMap1 0x08
                .|. cf gpu.bgSet1 0x10
                .|. cf gpu.dispOn 0x80
 where cf bool flag = if bool then flag else 0x00

setCtrlFlags :: I8 -> Gpu -> Gpu
setCtrlFlags ctrlFlags gpu = enableDisableOrNot gpu.dispOn dispOn'
  gpu
    { bgOn   = isFlagSet 0x01
    , bgMap1 = isFlagSet 0x08
    , bgSet1 = isFlagSet 0x10
    , dispOn = dispOn'
    }
 where
  isFlagSet flag = (flag .&. ctrlFlags) /= 0
  dispOn' = isFlagSet 0x80

  enableDisableOrNot true false = disableGpu
  enableDisableOrNot false true = enableGpu
  enableDisableOrNot _ _ = id

disableGpu :: Gpu -> Gpu
disableGpu gpu = gpu
  { mTimer = 0
  , currLine = 0
  , mode = HBlank
  , vblFinish = false
  , dispOn = false
  }

enableGpu :: Gpu -> Gpu
enableGpu gpu = gpu
  { mTimer = 0
  , mode = OamScan
  , dispOn = true
  }



wrVRam :: forall e. I8 -> I16 -> Gpu -> Eff (ma :: MemAccess | e) Gpu
wrVRam i8 addr gpu@{vram,tiles} = do
  vram' <- M.replace i8 addr' vram
  tiles' <- vramWriteToTiles addr' vram' tiles
  return gpu { vram = vram', tiles = tiles' }
 where
  addr' = 0x1FFF .&. addr
  
--------------TILE--------    R O W
-- b    b b b b    b b b b    b b b b
--Each tile is 8 * 8 pixels * 2 bits per pixel = 16 bytes
--Each row is 8 pixels * 2 bits per pixel = 16 bits = 2 bytes

--Total of 384 TILES from 0x8000 to 0x97FF

--NOTE:potential problem if we write from 0x9800 to 0x9FFF
--which is not the tiles. It is the 2 tile maps.
vramWriteToTiles :: forall e. I16 -> MemSection -> Tiles -> Eff (ma :: MemAccess | e) Tiles
vramWriteToTiles addr vram tiles = do
  tile' <- A.foldM updateColor tile (0 A... 7)
  return $ setTile tile' tileIx tiles
 where
  tile = getTile tileIx tiles
  -- Doesn't matter if a byte of LOWER color bit of 8 pixels or HIGHER have
  -- changed, I have to update those 8 pixels colors just the same.
  addr' = addr - (addr .&. 1)
  tileIx = (addr `zshr` 4) .&. 511
  y = (addr `zshr` 1) .&. 7

  updateColor :: forall e. Tile -> Int -> Eff (ma :: MemAccess | e) Tile
  updateColor t x = do
    highColors <- vram M.!! addr + 1
    lowColors <- vram M.!! addr
    let c =  (if (shiftMask .&. highColors) /= 0 then 2 else 0)
         .|. (if (shiftMask .&. lowColors) /= 0 then 1 else 0)
    return $ setTilePixel c x y t
   where
    shiftMask = 1 `shl` (7-x)

--NOTE Add a check that tile index is valid (0<=i<384)

setTile :: Tile -> Int -> Tiles -> Tiles
setTile t i ts = S.replace t i ts

--NOTES  replace fromMaybe with something that will log invalid indices
getTile :: Int -> Tiles -> Tile
getTile ix = getFromSeq cleanTile ix

getTilePixel :: Int -> Int -> Tile -> Int
getTilePixel x y (Tile s) = s !! (y*8 + x)

--NOTE Add a check that indices and color are valid (0<=i<8) (0<=c<4)
setTilePixel :: Int -> Int -> Int -> Tile -> Tile
setTilePixel c x y (Tile s) = Tile $ S.replace c (y*8 + x) s

pixWidth :: Int
pixWidth = 160

bytesWidth :: Int
bytesWidth = 160*4

pixHeight :: Int
pixHeight = 144
