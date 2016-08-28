module Z80Impl.Strategy1 where

-- Strategy1: Zero param effectful thunk array.

--import Z80Monoid as Z80Monoid

import Prelude
import Control.Monad.Eff (Eff)

foreign import data Z80IO :: !
foreign import data Z80State :: *

foreign import nop :: Z80State -> forall e. Eff (z80io :: Z80IO | e) Unit

-- Z80 instruction intermediate representation
data Z80InstrIR =
  Z80InstrIR
    Int     -- ^ instruction size in bytes
    Int     -- ^ number of cycles the instruction should take
    (Z80State -> forall e. Eff (z80io :: Z80IO | e) Unit)

-- Z80 intermediate representation
-- A second sweap will translate jumps to memory locations into
-- jumps to indices.
data Z80IR =
  Z80IR
    (Array Z80InstrIR)
