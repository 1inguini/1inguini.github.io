-- | my custom Prelude (basicly re-exporting RIO with some things hided)
module Prelude
  ( module Data.Default.Class,
    module GHC.Generics,
    module Optics,
    module RIO,
  )
where

import Data.Default.Class
import GHC.Generics (Generic)
import Optics
import RIO hiding
  ( Lens (..),
    Lens' (..),
    for_,
    lens,
    over,
    preview,
    set,
    sets,
    to,
    view,
    (%~),
    (.~),
    (^.),
    (^..),
    (^?),
  )
