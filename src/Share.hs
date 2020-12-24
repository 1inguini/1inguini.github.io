-- | my custom Prelude (basicly re-exporting RIO with some things hided)
module Share
  ( module Data.Default.Class,
    module Data.Generics.Product,
    module GHC.Generics,
    module Optics,
    module RIO,
    module RIO.List,
  )
where

import Data.Default.Class
import Data.Generics.Product
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
import RIO.List hiding (uncons)
