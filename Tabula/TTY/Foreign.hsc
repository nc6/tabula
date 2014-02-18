{-
Copyright (c) 2014 Genome Research Ltd.

Author: Nicholas A. Clarke <nicholas.clarke@sanger.ac.uk>

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <http://www.gnu.org/licenses/>.
-}
{-# LANGUAGE ForeignFunctionInterface, CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}

#ifdef OS_MAC
  #include <sys/ttycom.h>
#elif defined OS_LINUX
  #include <asm-generic/ioctls.h>
#endif
#include <sys/ioctl.h>

module Tabula.TTY.Foreign where
  import Control.Applicative
  import Control.Monad (liftM)

  import Foreign.Storable
  import Foreign.C

  import System.Posix.IOCtl

  #let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

  data WindowSize = WindowSize {
      ws_row :: Int
    , ws_col :: Int
    , ws_xpixel :: Int
    , ws_ypixel :: Int
  } deriving (Eq, Show)

  instance Storable WindowSize where
    sizeOf _ = (#size struct winsize)
    alignment _ = (#alignment struct winsize)
    peek p = WindowSize
        <$> liftM fromCUShort ((#peek struct winsize, ws_row) p)
        <*> liftM fromCUShort ((#peek struct winsize, ws_col) p)
        <*> liftM fromCUShort ((#peek struct winsize, ws_xpixel) p)
        <*> liftM fromCUShort ((#peek struct winsize, ws_ypixel) p)
      where fromCUShort = fromIntegral :: CUShort -> Int
    poke p x = do
        (#poke struct winsize, ws_row) p (toCUShort $ ws_row x)
        (#poke struct winsize, ws_col) p (toCUShort $ ws_col x)
        (#poke struct winsize, ws_xpixel) p (toCUShort $ ws_xpixel x)
        (#poke struct winsize, ws_ypixel) p (toCUShort $ ws_ypixel x)
      where toCUShort = fromIntegral :: Int -> CUShort

  data TIOCGWINSZ = TIOCGWINSZ

  instance IOControl TIOCGWINSZ WindowSize where
    ioctlReq _ = #const TIOCGWINSZ

  data TIOCSWINSZ = TIOCSWINSZ

  instance IOControl TIOCSWINSZ WindowSize where
    ioctlReq _ = #const TIOCSWINSZ

  data TIOCSCTTY = TIOCSCTTY

  instance IOControl TIOCSCTTY Int where
    ioctlReq _ = #const TIOCSCTTY