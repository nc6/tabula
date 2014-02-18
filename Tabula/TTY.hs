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
module Tabula.TTY where
  import Control.Exception.Base (bracket_)

  import System.Log.Logger
  import System.Posix.IO (openFd, OpenMode(ReadWrite), defaultFileFlags, stdOutput)
  import System.Posix.IOCtl
  import System.Posix.Terminal 
  import System.Posix.Types (Fd(..))

  import Tabula.TTY.Foreign

  getControllingTerminal :: IO Fd
  getControllingTerminal = getControllingTerminalName >>= 
            \a -> openFd a ReadWrite Nothing defaultFileFlags

  bracketChattr :: Fd -> (TerminalAttributes -> TerminalAttributes) -> IO a -> IO a
  bracketChattr fd chattr action = do
      oldAttrs <- getTerminalAttributes fd
      bracket_ (initialise oldAttrs)
                (finalise oldAttrs)
                action
    where
      initialise oldAttrs = do
        let newAttrs = chattr oldAttrs
        setTerminalAttributes fd newAttrs Immediately
      finalise oldAttrs = setTerminalAttributes fd oldAttrs Immediately

  setRaw :: (TerminalAttributes -> TerminalAttributes)
  setRaw = withoutModes rawModes where
    rawModes = [ProcessInput, KeyboardInterrupts, ExtendedFunctions, 
                EnableEcho, InterruptOnBreak, MapCRtoLF, IgnoreBreak, 
                IgnoreCR, MapLFtoCR, CheckParity, StripHighBit, 
                StartStopOutput, MarkParityErrors, ProcessOutput]
    withoutModes modes tty = foldl withoutMode tty modes

  cloneAttr :: Fd -> Fd -> IO ()
  cloneAttr from to = getTerminalAttributes from >>= 
    \a -> setTerminalAttributes to a Immediately

  setWindowSize :: Fd -> IO ()
  setWindowSize fd = do
    winSize <- ioctl' stdOutput TIOCGWINSZ
    debugM "tabula" $ "Caught SIGWINCH - resizing window to:" ++ (show winSize)
    ioctl_ fd TIOCSWINSZ winSize
    ioctl' fd TIOCGWINSZ >>= debugM "tabula" . ("New window size: " ++ ) . show

  setControllingTerminal :: Fd -> IO ()
  setControllingTerminal fd = ioctl_ fd TIOCSCTTY 0