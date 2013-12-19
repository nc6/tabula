module Tabula.TTY where
  import Control.Exception.Base (bracket_)

  import System.Posix.IO (openFd, OpenMode(ReadWrite), defaultFileFlags)
  import System.Posix.Terminal 
  import System.Posix.Types (Fd(..))

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
  setRaw oldAttrs = withoutModes rawModes oldAttrs where
    rawModes = [ProcessInput, KeyboardInterrupts, ExtendedFunctions, 
                EnableEcho, InterruptOnBreak, MapCRtoLF, IgnoreBreak, 
                IgnoreCR, MapLFtoCR, CheckParity, StripHighBit, 
                StartStopOutput, MarkParityErrors, ProcessOutput]
    withoutModes modes tty = foldl withoutMode tty modes

  cloneAttr :: Fd -> Fd -> IO ()
  cloneAttr from to = getTerminalAttributes from >>= 
    \a -> setTerminalAttributes to a Immediately