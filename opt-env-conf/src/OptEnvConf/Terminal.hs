{-# LANGUAGE CPP #-}

module OptEnvConf.Terminal
  ( getTerminalCapabilitiesFromHandle,
  )
where

#if !defined(mingw32_HOST_OS)
import Text.Colour.Capabilities.FromEnv (getTerminalCapabilitiesFromHandle)
#else
import System.IO (Handle, hIsTerminalDevice)
import Text.Colour.Capabilities (TerminalCapabilities (..))

getTerminalCapabilitiesFromHandle :: Handle -> IO TerminalCapabilities
getTerminalCapabilitiesFromHandle h = do
  isTerm <- hIsTerminalDevice h
  pure $ if isTerm then With8BitColours else WithoutColours
  -- Note: This may be conservative. Modern Windows terminals (Windows 10+)
  -- support 24-bit RGB colors when VT processing is enabled.
#endif
