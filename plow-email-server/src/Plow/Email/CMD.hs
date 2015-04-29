{-# LANGUAGE DeriveDataTypeable #-}

module Plow.Email.CMD (   MyOptions (..)
                        , getOpts
                      ) where

import           System.Console.CmdArgs

data MyOptions = MyOptions
    {   port :: Maybe Int

    } deriving (Data, Typeable, Show, Eq)



-- Customize your options, including help messages, shortened names, etc.

myProgOpts :: MyOptions
myProgOpts = MyOptions
    { port = def &= help "use a different port number"
    }

getOpts :: IO MyOptions
getOpts = cmdArgs $ myProgOpts
    &= verbosityArgs [explicit, name "Verbose", name "V"] []
    &= versionArg [explicit, name "version", name "v", summary _PROGRAM_INFO]
    &= summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT)
    &= help _PROGRAM_ABOUT
    &= helpArg [explicit, name "help", name "h"]
    &= program _PROGRAM_NAME

_PROGRAM_NAME :: String
_PROGRAM_NAME = "AlarmLog"

_PROGRAM_VERSION :: String
_PROGRAM_VERSION = "0.1.0"

_PROGRAM_INFO :: String
_PROGRAM_INFO = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION

_PROGRAM_ABOUT :: String
_PROGRAM_ABOUT = "Plow Email Server"

_COPYRIGHT :: String
_COPYRIGHT = "(C) Plow Tech 2014"
