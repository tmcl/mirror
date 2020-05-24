{-# Language CPP #-}

module Resources where

import qualified Foreign.Ptr
import qualified System.Win32

#include "resource.h"

idiAppIcon :: System.Win32.LPCTSTR
idiAppIcon = Foreign.Ptr.intPtrToPtr IDI_APPICON