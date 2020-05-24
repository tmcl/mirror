# mirror
reflex-mirror = Reflex.Win32, bindings between Reflex and Win32

A binding between Reflex (0.7.0) and Win32. Requires a custom branch of Win32 (for extra 
bindings - these should mostly get merged back into Win32 but I haven't got around to 
filing the pr's yet), see the included submodule.

## Install
Install `choco` first.

Then you should be able to do:

```powershell
choco install --version 8.8.3 ghc
choco install haskell-dev
(update path, etc)
cabal new-build counter
```

Since many things transitively depend on `Win32`, it will rebuild a lot of things. 

I encounter a problem rebuild time, complaining HsTimeConfig.h not being found, 
since it is generated in `dist-newstyle/.../include` but it only looks for it in 
other locations. I work around that by copying it into a location it does search 
for manually. (Perhaps due to https://github.com/haskell/cabal/issues/5223 which
has a fix marked as merged, but perhaps that only affects the future, whereas we
are in the past because reflex does not support ghc 8.10.)

```powershell
cabal get time-1.9.3
notepad cabal.project # append time-1.9.3
notepad time-1.9.3/time.cabal # delete all tests, since the cause a circular dependency
cp $env:PATH/dist-newstyle/build/x86_64-windows/ghc-8.8.3/time-1.9.3/build/lib/include/HsTimeConfig.h time-1.9.3/lib/include
```

Obviously, this requires you to build it twice: once it will fail but it will 
produce the HsTimeConfig.h file so you can copy it, and then you can use it.

It seems like there should be other ways to get that to work 
e.g. --extra-include-dirs, but I wasn't able to get that to work.

## other things, limitations etc
See the README.md in the package for more relevant info.

## Reflex.Host.Basic

Reflex.Host.Basic was invaluable in this process. I converted the Host1..5.hs files 
into Win32 to help me get there. Whether they still compile or not, I do not know, 
but they are included in the reflx-host-basic-Win32 folder for reference.

https://github.com/qfpl/reflex-basic-host/
