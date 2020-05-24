# mirror
reflex-mirror = Reflex.Win32, bindings between Reflex and Win32

A binding between Reflex (0.7.0) and Win32. Requires a custom branch of Win32, see the included submodule.

## Install
Install `choco` first.

Then you should be able to do:

```powershell
choco install --version 8.8.3 ghc
choco install haskell-dev
(update path, etc)
cabal new-run counter 
```

Since many things transitively depend on `Win32`, it will rebuild a lot of things. 
I encounter a problem rebuild time, complaining HsTimeConfig.h not being found, 
since it is generated in `dist-newstyle/.../include` but it only looks for it in 
other locations. I work around that by copying it into a location it does search 
for manually.

## other things, limitations etc
See the README.md in the package for more relevant info.

## Reflex.Host.Basic

Reflex.Host.Basic was invaluable in this process. I converted the Host1..5.hs files 
into Win32 to help me get there. Whether they still compile or not, I do not know, 
but they are included in the reflx-host-basic-Win32 folder for reference.

https://github.com/qfpl/reflex-basic-host/
