Index: haskell-lens-3.9.0.2/tests/doctests.hsc
===================================================================
--- haskell-lens-3.9.0.2.orig/tests/doctests.hsc	2013-06-15 13:40:30.329035355 +0200
+++ haskell-lens-3.9.0.2/tests/doctests.hsc	2013-06-15 13:40:30.325035355 +0200
@@ -54,9 +54,9 @@
 main :: IO ()
 main = withUnicode $ getSources >>= \sources -> doctest $
     "-isrc"
-  : "-idist/build/autogen"
+  : "-idist-ghc/build/autogen"
   : "-optP-include"
-  : "-optPdist/build/autogen/cabal_macros.h"
+  : "-optPdist-ghc/build/autogen/cabal_macros.h"
   : "-hide-all-packages"
   : map ("-package="++) deps ++ sources
 
