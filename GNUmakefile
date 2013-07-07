# GNUmakefile

GHC             = ghc
GHCFLAGS        = -Wall -isrc

SOURCES        := $(shell find src/*/ -name '*.hs')
OBJECTS	       := $(SOURCES:.hs=.o)
INTERFACES     := $(SOURCES:.hs=.hi)

.PHONY:	  all clean depend

all::		data/haskell-packages/default.nix

data/haskell-packages/default.nix : src/pkglist data/packages.txt
	@mkdir -p data/haskell-packages
	src/pkglist data/packages.txt data/haskell-packages

src/pkglist :	src/pkglist.o $(OBJECTS)
	${GHC} ${GHCFLAGS} -o $@ $^ -package pretty -package Cabal -package regex-posix \
		-package HTTP -package hackage-db

%.o : %.hs
	${GHC} ${GHCFLAGS} -o $@ -c $<

%.hi : %.o
	@: do nothing

clean::
	@rm -f src/pkglist.o src/pkglist.hi
	@rm -f $(OBJECTS) $(INTERFACES)

depend:
	${GHC} -M -dep-makefile GNUmakefile $(GHCFLAGS) src/pkglist.hs

# DO NOT DELETE: Beginning of Haskell dependencies
src/Cabal2Nix/Flags.o : src/Cabal2Nix/Flags.hs
src/Cabal2Nix/CorePackages.o : src/Cabal2Nix/CorePackages.hs
src/Cabal2Nix/Name.o : src/Cabal2Nix/Name.hs
src/Distribution/NixOS/PrettyPrinting.o : src/Distribution/NixOS/PrettyPrinting.hs
src/Distribution/NixOS/Derivation/License.o : src/Distribution/NixOS/Derivation/License.hs
src/Distribution/NixOS/Derivation/License.o : src/Distribution/NixOS/PrettyPrinting.hi
src/Cabal2Nix/License.o : src/Cabal2Nix/License.hs
src/Cabal2Nix/License.o : src/Distribution/NixOS/Derivation/License.hi
src/Distribution/NixOS/Derivation/Meta.o : src/Distribution/NixOS/Derivation/Meta.hs
src/Distribution/NixOS/Derivation/Meta.o : src/Distribution/NixOS/Derivation/License.hi
src/Distribution/NixOS/Derivation/Meta.o : src/Distribution/NixOS/PrettyPrinting.hi
src/Distribution/NixOS/Derivation/Cabal.o : src/Distribution/NixOS/Derivation/Cabal.hs
src/Distribution/NixOS/Derivation/Cabal.o : src/Distribution/NixOS/PrettyPrinting.hi
src/Distribution/NixOS/Derivation/Cabal.o : src/Distribution/NixOS/Derivation/Meta.hi
src/Cabal2Nix/PostProcess.o : src/Cabal2Nix/PostProcess.hs
src/Cabal2Nix/PostProcess.o : src/Distribution/NixOS/Derivation/Cabal.hi
src/Cabal2Nix/Normalize.o : src/Cabal2Nix/Normalize.hs
src/Cabal2Nix/Normalize.o : src/Cabal2Nix/CorePackages.hi
src/Cabal2Nix/Normalize.o : src/Cabal2Nix/Name.hi
src/Cabal2Nix/Normalize.o : src/Distribution/NixOS/Derivation/Cabal.hi
src/Cabal2Nix/Hackage.o : src/Cabal2Nix/Hackage.hs
src/Cabal2Nix/Generate.o : src/Cabal2Nix/Generate.hs
src/Cabal2Nix/Generate.o : src/Distribution/NixOS/Derivation/Cabal.hi
src/Cabal2Nix/Generate.o : src/Cabal2Nix/Flags.hi
src/Cabal2Nix/Generate.o : src/Cabal2Nix/Normalize.hi
src/Cabal2Nix/Generate.o : src/Cabal2Nix/PostProcess.hi
src/Cabal2Nix/Generate.o : src/Cabal2Nix/License.hi
src/pkglist.o : src/pkglist.hs
src/pkglist.o : src/Distribution/NixOS/Derivation/Cabal.hi
src/pkglist.o : src/Cabal2Nix/Normalize.hi
src/pkglist.o : src/Cabal2Nix/Hackage.hi
src/pkglist.o : src/Cabal2Nix/Generate.hi
# DO NOT DELETE: End of Haskell dependencies
