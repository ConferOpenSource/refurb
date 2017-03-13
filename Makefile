
.PHONY: update-build refurb.cabal

update-build: package.nix

package.nix: refurb.cabal
	rm -f package.nix
	nix-shell -p cabal2nix --run 'cabal2nix .' > package.nix

refurb.cabal:
	nix-shell -p haskellPackages.hpack --run 'hpack'
