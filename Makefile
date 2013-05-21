.PHONY: all
all:
	 ghc -O2 Main.hs
	 echo "now run:   ./Main [SOMEFOLDER] -o results.html > /dev/null"
