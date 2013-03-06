#!/bin/sh

ghc --make -threaded Http-fs.hs \
	&& echo "* see: http://localhost:8080\n" \
	&& ./Http-fs