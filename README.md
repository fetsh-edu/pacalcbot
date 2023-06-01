# pacalcbot

https://tech.xogrp.com/my-precious-little-container-emphasis-on-little-8a2b2077e35f
https://github.com/forficate/haskell_build_env/tree/master
https://github.com/lierdakil/alpine-haskell

make
docker images
docker run --rm -v $(pwd):/usr/src/build -w /usr/src/build -it 042ee929848b
stack install --system-ghc --no-install-ghc --allow-different-user --ghc-options '-optl-static -fPIC -optc-Os'
