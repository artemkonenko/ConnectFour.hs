ConnectFour.hs
=========
The "Connect Four" game implemented in Haskell using [Helm](https://github.com/switchface/helm) library.

[![Build Status](https://travis-ci.org/dummer/ConnectFour.hs.svg?branch=master)](https://travis-ci.org/dummer/ConnectFour.hs)

Build and run
========
Run ‘build_depends.sh‘ to download and build all the necessary components. Then run ‘cabal run‘ to launch the game.

Gameplay
=======
0. Choose a game type (vs. another user or vs. AI) by pressing 1 or 2.
1. Your goal is to collect 4 consecutive balls of your color in a row, column or diagonal. You can add a ball of corresponding color to the top of column by pressing buttons 1-7 (not on numpad).