sudo apt-get update
sudo apt-get install -y libghc-cairo-dev libghc-sdl-dev libghc-pango-dev
wget http://libsdl.org/release/SDL2-2.0.3.tar.gz
tar -zxvf SDL2-2.0.3.tar.gz
cd SDL2-2.0.3
./configure
sudo make
sudo make install
cabal update
cabal install alex
cabal install gtk2hs-buildtools
cabal install helm
