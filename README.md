# Nupic.hs

Haskell bindings for nupic.cpp

## Build

```bash
git clone https://github.com/htm-community/nupic.cpp.git
mkdir build
cd build
cmake ../nupic.cpp
make
make install
cd ..

git clone https://github.com/Lupino/nupic.hs.git
cp -av build/Release/lib/libnupic_core.a nupic.hs/nupic-cpp/cpp
cp -av build/Release/include/src/nupic nupic.hs/nupic-cpp/cpp
cp -av build/Release/include/nupic/* nupic.hs/nupic-cpp/cpp/nupic
cp -av build/ThirdParty/boost/Boost_download-src/boost nupic.hs/nupic-cpp/cpp
cp -av build/ThirdParty/mnist_data/mnist-src/boost nupic.hs

cd nupic.hs

stack build

stack exec nupic
```
