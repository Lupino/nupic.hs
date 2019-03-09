.PHONY: all

OS=$(shell ghc -e ":m +System.Info" -e "putStrLn os")

NUPIC_ROOT=/Users/lmj/nupic/nupic.cpp/build
NUPIC_DIR=$(NUPIC_ROOT)/Release
THIRDPATRY=$(NUPIC_ROOT)/ThirdParty

CC = g++ -std=c++11
CFLAGS = -O2 \
		 -I$(NUPIC_DIR)/include \
		 -I$(NUPIC_DIR)/include/src \
		 -I${THIRDPATRY}/boost/Boost_download-src \
		 -I${THIRDPATRY}/mnist_data/mnist-src/include

LDFLAGS = -O2 -L$(NUPIC_DIR)/lib -lnupic_core

all: cpp/libnupic.so

cpp/libnupic.so: cpp/gen_nupic.o cpp/gen_std.o cpp/utils.o
ifeq ($(OS),darwin)
	$(CC) $(LDFLAGS) -dynamic -shared -fPIC -install_name @rpath/libnupic.so -o $@ $^
else
	$(CC) $(LDFLAGS) -dynamic -shared -fPIC -o $@ $^
endif

%.o: %.cpp
	$(CC) $(CFLAGS) -fPIC -c -o $@ $<

clean:
	rm -f cpp/*.o cpp/libnupic.so

install:
ifeq ($(OS),darwin)
	install cpp/libnupic.so "$(libdir)"
else
	install -t "$(libdir)" cpp/libnupic.so
endif
