.PHONY: all

OS=$(shell ghc -e ":m +System.Info" -e "putStrLn os")

NUPIC_ROOT=$(shell dirname `pwd`)/nupic.cpp
NUPIC_BUILD=$(NUPIC_ROOT)/build
NUPIC_DIR=$(NUPIC_BUILD)/Release
THIRDPATRY=$(NUPIC_BUILD)/ThirdParty

CC = g++ -std=c++11
CFLAGS = -O2 \
		 -I$(NUPIC_DIR)/include \
		 -I$(NUPIC_DIR)/include/src \
		 -I${THIRDPATRY}/boost/Boost_download-src \
		 -I${THIRDPATRY}/mnist_data/mnist-src/include

LDFLAGS = -O2

all: cpp/libnupic.so

cpp/libnupic.so: cpp/libnupic_core.a cpp/gen_nupic.o cpp/gen_std.o cpp/utils.o
ifeq ($(OS),darwin)
	$(CC) $(LDFLAGS) -dynamic -shared -fPIC -install_name @rpath/libnupic.so -o $@ $^
else
	$(CC) $(LDFLAGS) -dynamic -shared -fPIC -o $@ $^
endif

cpp/libnupic_core.a:
	mkdir -p $(NUPIC_BUILD)
	cd $(NUPIC_BUILD) && cmake $(NUPIC_ROOT) && make && make install
	cp $(NUPIC_DIR)/lib/libnupic_core.a cpp

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
