.PHONY: all

OS=$(shell ghc -e ":m +System.Info" -e "putStrLn os")

CC = g++ -std=c++11
CFLAGS = -O2

all: cpp/libnupic.so

cpp/libnupic.so: cpp/gen_nupic.o cpp/gen_std.o cpp/utils.o
ifeq ($(OS),darwin)
	$(CC) $(CFLAGS) -dynamic -shared -fPIC -install_name @rpath/libnupic.so -lnupic_core -Lcpp -o $@ $^
else
	$(CC) $(CFLAGS) -dynamic -shared -fPIC -lnupic_core -Lcpp -o $@ $^
endif

%.o: %.cpp
	$(CC) $(CFLAGS) -fPIC -Icpp -c -o $@ $<

clean:
	rm -f cpp/*.o cpp/libnupic.so

install:
ifeq ($(OS),darwin)
	install cpp/libnupic.so "$(libdir)"
else
	install -t "$(libdir)" cpp/libnupic.so
endif
