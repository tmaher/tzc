# Configure as appropriate.

KERB_LIBS = -lkrb -lkrb5 -lcom_err
ZEPHYR_LIBS = -lzephyr

DESTDIR = 

# You may need to use these to pick up some BSD'ish functions like flock().
# Linux needs MISC_LIBS = -lbsd
# NetBSD needs MISC_LIBS = -lcrypt
# AIX needs MISC_CFLAGS = -D_BSD and MISC_LIBS = -lbsd
# Solaris needs MISC_CFLAGS = -I/usr/include -DNO_SIGMASK
#           and MISC_LIBS = -lsocket -lnsl  /usr/ucblib/libucb.a
MISC_CFLAGS = 
MISC_LIBS = 

# uncomment this if getenv() isn't already available (e.g. SunOS 4.1.x)
# EXTRA_OBJS = getenv.o

# This is not included since in some cases it can do more harm than good.
EXTRA_OBJS = ZCkAuth.o 

OBJS = tzc.o lread.o $(EXTRA_OBJS)

CC = gcc
LD = $(CC)

DEFINES = -DINTERREALM
INCLUDES = $(ZEPHYR_INCLUDES) $(KERB_INCLUDES) $(MISC_CFLAGS)
CFLAGS = -g -O -Wall $(DEFINES) $(INCLUDES)

LIBS = $(ZEPHYR_LIBS) $(KERB_LIBS) $(MISC_LIBS)

tzc: $(OBJS) 
	$(LD) $(LDFLAGS) -o tzc.new $(OBJS) $(LIBS)
	/bin/mv tzc.new tzc

install: tzc
	install tzc $(DESTDIR)/usr/bin
	install tzc.1 $(DESTDIR)/usr/share/man/man1

lread.o: lread.h
tzc.o: lread.h

clean:
	/bin/rm -f *.o tzc tzc.new core
