# Makefile for `f4view', written at Thu, 31 Oct 2002 15:01:17 +0100
# by "Boris Jakubith" <bj@isv-gmbh.de>
#
# Copyright (c) 2002, Boris Jakubith <bj@isv-gmbh.de>

PREFIX = /usr/local

CC = gcc
CFLAGS = -Wall -O2 -g
#LFLAGS = -g
LFLAGS = -Wl,-s
OBJS = f4view.o
LIBS =
TARGET = f4view

%.o : %.c
	$(CC) $(CFLAGS) -c $< -o $@

all: $(TARGET)

$(TARGET): $(OBJS)
	$(CC) $(LFLAGS) -o $@ $(OBJS) $(LIBS)

install: $(TARGET)
	install -s -m 0755 $(TARGET) $(DESTDIR)$(PREFIX)/bin

clean:
	rm -f *~ *.o $(TARGET)
