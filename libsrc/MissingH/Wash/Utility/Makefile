#
# Makefile for WASH/Utility (Haskell98)
# copyright 2001-2003 by Peter Thiemann
#

PACKAGE=Utility
VERSION=0.3.11

include $(TOP)/mk/config.mk
include $(TOP)/mk/common.mk

FILES= Makefile LICENSE Utility.pkg $(HS_SOURCES) $(C_SOURCES) $(H_SOURCES)
HS_SOURCES=\
	Auxiliary.hs \
	Base32.hs \
	Base64.hs \
	FileNames.hs \
	Hex.hs \
	ISO8601.hs \
	IntToString.hs \
	JavaScript.hs \
	Locking.hs \
	QuotedPrintable.hs \
	RFC2047.hs \
	RFC2279.hs \
	RFC2397.hs \
	SHA1.hs \
	SimpleParser.hs \
	Shell.hs \
	URLCoding.hs \
	Unique.hs \

C_SOURCES= sha1lib.c
H_SOURCES= sha1lib.h

FULLNAME=$(PACKAGE)-$(VERSION)
BINDISTNAME=$(PACKAGE)-bin-$(VERSION).tgz
DISTNAME=$(FULLNAME).tgz
# where the WASH homepage resides
WWWDIR=$(HOME)/public/www/haskell/WASH
TMPDIR=/tmp

######################################################################
# goals

all: libUtility.a

libUtility.a: libUtility.a($(HS_SOURCES:.hs=.o) $(C_SOURCES:.c=.o))
	$(RANLIB) $@

dist: documentation
	$(RM) -rf $(TMPDIR)/$(FULLNAME)
	$(MKDIR) -p $(TMPDIR)/$(FULLNAME)/$(DOCSUBDIR)
	$(CP) $(FILES) $(TMPDIR)/$(FULLNAME)
	$(CP) -rp $(DOCSUBDIR) $(TMPDIR)/$(FULLNAME)
	$(TAR) cfCvz $(DISTNAME) $(TMPDIR) $(FULLNAME)

install-distribution: dist
	$(INSTALL) $(DISTNAME) $(WWWDIR)

install: libUtility.a
	$(INSTALL) -d $(PACKAGELIBDIR)
	$(INSTALL) -d $(PACKAGEIMPORTDIR)
	$(INSTALL) -c -m 644 $(HS_SOURCES:.hs=.hi) $(PACKAGEIMPORTDIR)
	$(INSTALL) -c -m 644 libUtility.a $(PACKAGELIBDIR)
	$(RANLIB) $(PACKAGELIBDIR)/libUtility.a
ifeq ($(ENABLE_REG_PACKAGE),yes)
	$(GENPKG) $(PACKAGE) --import_dirs $(PACKAGEIMPORTDIR) --library_dirs $(PACKAGELIBDIR) --hs_libraries $(PACKAGE) --package_deps text | $(GHCPKG) $(GHCPKGFLAGS) --update-package --auto-ghci-libs
endif

######################################################################
# special dependencies

sha1lib.o: sha1lib.h
SHA1.o: sha1lib.h

######################################################################
# generic stuff

HC_SPEC_FLAGS=  -package-name $(PACKAGE)
HCINCLUDES=	-package text
HCLOADFLAGS=

OPT=		-O2 -ffi

HCFLAGS=	$(OPT) $(HC_SPEC_FLAGS) $(HCINCLUDES) $(HCEXTRAFLAGS)

HS_FILES=	$(HS_SOURCES)

DOCSUBDIR= doc

documentation: $(DOCSUBDIR)

$(DOCSUBDIR): $(HS_SOURCES)
	$(RM) -rf $(DOCSUBDIR)
	$(MKDIR) $(DOCSUBDIR)
	$(HADDOCK) -o $(DOCSUBDIR) -h $(HS_SOURCES)

clean:
	$(RM) -f *.a *.o *.hi

veryclean:: clean

depend::
	$(HC) -M $(HCFLAGS) $(HS_FILES)

# DO NOT DELETE: Beginning of Haskell dependencies
Auxiliary.o : Auxiliary.hs
Auxiliary.o : ./Shell.hi
Auxiliary.o : ./FileNames.hi
Base32.o : Base32.hs
Base64.o : Base64.hs
FileNames.o : FileNames.hs
Hex.o : Hex.hs
ISO8601.o : ISO8601.hs
ISO8601.o : ./SimpleParser.hi
ISO8601.o : ./IntToString.hi
IntToString.o : IntToString.hs
JavaScript.o : JavaScript.hs
JavaScript.o : Hex.hi
Locking.o : Locking.hs
Locking.o : Auxiliary.hi
QuotedPrintable.o : QuotedPrintable.hs
QuotedPrintable.o : Hex.hi
RFC2047.o : RFC2047.hs
RFC2047.o : Hex.hi
RFC2047.o : QuotedPrintable.hi
RFC2047.o : Base64.hi
RFC2279.o : RFC2279.hs
RFC2397.o : RFC2397.hs
RFC2397.o : Base64.hi
RFC2397.o : ./URLCoding.hi
SHA1.o : SHA1.hs
SimpleParser.o : SimpleParser.hs
Shell.o : Shell.hs
URLCoding.o : URLCoding.hs
URLCoding.o : Hex.hi
Unique.o : Unique.hs
Unique.o : Locking.hi
Unique.o : Auxiliary.hi
# DO NOT DELETE: End of Haskell dependencies
