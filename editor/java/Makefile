#
# Files in this application
#

FILES :=\
	Target\
	NuprlDebug\
	NuprlException\
	NuprlAuthorization\
	NuprlToken\
	NuprlSyncToken\
	NuprlDataToken\
	NuprlControlToken\
	NuprlEscapedToken\
	NuprlArgumentToken\
	NuprlOptionToken\
	NuprlOptionResponseToken\
	NuprlOptionRequestToken\
	NuprlOptionBlockToken\
	NuprlOptionSBToken\
	NuprlCommandToken\
	NuprlURLToken\
	NuprlBusMessage\
	NuprlBusPort\
	NuprlBusEndpoint\
	NuprlBusClient\
	NuprlBus\
	NuprlFrame\
	NuprlClient\
	NuprlInternalFrame\
	NuprlText\
	NuprlCommand\
	NuprlTerm\
	NuprlProof\
	NuprlMenu\
	NuprlHost\
	NuprlContext\
	NuprlControl\
	NuprlManager\
	Nuprl

JFILES := $(addsuffix .java, $(FILES))
CFILES := $(addsuffix .class, $(FILES))

MAIN := Nuprl
PACKAGE := edu/cornell/cs/jyh/nuprl

.PHONY: all opt clean main

all: $(MAIN).jar
opt: $(MAIN).jar

ifndef OSTYPE
   OSTYPE=
endif
ifeq ($(OSTYPE),linux)
   OSTYPE=Linux
endif
ifeq ($(OSTYPE),solaris)
   OSTYPE=Solaris
endif
ifeq ($(OSTYPE),win32)
   TYPE=win
endif
ifeq ($(OSTYPE),cygwin32)
   TYPE=win
endif

ifeq ($(OSTYPE),Linux)
   TYPE=Unix
endif
ifeq ($(Solaris),Linux)
   TYPE=Unix
endif

JAVAC = javac -g

#
# Rebuild from scratch
#
main: $(MAIN).java clean
	$(JAVAC) $(MAIN).java

#
# Library
#
ifeq ($(TYPE),Unix)

$(PACKAGE)/Nuprl.java:
	mkdir -p `dirname $(PACKAGE)`
	ln -s `pwd` $(PACKAGE)

$(MAIN).jar: $(CFILES) Nuprl.man $(PACKAGE)/Nuprl.java
	jar cmf $(PACKAGE)/Nuprl.man $(PACKAGE)/$(MAIN).jar $(PACKAGE)/*.class

#
# Rule for compiling Java
#
%.class: %.java $(PACKAGE)/Nuprl.java
	$(JAVAC) $*.java

else

$(MAIN).jar: $(CFILES) Nuprl.man
	(cd ../../../../..; jar cmf $(PACKAGE)/Nuprl.man $(PACKAGE)/$(MAIN).jar $(PACKAGE)/*.class)

#
# Rule for compiling Java
#
%.class: %.java
	$(JAVAC) $*.java

endif

#
# Clean everything
#
clean:
	rm -f *.jar *.class *~
