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

#
# Rebuild from scratch
#
main: $(MAIN).java clean
	javac -g $(MAIN).java

#
# Library
#
$(MAIN).jar: $(CFILES) Nuprl.man
	(cd ../../../../..; jar cmf $(PACKAGE)/Nuprl.man $(PACKAGE)/$(MAIN).jar $(PACKAGE)/*.class)

#
# Rule for compiling Java
#
%.class: %.java
	javac -g $*.java

#
# Clean everything
#
clean:
	rm -f *.jar *.class *~