#
# Files in this application
#
FILES :=\
	Context\
	MarshalInfo\
	Marshalable\
	ImageLabel\
	Icon\
	IconWindow\
	Editor\
	Nuprl

JFILES := $(addsuffix .java, $(FILES))
CFILES := $(addsuffix .class, $(FILES))

MAIN := Nuprl

all: $(MAIN).jar
opt: $(MAIN).jar

#
# Library
#
$(MAIN).jar: $(CFILES)
	jar cf $(MAIN).jar $(CFILES)

#
# Rule for compiling Java
#
%.class: %.java
	javac -g $*.java
