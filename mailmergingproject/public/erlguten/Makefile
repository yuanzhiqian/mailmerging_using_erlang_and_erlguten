include vsn.mk
APPNAME=erlguten
SUBDIRS=src priv/src
DOC_OPTS={def,{version,\"$(VSN)\"}}
ERL=erl

.PHONY: all conf test $(SUBDIRS)

all: conf $(SUBDIRS)

conf test:
	cd $@ && $(MAKE)

$(SUBDIRS):
	cd $@ && $(MAKE) VSN=$(VSN)

docs: doc/index.html

doc/index.html: doc/overview.edoc $(wildcard src/*.erl)
	$(ERL) -noshell -eval "edoc:application($(APPNAME), \".\", [$(DOC_OPTS)])" -s init stop

clean:
	cd src && $(MAKE) clean
	cd test && $(MAKE) clean
