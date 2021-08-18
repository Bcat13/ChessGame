MODULES=piece square state board setup command engine main author
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS = $(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	OCAMLRUNPARAM=b utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

play:
	$(OCAMLBUILD) -tag 'debug' $(MAIN) && OCAMLRUNPARAM=b ./$(MAIN)

zip:
	zip chess.zip *.ml* _tags .merlin .ocamlformat .ocamlinit LICENSE Makefile INSTALL.txt

docs: docs-public 
	
docs-public: build
	mkdir -p _doc.public
	ocamlfind ocamldoc -I _build \
		-html -stars -d _doc.public $(MLIS)