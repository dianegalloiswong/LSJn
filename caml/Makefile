OCAMLBUILD=ocamlbuild -classic-display \
		-tags annot,debug,thread \
		-libs unix \
                -Is prouveur,prouveur/indexation,prouveur/sequent,prouveur/sequent/structures,prouveur/strategie,prouveur/reponse,analyseur,sortie,exemples \
                -use-menhir
TARGET=native


main:
	$(OCAMLBUILD) main.$(TARGET)


clean:
	$(OCAMLBUILD) -clean

realclean: clean
	rm -f *~ **/*~

cleanall: realclean
