OCAMLBUILD=ocamlbuild -classic-display \
		-tags annot,debug,thread \
		-libs unix \
                -Is prouveurs/commun,prouveurs/commun/indexation,prouveurs/caml,prouveurs/caml/sequent,prouveurs/caml/sequent/structures,prouveurs/caml/strategie,prouveurs/caml/preuve_contre_modele,analyseur_ILTP,sortie,exemples,test_recherche_compilee \
                -use-menhir
TARGET=native


main:
	$(OCAMLBUILD) main.$(TARGET)


clean:
	$(OCAMLBUILD) -clean

realclean: clean
	rm -f *~ **/*~ **/**/*~ **/**/**/*~

cleanall: realclean
