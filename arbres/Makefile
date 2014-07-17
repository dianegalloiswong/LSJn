OCAMLBUILD=ocamlbuild -classic-display \
		-tags annot,debug,thread \
		-libs unix \
		-Is copieprouveur,copieprouveur/indexation,analyseur,interpreteur \
		-use-menhir
TARGET=native


main:
	$(OCAMLBUILD) main.$(TARGET)
	./main.native -f > texte/fonctions_sf.ml
	cat texte/utilities.ml texte/sequent.ml texte/fonctions_sf.ml texte/plus_forte_priorite.ml texte/all_imp.ml texte/prouvable.ml texte/expr.ml > texte.ml


clean:
	$(OCAMLBUILD) -clean

cleantild:
	rm -f *~ **/*~ **/**/*~

realclean: clean
	cleantild

cleanall: realclean
