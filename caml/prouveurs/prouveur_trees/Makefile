OCAMLBUILD=ocamlbuild -classic-display \
		-tags annot,debug,thread \
		-libs unix \
		-Is copieprouveur,copieprouveur/indexation \
		-I interpreteur_trees \
		-use-menhir
TARGET=native


main:
	$(OCAMLBUILD) main.$(TARGET)
	


clean:
	$(OCAMLBUILD) -clean

cleantild:
	rm -f *~ **/*~ **/**/*~

realclean: clean cleantild

cleanall: realclean
