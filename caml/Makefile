OCAMLBUILD=ocamlbuild -classic-display \
		-tags annot,debug,thread \
		-libs unix \
                -Is aux,aux/structures_dans_seq
TARGET=native


main:
	$(OCAMLBUILD) main.$(TARGET)


clean:
	$(OCAMLBUILD) -clean

realclean: clean
	rm -f *~
	rm -f aux/*~
	rm -f aux/structures_dans_seq/*~

cleanall: realclean
