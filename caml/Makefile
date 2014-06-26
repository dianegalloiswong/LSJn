OCAMLBUILD=ocamlbuild -classic-display \
		-tags annot,debug,thread \
		-libs unix \
                -Is aux,foo
TARGET=native


main:
	$(OCAMLBUILD) main.$(TARGET)


clean:
	$(OCAMLBUILD) -clean

realclean: clean
	rm -f *~
	rm -f aux/*~

cleanall: realclean
