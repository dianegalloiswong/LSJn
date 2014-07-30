OCAMLBUILD=ocamlbuild -classic-display \
	-tags annot,debug,thread \
	-libs unix \
	-I analyseur_ILTP \
	-I sortie \
	-I exemples \
	-I prouveurs \
		-I prouveurs/indexation \
		-I prouveurs/prouveur_caml \
			-I prouveurs/prouveur_caml/sequent \
			-I prouveurs/prouveur_caml/sequent/structures \
			-I prouveurs/prouveur_caml/preuve_contre_modele \
		-I prouveurs/fonctions_compilees \
		-I prouveurs/prouveur_trees \
			-I prouveurs/prouveur_trees/analyseur_trees \
			-I prouveurs/prouveur_trees/exec_trees_via_machine \
		-I prouveurs/prouveur_compile_caml \
	-I test_recherche_compilee \
	-use-menhir
TARGET=native


main:
	$(OCAMLBUILD) main.$(TARGET)

bin:
	$(OCAMLBUILD) exec_bin.$(TARGET)

trees:
	$(OCAMLBUILD) main_trees.$(TARGET)

clean:
	$(OCAMLBUILD) -clean

realclean: clean
	rm -f *~ **/*~ **/**/*~ **/**/**/*~

cleanall: realclean
