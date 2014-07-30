OCAMLBUILD=ocamlbuild -classic-display \
	-tags annot,debug,thread \
	-libs unix \
	-I analyseur_ILTP \
	-I sortie \
	-I exemples \
	-I prouveurs/commun \
		-I prouveurs/commun/indexation \
	-I prouveurs/prouveur_caml \
		-I prouveurs/prouveur_caml/sequent \
		-I prouveurs/prouveur_caml/sequent/structures \
		-I prouveurs/prouveur_caml/strategie \
		-I prouveurs/prouveur_caml/preuve_contre_modele \
	-I test_recherche_compilee \
	-I prouveurs/prouveur_trees \
		-I prouveurs/prouveur_trees/analyseur_trees \
		-I prouveurs/prouveur_trees/compilation_vers_trees \
		-I prouveurs/prouveur_trees/compilation_trees_via_caml \
		-I prouveurs/prouveur_trees/interpreteur_trees \
		-I prouveurs/prouveur_trees/trees_vers_coq \
		-I prouveurs/prouveur_trees/caml_caml \
		-I prouveurs/prouveur_trees/fonctions_compilees \
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
