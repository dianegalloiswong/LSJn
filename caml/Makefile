OCAMLBUILD=ocamlbuild -classic-display \
	-tags annot,debug,thread \
	-libs unix \
	-I utilitaires \
		-I utilitaires/analyseur_ILTP \
		-I utilitaires/exemples_formules \
	-I prouveurs \
		-I prouveurs/indexation \
		-I prouveurs/prouveur_simple \
			-I prouveurs/prouveur_simple/sequent \
			-I prouveurs/prouveur_simple/sequent/structures \
			-I prouveurs/prouveur_simple/preuve_contre_modele \
		-I prouveurs/prouveurs_autres \
			-I prouveurs/prouveurs_autres/fonctions_compilees \
			-I prouveurs/prouveurs_autres/prouveurs_T_et_TM \
				-I prouveurs/prouveurs_autres/prouveurs_T_et_TM/analyseur_trees \
				-I prouveurs/prouveurs_autres/prouveurs_T_et_TM/exec_trees_via_machine \
		-I prouveurs/prouveurs_autres/prouveur_compile_caml \
	-use-menhir
TARGET=native


main:
	$(OCAMLBUILD) main.$(TARGET)

clean:
	$(OCAMLBUILD) -clean

realclean: clean
	rm -f *~ **/*~ **/**/*~ **/**/**/*~

cleanall: realclean
