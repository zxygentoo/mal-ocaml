STEPS = step0_repl.ml \
		step1_read_print.ml \
		step2_eval.ml \
		step3_env.ml \
		step4_if_fn_do.ml \
		step5_tco.ml \
		step6_file.ml \
		step7_quote.ml \
		step8_macros.ml \
		step9_try.ml \
		stepA_mal.ml

STEP_BINS = $(STEPS:%.ml=%)
LAST_STEP_BIN = $(word $(words $(STEP_BINS)),$(STEP_BINS))

all :
	dune build @all

dist: mal

mal: $(LAST_STEP_BIN)
	cp $< $@

# ocaml repl apparently needs bytecode, not native, compilation.
# Just do it all right here:
# repl:
# 	ocamlc -c $(LIBS:%.cmxa=%.cma) $(MODULES) $(STEPS)
# 	rlwrap ocaml $(LIBS:%.cmxa=%.cma) $(MODULES:%.ml=%.cmo)

$(STEP_BINS) :
	dune build $@.exe

clean :
	dune clean @all

.PHONY: all clean
