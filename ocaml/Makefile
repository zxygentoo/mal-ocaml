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

all :
	dune build @all


$(STEP_BINS) :
	dune build $@.exe

clean :
	dune clean

.PHONY: all clean
