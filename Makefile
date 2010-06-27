all: compile

compile:
	erlc -o ebin +debug_info erl_make.erl       
	erl -pa ./ebin -eval "erl_make:make(development)" -s init stop -noshell

.PHONY: all compile 
