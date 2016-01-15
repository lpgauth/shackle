CACHEGRIND=qcachegrind
ELVIS=./bin/elvis
REBAR=./bin/rebar3

all: compile

clean:
	@echo "Running rebar3 clean..."
	@$(REBAR) clean -a

compile:
	@echo "Running rebar3 compile..."
	@$(REBAR) as compile compile

dialyzer:
	@echo "Running rebar3 dialyze..."
	@$(REBAR) dialyzer

edoc:
	@echo "Running rebar3 edoc..."
	@$(REBAR) as edoc edoc

elvis:
	@echo "Running elvis rock..."
	@$(ELVIS) rock

eunit:
	@echo "Running rebar3 eunit..."
	@$(REBAR) do eunit -cv, cover -v

profile:
	@echo "Profiling..."
	@$(REBAR) as test compile
	@erl -noshell \
	     -pa _build/test/lib/*/ebin \
		 -eval 'shackle_profile:fprofx()' \
		 -eval 'init:stop()'
	@_build/test/lib/fprofx/erlgrindx -p fprofx.analysis
	@$(CACHEGRIND) fprofx.cgrind

shell:
	@echo "Running rebar3 shell..."
	@$(REBAR) as test shell

test: dialyzer elvis eunit xref

xref:
	@echo "Running rebar3 xref..."
	@$(REBAR) xref

.PHONY: edoc test xref
