CACHEGRIND=qcachegrind
ELVIS=./bin/elvis
REBAR3=./bin/rebar3

all: compile

bench:
	@echo "Benchmarking..."
	@$(REBAR3) as test compile
	@./bin/bench.sh

clean:
	@echo "Running rebar3 clean..."
	@$(REBAR3) clean -a

compile:
	@echo "Running rebar3 compile..."
	@$(REBAR3) as compile compile

coveralls:
	@echo "Running rebar3 coveralls send..."
	@$(REBAR3) as test coveralls send

dialyzer:
	@echo "Running rebar3 dialyze..."
	@$(REBAR3) dialyzer

edoc:
	@echo "Running rebar3 edoc..."
	@$(REBAR3) as edoc edoc

elvis:
	@echo "Running elvis rock..."
	@$(ELVIS) rock

eunit:
	@echo "Running rebar3 eunit..."
	@$(REBAR3) do eunit -cv, cover -v

profile:
	@echo "Profiling..."
	@$(REBAR3) as test compile
	@./bin/profile.sh
	@_build/test/lib/fprofx/erlgrindx -p fprofx.analysis
	@$(CACHEGRIND) fprofx.cgrind

test: elvis xref eunit dialyzer

travis: test coveralls

xref:
	@echo "Running rebar3 xref..."
	@$(REBAR3) xref

.PHONY: bench clean compile coveralls dialyzer edoc elvis eunit profile xref
