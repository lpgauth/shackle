CACHEGRIND=qcachegrind
REBAR3=$(shell which rebar3)
ifeq ($(REBAR3),)
REBAR3=./bin/rebar3
endif

all: compile

bench:
	@echo "Benchmarking..."
	@$(REBAR3) as bench compile
	@./bin/bench.sh

clean:
	@echo "Running rebar3 clean..."
	@$(REBAR3) clean -a

compile:
	@echo "Running rebar3 compile..."
	@$(REBAR3) as compile compile

edoc:
	@echo "Running rebar3 edoc..."
	@$(REBAR3) as edoc edoc

lint elvis xref dialyzer:
	@echo "Running $@..."
	@$(REBAR3) as lint $@

eunit:
	@echo "Running rebar3 eunit..."
	@$(REBAR3) do eunit -cv, cover -v

profile:
	@echo "Profiling..."
	@$(REBAR3) as profile compile
	@./bin/profile.sh
	@_build/profile/lib/fprofx/erlgrindx -p fprofx.analysis
	@$(CACHEGRIND) fprofx.cgrind

test: lint xref eunit dialyzer

.PHONY: bench clean compile dialyzer edoc lint elvis eunit profile xref
