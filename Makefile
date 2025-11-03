.PHONY: all compile format test cover clean doc perf

all: compile format test cover doc

compile:
	rebar3 compile

format:
	rebar3 fmt

format_verify:
	rebar3 fmt --check

hank:
	rebar3 hank

test:
	@if command -v elixirc >/dev/null 2>&1; then \
		echo "Compiling Elixir files..."; \
		cd test && elixirc *.ex; \
	else \
		echo "Elixir not found, skipping Elixir compilation"; \
	fi
	rebar3 eunit

proper:
	rebar3 proper

cover:
	rebar3 cover

check_app_calls:
	rebar3 check_app_calls

build-test: compile xref type_check test dialyzer hank check_app_calls format_verify cover

clean:
	rebar3 clean

xref:
	rebar3 xref

dialyzer:
	rebar3 dialyzer

type_check:
	@output=$$(elp eqwalize-all); \
	echo "$$output"; \
	if ! echo "$$output" | grep -q "NO ERRORS"; then \
		exit 1; \
	else \
		exit 0; \
	fi

doc:
	rebar3 ex_doc

release:
	@echo "Last 5 tags:"
	@git tag --sort=-version:refname | head -n 5
	@echo ""
	@read -r -p "Enter the next tag (e.g., v1.0.0): " tag && [ -n "$$tag" ] || { echo "Tag cannot be empty. Aborted."; exit 1; }; \
	read -r -p "Did you update the README install instructions? (Y/N) " a && [ "$$a" = "Y" ] || { echo "Aborted."; exit 1; }; \
	git tag "$$tag" && \
	git push origin "$$tag" && \
	rebar3 compile && \
	rebar3 hex build && \
	rebar3 hex publish && \
	echo "Released and tagged as $$tag"\

perf: compile
	@echo "Running performance benchmark..."
	@erlc -o test +debug_info test/perf_benchmark.erl
	@erl -pa _build/default/lib/*/ebin test -noshell -eval "perf_benchmark:run(), halt()."
	@rm -f test/perf_benchmark.beam
