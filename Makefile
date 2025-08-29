.PHONY: all compile format test cover clean

all: compile format test cover

compile:
	rebar3 compile

format:
	rebar3 format

hank:
	rebar3 hank

format_verify:
	rebar3 format --verify

test:
	rebar3 eunit

proper:
	rebar3 proper

cover:
	rebar3 cover

build-test: compile xref type_check test hank format_verify cover

clean:
	rebar3 clean

xref:
	rebar3 xref

type_check:
	@output=$$(elp eqwalize-all); \
	echo "$$output"; \
	if ! echo "$$output" | grep -q "NO ERRORS"; then \
		exit 1; \
	else \
		exit 0; \
	fi
