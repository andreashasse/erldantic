.PHONY: all compile format test cover clean type_check

all: compile format test cover

compile:
	rebar3 compile

format:
	rebar3 format

test:
	rebar3 eunit

cover:
	rebar3 cover

build-test: compile type_check format test cover

type_check:
	@output=$$(elp eqwalize-all); \
	echo "$$output"; \
	if ! echo "$$output" | grep -q "NO ERRORS"; then \
		echo "ELP found errors. Failing build."; \
		exit 1; \
	fi

clean:
	rebar3 clean