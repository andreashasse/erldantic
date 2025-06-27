.PHONY: all compile format test cover clean

all: compile format test cover

compile:
	rebar3 compile

format:
	rebar3 format

test:
	rebar3 eunit

cover:
	rebar3 cover

build-test: compile format test cover

clean:
	rebar3 clean