.PHONY: all compile format test cover clean hank

all: compile format test cover

compile:
	rebar3 compile

format:
	rebar3 format

test:
	rebar3 eunit

cover:
	rebar3 cover

hank:
	rebar3 hank

build-test: compile xref format hank test cover

clean:
	rebar3 clean

xref:
	rebar3 xref