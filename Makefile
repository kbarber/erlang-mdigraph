all: compile

compile:
	./rebar compile
clean:
	./rebar clean

test: all
	./rebar skip_deps=true eunit
