all: compile

compile:
	./rebar compile
clean:
	./rebar clean

test: all
	./rebar skip_deps=true eunit
	./rebar ct
docs:
	./rebar skip_deps=true doc
