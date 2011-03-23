all: compile

compile:
	./rebar compile
clean:
	./rebar clean
	rm -fr logs
	rm test/*.beam
test: all
	./rebar skip_deps=true eunit
	./rebar ct
docs:
	./rebar skip_deps=true doc
