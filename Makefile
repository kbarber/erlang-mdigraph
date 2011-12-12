all: compile

compile:
	./rebar compile
clean:
	./rebar clean
	rm -fr logs
	rm -f test/*.beam
test: all
	./rebar skip_deps=true eunit
	./rebar ct
docs:
	./rebar skip_deps=true doc

APPS = kernel stdlib sasl erts ssl tools os_mon runtime_tools crypto inets\
xmerl snmp public_key mnesia eunit syntax_tools compiler eunit webtool

COMBO_PLT = $(HOME)/.migraph_dialyzer_plt

check_plt: compile
	dialyzer --check_plt --plt $(COMBO_PLT) --apps $(APPS) \

build_plt: compile
	dialyzer --build_plt --output_plt $(COMBO_PLT) --apps $(APPS) \

dialyzer: all
	@echo
	@echo Use "'make check_plt'" to check PLT prior to using this target.
	@echo Use "'make build_plt'" to build PLT prior to using this target.
	@echo
	@sleep 1
	dialyzer -Wno_return --plt $(COMBO_PLT) ebin

cleanplt:
	@echo
	@echo "Are you sure? It takes about 1/2 hour to re-build."
	@echo Deleting $(COMBO_PLT) in 5 seconds.
	@echo
	sleep 5
	rm $(COMBO_PLT)
