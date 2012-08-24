ERL ?= erl
APP := emotions
ERL_ARGS := -pa $(PWD)/apps/*/ebin
ERL_ARGS += -pa $(PWD)/deps/*/ebin
ERL_ARGS += -sname dev
ERL_ARGS += -boot start_sasl
ERL_ARGS += -s reloader
ERL_ARGS += -s eweb
ERL_ARGS += -config $(PWD)/apps/edb/priv/pooler.config
ERL_ARGS += -s edb

.PHONY: deps

all: deps
	@./rebar compile

app:
	@./rebar compile skip_deps=true

deps:
	@./rebar get-deps

clean:
	@./rebar clean

distclean: clean
	@./rebar delete-deps

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'

webstart: app
	exec erl $(ERL_ARGS)

proxystart:
	@haproxy -f dev.haproxy.conf
