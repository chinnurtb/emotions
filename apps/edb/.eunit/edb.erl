-module(edb).

-export([
	start/0,
	start_link/0,
	stop/0
]).


ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

start_common() ->
	ensure_started(crypto),
	ensure_started(pooler).


start() ->
	start_common(),
	application:start(edb).

start_link() ->
	start_common(),
	edb_sup:start_link().

stop() ->
	Result = application:stop(edb),
	application:stop(pooler),
	application:stop(crypto),
	Result.
