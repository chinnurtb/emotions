%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the eweb application.

-module(eweb_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for eweb.
start(_Type, _StartArgs) ->
    eweb_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for eweb.
stop(_State) ->
    ok.
