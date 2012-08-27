-module(edb_id).

-export([
	new/0
]).

-spec new() -> binary().
new() ->
	eid:new_id().
