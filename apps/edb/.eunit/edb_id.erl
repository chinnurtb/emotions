-module(edb_id).

-export([
	new/0
]).

-spec new() -> binary().
new() ->
	<<I1:24/integer, I2:136/integer>> = crypto:sha(term_to_binary({make_ref(), now(), node()})),
	list_to_binary(integer_to_list(I1, 16) ++ [$2] ++ integer_to_list(I2, 16)).
