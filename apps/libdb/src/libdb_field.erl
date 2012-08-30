-module(libdb_field).

-type value() :: term().
-type index_type() :: int | bin.
-type validate_fun() :: fun((value()) -> ok | {error, string()}).

-export_type([
		value/0,
		index_type/0,
		validate_fun/0
	]).

-callback to_html(Value :: value()) -> string().
-callback from_html(Value :: string()) -> value().
-callback to_json(Value :: value()) -> value().
-callback from_json(Value :: value()) -> value().
-callback index_type() -> index_type().
-callback is_indexed() -> boolean().
-callback is_unique() -> boolean().
-callback validate(Value :: value()) -> ok | {error, [string()]}.
-callback name() -> atom().
-callback default_value() -> value().

