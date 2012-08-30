-module(libdb_int_field_impl, [Name, Indexed, Validators, DefaultValue]).

-behavior(libdb_field).

-export([
	to_html/1,
	from_html/1,
	from_json/1,
	to_json/1,
	index_type/0,
	is_unique/0,
	is_indexed/0,
	validate/1,
	name/0,
	default_value/0
]).

-spec to_html(Value :: integer()) -> string().
to_html(Value) when is_integer(Value) ->
	estring:to_string(Value).

-spec from_html(Value :: string()) -> integer().
from_html(Value) when is_list(Value) ->
	try
		list_to_integer(Value)
	catch
		_:badarg -> Value
	end.

-spec to_json(Value :: integer()) -> integer().
to_json(Value) when is_integer(Value) ->
	Value.

-spec from_json(Value :: integer()) -> integer().
from_json(Value) when is_integer(Value) ->
	Value.

-spec index_type() -> libdb_field:index_type().
index_type() -> int.

-spec is_unique() -> boolean().
is_unique() -> is_unique(Indexed).

-spec is_indexed() -> boolean().
is_indexed() -> is_indexed(Indexed).

-spec validate(Value :: integer()) -> ok | {error, [string()]}.
validate(Value) ->
	libdb_validators:validate(Value, [libdb_validators:is_type(integer) | Validators]).

-spec name() -> atom().
name() -> Name.

-spec default_value() -> integer().
default_value() -> DefaultValue.

is_indexed(false) -> false;
is_indexed(_) -> true.

is_unique(unique) -> true;
is_unique(_) -> false.
