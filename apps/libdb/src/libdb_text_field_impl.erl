-module(libdb_text_field_impl, [Name, Indexed, Validators]).

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
	name/0
]).

-spec to_html(Value :: string()) -> string().
to_html(Value) when is_list(Value) ->
	estring:sanitize_for_html(Value).

-spec from_html(Value :: string()) -> string().
from_html(Value) when is_list(Value) ->
	Value.

-spec to_json(Value :: string()) -> binary().
to_json(Value) when is_list(Value) ->
	list_to_binary(Value).

-spec from_json(Value :: binary()) -> string().
from_json(Value) when is_binary(Value) ->
	binary_to_list(Value).

-spec index_type() -> libdb_field:index_type().
index_type() -> bin.

-spec is_unique() -> boolean().
is_unique() -> is_unique(Indexed).

-spec is_indexed() -> boolean().
is_indexed() -> is_indexed(Indexed).

-spec validate(Value :: string()) -> ok | {error, [string()]}.
validate(Value) ->
	libdb_validators:validate(Value, [libdb_validators:is_type(string) | Validators]).

-spec name() -> atom().
name() -> Name.


is_indexed(false) -> false;
is_indexed(_) -> true.

is_unique(unique) -> true;
is_unique(_) -> false.
