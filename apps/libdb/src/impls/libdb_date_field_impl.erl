-module(libdb_date_field_impl, [Name, Validators, DefaultValue]).

-behavior(libdb_field).

-define(FORMAT, "%d.%m.%Y").

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

-spec to_html(Value :: calendar:date()) -> string().
to_html(Value = {_Y,_M,_D}) ->
	edatetime:format(?FORMAT, Value).

-spec from_html(Value :: string()) -> calendar:date().
from_html(Value) when is_list(Value) ->
	try
		edatetime:parse(?FORMAT, Value)
	catch
		_:badarg -> Value
	end.

-spec to_json(Value :: calendar:date()) -> integer().
to_json(Value = {_Y,_M,_S}) ->
	calendar:date_to_gregorian_days(Value).

-spec from_json(Value :: integer()) -> calendar:date().
from_json(Value) when is_integer(Value) ->
	calendar:gregorian_days_to_date(Value).

-spec index_type() -> libdb_field:index_type().
index_type() -> none.

-spec is_unique() -> boolean().
is_unique() -> false.

-spec is_indexed() -> boolean().
is_indexed() -> false.

-spec validate(Value :: calendar:date()) -> ok | {error, [string()]}.
validate(Value) ->
	libdb_validators:validate(Value, [libdb_validators:is_type(date) | Validators]).

-spec name() -> atom().
name() -> Name.

-spec default_value() -> calendar:date().
default_value() -> DefaultValue.

