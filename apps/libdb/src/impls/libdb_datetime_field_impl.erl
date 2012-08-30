-module(libdb_datetime_field_impl, [Name, Validators, DefaultValue]).

-behavior(libdb_field).

-define(FORMAT, "%d.%m.%Y %H:%M:%S").

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

-spec to_html(Value :: calendar:datetime()) -> string().
to_html(Value = {{_Y,_Mth,_D},{_H,_Min,_S}}) ->
	edatetime:format(?FORMAT, Value).

-spec from_html(Value :: string()) -> calendar:datetime().
from_html(Value) when is_list(Value) ->
	try
		edatetime:parse(?FORMAT, Value)
	catch
		_:badarg -> Value
	end.

-spec to_json(Value :: calendar:datetime()) -> integer().
to_json(Value = {{_Y,_Mth,_D},{_H,_Min,_S}}) ->
	calendar:datetime_to_gregorian_seconds(Value).

-spec from_json(Value :: integer()) -> calendar:datetime().
from_json(Value) when is_integer(Value) ->
	calendar:gregorian_seconds_to_datetime(Value).

-spec index_type() -> libdb_field:index_type().
index_type() -> none.

-spec is_unique() -> boolean().
is_unique() -> false.

-spec is_indexed() -> boolean().
is_indexed() -> false.

-spec validate(Value :: calendar:datetime()) -> ok | {error, [string()]}.
validate(Value) ->
	libdb_validators:validate(Value, [libdb_validators:validate_type(datetime) | Validators]).

-spec name() -> atom().
name() -> Name.

-spec default_value() -> calendar:datetime().
default_value() -> DefaultValue.

