-module(libdb_date_field).

-export([
	new/1,
	new/2,
	new/3
]).

-spec new(Name :: atom(),
		  Validators :: [libdb_field:validate_fun()],
		  DefaultValue :: calendar:date()) -> module().
new(Name, Validators, DefaultValue) when is_atom(Name) ->
	libdb_date_field_impl:new(Name, Validators, DefaultValue).

-spec new(Name :: atom()) -> module().
new(Name) ->
	new(Name, [], undefined).

-spec new(Name :: atom(),
		  libdb_field:validate_fun() |
		  [libdb_field:validate_fun()] |
		  calendar:date()) -> module().
new(Name, Fun) when is_function(Fun, 1) ->
	new(Name, [Fun], undefined);
new(Name, Funs = [Fun | _T]) when is_function(Fun, 1) ->
	new(Name, Funs, undefined);
new(Name, DefValue) when is_list(DefValue) ->
	new(Name, [], DefValue).
