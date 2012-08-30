-module(libdb_int_field).

-export([
	new/1,
	new/2,
	new/3,
	new/4
]).

-type index_type() :: boolean() | unique.

-spec new(Name :: atom(),
		  Indexed :: index_type(),
		  Validators :: [libdb_field:validate_fun()],
		  DefaultValue :: integer()) -> module().
new(Name, Indexed, Validators, DefaultValue)
		when is_atom(Name) andalso (is_boolean(Indexed) orelse Indexed =:= unique) ->
	libdb_int_field_impl:new(Name, Indexed, Validators, DefaultValue).

-spec new(Name :: atom()) -> module().
new(Name) ->
	new(Name, false, [], undefined).

-spec new(Name :: atom(),
		  index_type() |
		  libdb_field:validate_fun() |
		  [libdb_field:validate_fun()] |
		  integer()) -> module().
new(Name, Indexed) when is_atom(Indexed) ->
	new(Name, unique, [], undefined);
new(Name, Fun) when is_function(Fun, 1) ->
	new(Name, false, [Fun], undefined);
new(Name, Funs = [Fun | _T]) when is_function(Fun, 1) ->
	new(Name, false, Funs, undefined);
new(Name, DefValue) when is_list(DefValue) ->
	new(Name, false, [], DefValue).

new(Name, Indexed, Fun) when is_atom(Indexed), is_function(Fun, 1) ->
	new(Name, Indexed, [Fun], undefined);
new(Name, Indexed, Funs = [Fun | _T]) when is_atom(Indexed), is_function(Fun, 1) ->
	new(Name, Indexed, Funs, undefined);
new(Name, Indexed, DefValue) when is_atom(Indexed), is_list(DefValue) ->
	new(Name, Indexed, [], DefValue);
new(Name, Fun, DefValue) when is_function(Fun, 1), is_list(DefValue) ->
	new(Name, false, [Fun], DefValue);
new(Name, Funs = [Fun | _T], DefValue) when is_function(Fun, 1), is_list(DefValue) ->
	new(Name, false, Funs, DefValue).

