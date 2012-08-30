-module(libdb_validators).

-export([
	validate/2,
	validate_type/1
]).

validate(_Value, _Validators) ->
	throw(not_implemented).

validate_type(_Type) ->
	throw(not_implemented).
