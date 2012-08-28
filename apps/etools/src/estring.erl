-module(estring).
-export([
	to_string/1,
	format/1,
	format/2,
	format/3,
	format/4,
	format/5,
	format/6
]).

to_string(A) when is_integer(A) -> integer_to_list(A, 10);
to_string(A) when is_float(A) -> float_to_list(A);
to_string(A) when is_binary(A) -> binary_to_list(A);
to_string(A) when is_atom(A) -> atom_to_list(A);
to_string(A) when is_list(A) -> A.

format(Format) -> lists:flatten(Format).
format(Format, N0) -> lists:flatten(io_lib:format(Format, [N0])).
format(Format, N0, N1) -> lists:flatten(io_lib:format(Format, [N0, N1])).
format(Format, N0, N1, N2) -> lists:flatten(io_lib:format(Format, [N0, N1, N2])).
format(Format, N0, N1, N2, N3) -> lists:flatten(io_lib:format(Format, [N0, N1, N2, N3])).
format(Format, N0, N1, N2, N3, N4) -> lists:flatten(io_lib:format(Format, [N0, N1, N2, N3, N4])).

