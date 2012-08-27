-module(estring).
-export([to_string/1]).

to_string(A) when is_integer(A) -> integer_to_list(A, 10);
to_string(A) when is_float(A) -> float_to_list(A);
to_string(A) when is_binary(A) -> binary_to_list(A);
to_string(A) when is_atom(A) -> atom_to_list(A);
to_string(A) when is_list(A) -> A.
