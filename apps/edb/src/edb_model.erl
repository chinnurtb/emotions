-module(edb_model).

-type field_type() :: input_text |
					  input_email |
					  input_password |
					  input_date.

-type index_type() :: int | bin.

-type indexed_type() :: boolean() | unique.

-type validator_fun() :: fun((Value :: term()) -> boolean()).
-type transform_fun() :: fun((Dir :: to_string | from_string, Value :: term()) -> term() | string()).

-record(field, {name :: atom(),
				type :: field_type(),
				index_type :: index_type(),
				unique = false :: boolean(),
				value :: term(),
				validators :: [validator_fun()],
				transform :: transform_fun(),
				errors = [] :: [string()]}).


-type field() :: #field{}.

-record(model, {name :: string(), fields :: [field()]}).

-type model() :: #model{}.

-export([
	validate_length/1,
	validate_presents/0,
	validate_with/2,
	validate_type/1,
	validate_contains/2,
	model/2,
	to_json/1,
	from_json/2,
	validate/1,
	get_field_value/2,
	get_field_errors/2,
	set_field/3,
	set_fields/2,
	text_field/1,
	text_field/2,
	text_field/3,
	int_field/1,
	int_field/2,
	int_field/3,
	float_field/1,
	float_field/2,
	float_field/3,
	email_field/1,
	email_field/2,
	email_field/3,
	password_field/1,
	password_field/2,
	password_field/3,
	date_field/1,
	date_field/2,
	date_field/3,
	time_field/1,
	time_field/2,
	time_field/3,
	datetime_field/1,
	datetime_field/2,
	datetime_field/3
]).

-spec model(Name :: string(), Fields :: [field()]) -> model() | no_return().
model(Name, Fields) -> #model{name = Name, fields = validate_fields(Fields ++ [id_field(), datetime_field(created), datetime_field(updated)])}.

validate_fields(Fields) ->
	validate_fields(Fields, [], [], []).

validate_fields([], _Names, Fields, []) -> Fields;
validate_fields([], _Names, _Fields, DubNames) -> throw({dublicated_fiels, DubNames});
validate_fields([H = #field{name = Name} | T], Names, Fields, DubNames) ->
	case lists:member(Name, Names) of
		true -> validate_fields(T, Names, Fields, [Name | DubNames]);
		false -> validate_fields(T, [Name | Names], [H | Fields], DubNames)
	end.

-spec get_field_value(Model :: model(), FieldName :: atom()) -> term().
get_field_value(Model, FieldName) ->
	(get_field_record(Model, FieldName))#field.value.

-spec get_field_errors(Model :: model(), FieldName ::atom()) -> [string()].
get_field_errors(Model, FieldName) ->
	(get_field_record(Model, FieldName))#field.errors.

get_field_record(Model, FieldName) ->
	case lists:keyfind(FieldName, #field.name, Model#model.fields) of
		false -> throw({unknown_field, Model#model.name, FieldName});
		Field -> Field
	end.


-spec set_field(Model :: model(), FieldName :: string(), Value :: term()) -> model().
set_field(Model, FieldName, Value) ->
	set_fields(Model, [{FieldName, Value}]).

-spec set_fields(Model :: model(), [{FieldName :: string(), Value :: term()}]) -> model().
set_fields(Model, FieldNameValueList) ->
	NewFields = change_fields(Model#model.fields, FieldNameValueList, [], Model#model.name),
	Model#model{fields = NewFields}.

change_fields([], [], Result, _ModelName) ->
	Result;
change_fields([], FVPL, _Result, ModelName) ->
	throw({unknown_field, ModelName, FVPL});
change_fields([Field | Tail], FVPL, Result, ModelName) ->
	Name = Field#field.name,
	case lists:keyfind(Name, 1, FVPL) of
		false -> change_fields(Tail, FVPL, [Field | Result], ModelName);
		{Name, Value} -> change_fields(Tail,
									   lists:keydelete(Field#field.name, 1, FVPL),
									   [Field#field{value = Value} | Result],
									   ModelName)
	end.

-spec validate(Model :: model()) -> {ok, Model} | {error, Model}.
validate(Model = #model{fields = Fiels}) ->
	NewFields = [validate_field(Field) || Field <- Fiels],
	WasErrors = lists:any(fun(#field{errors = L}) -> length(L) =/= 0 end, NewFields),
	case WasErrors of
		true -> {error, Model#model{fields = NewFields}};
		false -> {ok, Model}
	end.

validate_field(F = #field{value = Value, validators = ValidatorFuns}) ->
	ValidatesResults = [Fun(Value) || Fun <- ValidatorFuns],
	Errors = lists:foldl(fun(ok, Acc) -> Acc; ({error, M}, Acc) -> [M|Acc] end, [], ValidatesResults),
	F#field{errors = Errors}.


bucket(Model) ->
	list_to_binary(estring:to_string(Model#model.name)).

id(Model) ->
	list_to_binary(get_field_value(Model, id)).

to_db_object(Model) ->
	Obj = edb_obj:new(bucket(Model), id(Model), to_json(Model)),
	set_indexes(Obj, Model#model.fields).

set_indexes(Obj, Fields) ->
	lists:foldl(fun set_field_index/2, Obj, Fields).

set_field_index(#field{index_type = undefined}, Obj) -> Obj;
set_field_index(#field{index_type = Type, value = Value, name = Name}, Obj) ->
	edb_obj:set_index(Obj, Type, Name, Value).

-spec to_json(Model :: model()) -> string().
to_json(Model = #model{}) ->
	{ok, Model} = validate(Model),
	JSS = {struct, lists:map(fun field_to_json/1, Model#model.fields)},
	list_to_binary(mochijson2:encode(JSS)).

-spec from_json(Json :: binary(), Model :: model()) -> {ok, model()} | no_return().
from_json(Json, Model = #model{}) ->
	{struct, PropList} = mochijson2:decode(Json),
	FieldsVK = lists:map(fun(PI) -> field_from_json(PI, Model) end, PropList),
	set_fields(Model, FieldsVK).

field_from_json({K, V}, Model) ->
	FieldName = list_to_atom(binary_to_list(K)),
	Field = get_field_record(Model, FieldName),
	{FieldName, (Field#field.transform)(from_json, V)}.



field_to_json(#field{name = Name, value = Value, transform = TransformFun}) ->
	{list_to_binary(estring:to_string(Name)), TransformFun(to_json, Value)}.

id_field() ->
	id_field(binary_to_list(edb_id:new())).

id_field(Id) ->
	#field {
		name = id,
		type = hidden,
		validators = [validate_presents()],
		unique = true,
		index_type = bin,
		transform = fun string_transform/2,
		value = Id
	}.

-spec text_field(Name :: string()) -> field().
text_field(Name) ->
	text_field(Name, false, []).

-spec text_field(Name :: string(), indexed_type() | validator_fun() | [validator_fun()]) -> field().
text_field(Name, true) ->
	text_field(Name, true, []);
text_field(Name, false) ->
	text_field(Name, false, []);
text_field(Name, unique) ->
	text_field(Name, unique, []);
text_field(Name, Validator) when is_function(Validator, 1) ->
	text_field(Name, false, [Validator]);
text_field(Name, [Validator]) when is_function(Validator, 1) ->
	text_field(Name, false, [Validator]);
text_field(Name, Validators = [V | _]) when is_function(V, 1) ->
	text_field(Name, false, Validators).

-spec text_field(Name :: string(), indexed_type(), [validator_fun()]) -> field().
text_field(Name, Indexed, Validators) ->
	IndexType = bin_field_index_type(Indexed),
	#field {
		name = Name,
		type = input_text,
		index_type = IndexType,
		unique = Indexed =:= unique,
		validators = [validate_type(string)] ++ Validators,
		transform = fun string_transform/2
	}.

-spec int_field(Name :: string()) -> field().
int_field(Name) ->
	int_field(Name, false, []).

-spec int_field(Name :: string(), indexed_type() | validator_fun() | [validator_fun()]) -> field().
int_field(Name, true) ->
	int_field(Name, true, []);
int_field(Name, false) ->
	int_field(Name, false, []);
int_field(Name, unique) ->
	int_field(Name, unique, []);
int_field(Name, Validator) when is_function(Validator, 1) ->
	int_field(Name, false, [Validator]);
int_field(Name, [Validator]) when is_function(Validator, 1) ->
	int_field(Name, false, [Validator]);
int_field(Name, Validators = [V | _]) when is_function(V, 1) ->
	int_field(Name, false, Validators).

-spec int_field(Name :: string(), indexed_type(), [validator_fun()]) -> field().
int_field(Name, Indexed, Validators) ->
	IndexType = int_field_index_type(Indexed),
	#field {
		name = Name,
		type = input_int,
		index_type = IndexType,
		unique = Indexed =:= unique,
		validators = [validate_type(integer)] ++ Validators,
		transform = fun integer_transform/2
	}.

-spec float_field(Name :: string()) -> field().
float_field(Name) ->
	float_field(Name, false, []).

-spec float_field(Name :: string(), indexed_type() | validator_fun() | [validator_fun()]) -> field().
float_field(Name, true) ->
	float_field(Name, true, []);
float_field(Name, false) ->
	float_field(Name, false, []);
float_field(Name, unique) ->
	float_field(Name, unique, []);
float_field(Name, Validator) when is_function(Validator, 1) ->
	float_field(Name, false, [Validator]);
float_field(Name, [Validator]) when is_function(Validator, 1) ->
	float_field(Name, false, [Validator]);
float_field(Name, Validators = [V | _]) when is_function(V, 1) ->
	float_field(Name, false, Validators).

-spec float_field(Name :: string(), indexed_type(), [validator_fun()]) -> field().
float_field(Name, Indexed, Validators) ->
	IndexType = bin_field_index_type(Indexed),
	#field {
		name = Name,
		type = input_float,
		index_type = IndexType,
		unique = Indexed =:= unique,
		validators = [validate_type(float)] ++ Validators,
		transform = fun float_transform/2
	}.

-spec email_field(Name :: string()) -> field().
email_field(Name) ->
	email_field(Name, false, []).

-spec email_field(Name :: string(), indexed_type() | validator_fun() | [validator_fun()]) -> field().
email_field(Name, true) ->
	email_field(Name, true, []);
email_field(Name, false) ->
	email_field(Name, false, []);
email_field(Name, unique) ->
	email_field(Name, unique, []);
email_field(Name, Validator) when is_function(Validator, 1) ->
	email_field(Name, false, [Validator]);
email_field(Name, [Validator]) when is_function(Validator, 1) ->
	email_field(Name, false, [Validator]);
email_field(Name, Validators = [V | _]) when is_function(V, 1) ->
	email_field(Name, false, Validators).

-spec email_field(Name :: string(), indexed_type(), [validator_fun()]) -> field().
email_field(Name, Indexed, Validators) ->
	IndexType = bin_field_index_type(Indexed),
	#field {
		name = Name,
		type = input_email,
		index_type = IndexType,
		unique = Indexed =:= unique,
		validators = [validate_type(string), validate_contains("@", "dosen't look like email")] ++ Validators,
		transform = fun string_transform/2
	}.

-spec password_field(Name :: string()) -> field().
password_field(Name) ->
	password_field(Name, false, []).

-spec password_field(Name :: string(), boolean() | validator_fun() | [validator_fun()]) -> field().
password_field(Name, true) ->
	password_field(Name, true, []);
password_field(Name, false) ->
	password_field(Name, false, []);
password_field(Name, Validator) when is_function(Validator, 1) ->
	password_field(Name, false, [Validator]);
password_field(Name, [Validator]) when is_function(Validator, 1) ->
	password_field(Name, false, [Validator]);
password_field(Name, Validators = [V | _]) when is_function(V, 1) ->
	password_field(Name, false, Validators).

-spec password_field(Name :: string(), boolean(), [validator_fun()]) -> field().
password_field(Name, Indexed, Validators) when is_boolean(Indexed) ->
	IndexType = bin_field_index_type(Indexed),
	#field {
		name = Name,
		type = input_password,
		index_type = IndexType,
		validators = [validate_type(string)] ++ Validators,
		transform = fun string_transform/2
	}.

-spec date_field(Name :: string()) -> field().
date_field(Name) ->
	date_field(Name, false, []).

-spec date_field(Name :: string(), boolean() | validator_fun() | [validator_fun()]) -> field().
date_field(Name, true) ->
	date_field(Name, true, []);
date_field(Name, false) ->
	date_field(Name, false, []);
date_field(Name, Validator) when is_function(Validator, 1) ->
	date_field(Name, false, [Validator]);
date_field(Name, [Validator]) when is_function(Validator, 1) ->
	date_field(Name, false, [Validator]);
date_field(Name, Validators = [V | _]) when is_function(V, 1) ->
	date_field(Name, false, Validators).

-spec date_field(Name :: string(), boolean(), [validator_fun()]) -> field().
date_field(Name, Indexed, Validators) when is_boolean(Indexed) ->
	IndexType = int_field_index_type(Indexed),
	#field {
		name = Name,
		type = input_date,
		index_type = IndexType,
		validators = [validate_type(date)] ++ Validators,
		transform = fun date_transform/2
	}.

-spec time_field(Name :: string()) -> field().
time_field(Name) ->
	time_field(Name, false, []).

-spec time_field(Name :: string(), boolean() | validator_fun() | [validator_fun()]) -> field().
time_field(Name, true) ->
	time_field(Name, true, []);
time_field(Name, false) ->
	time_field(Name, false, []);
time_field(Name, Validator) when is_function(Validator, 1) ->
	time_field(Name, false, [Validator]);
time_field(Name, [Validator]) when is_function(Validator, 1) ->
	time_field(Name, false, [Validator]);
time_field(Name, Validators = [V | _]) when is_function(V, 1) ->
	time_field(Name, false, Validators).

-spec time_field(Name :: string(), boolean(), [validator_fun()]) -> field().
time_field(Name, Indexed, Validators) when is_boolean(Indexed) ->
	IndexType = int_field_index_type(Indexed),
	#field {
		name = Name,
		type = input_time,
		index_type = IndexType,
		validators = [validate_type(time)] ++ Validators,
		transform = fun time_transform/2
	}.

-spec datetime_field(Name :: string()) -> field().
datetime_field(Name) ->
	datetime_field(Name, false, []).

-spec datetime_field(Name :: string(), boolean() | validator_fun() | [validator_fun()]) -> field().
datetime_field(Name, true) ->
	datetime_field(Name, true, []);
datetime_field(Name, false) ->
	datetime_field(Name, false, []);
datetime_field(Name, Validator) when is_function(Validator, 1) ->
	datetime_field(Name, false, [Validator]);
datetime_field(Name, [Validator]) when is_function(Validator, 1) ->
	datetime_field(Name, false, [Validator]);
datetime_field(Name, Validators = [V | _]) when is_function(V, 1) ->
	datetime_field(Name, false, Validators).

-spec datetime_field(Name :: string(), boolean(), [validator_fun()]) -> field().
datetime_field(Name, Indexed, Validators) when is_boolean(Indexed) ->
	IndexType = int_field_index_type(Indexed),
	#field {
		name = Name,
		type = input_datetime,
		index_type = IndexType,
		validators = [validate_type(datetime)] ++ Validators,
		transform = fun datetime_transform/2
	}.

bin_field_index_type(_Indexed = true) -> bin;
bin_field_index_type(unique) -> bin;
bin_field_index_type(false) -> undefined.

int_field_index_type(_Indexed = true) -> int;
int_field_index_type(unique) -> int;
int_field_index_type(false) -> undefined.


string_transform(to_json, String) when is_list(String) -> list_to_binary(String);
string_transform(from_json, Binary) when is_binary(Binary) -> binary_to_list(Binary);
string_transform(_, String) when is_list(String) -> String.

integer_transform(to_string, Int) when is_integer(Int) -> integer_to_list(Int, 10);
integer_transform(from_string, String) when is_list(String) -> try list_to_integer(String, 10) catch _:badarg -> String end;
integer_transform(_, V) -> V.

float_transform(to_string, Float) when is_float(Float) -> float_to_list(Float);
float_transform(from_string, String) when is_list(String) -> try list_to_float(String) catch _:badarg -> String end;
float_transform(_, V) -> V.

date_transform(to_string, {Year, Month, Day}) -> estring:format("~4..0w-~2..0w-~2..0w", Year, Month, Day);
date_transform(from_string, String) ->
	try
		[Year, Month, Day] = [list_to_integer(X) || X <- string:tokens(String, "-")],
		{Year, Month, Day}
	catch
		_:_ -> String
	end;
date_transform(to_json, Date) -> calendar:datetime_to_gregorian_seconds({Date, {0, 0, 0}});
date_transform(from_json, Seconds) -> {Date, _Time} = calendar:gregorian_seconds_to_datetime(Seconds), Date.

time_transform(to_string, {H, M, S}) -> estring:format("~2..0w:~2..0w:~2..0w", H, M, S);
time_transform(from_string, String) ->
	try
		[H, M, S] = [list_to_integer(X) || X <- string:tokens(String, "-")],
		{H, M, S}
	catch
		_:_ -> String
	end;
time_transform(to_json, Time) -> calendar:time_to_seconds(Time);
time_transform(from_json, Seconds) -> calendar:seconds_to_time(Seconds).

datetime_transform(to_string, {Date, Time}) -> estring:format("~s ~s", date_transform(to_string, Date), time_transform(to_string, Time));
datetime_transform(from_string, String) ->
	try
		[DateStr, TimeStr] = strings:tokens(String, " "),
		Date = date_transform(from_string, DateStr),
		Time = time_transform(from_string, TimeStr),
		{Date, Time}
	catch
		_:_ -> String
	end;
datetime_transform(to_json, Datetime) -> calendar:datetime_to_gregorian_seconds(Datetime);
datetime_transform(from_json, Seconds) -> calendar:gregorian_seconds_to_datetime(Seconds).

validate_with(ValidateFun, Message) ->
	fun(Value) -> validate_fun(Value, ValidateFun, Message) end.

validate_fun(Arg, CheckFun, Message) ->
	case CheckFun(Arg) of
		true -> ok;
		false -> {error, Message}
	end.

length_eq_validate_fun(String, N) ->
	length(String) =:= N.

length_in_validate_fun(String, A, B) ->
	L = length(String),
	A =< L andalso L =< B.

length_gt_validate_fun(String, N) ->
	length(String) > N.

length_ge_validate_fun(String, N) ->
	length(String) >= N.

length_lt_validate_fun(String, N) ->
	length(String) < N.

length_le_validate_fun(String, N) ->
	length(String) =< N.

validate_p_and_str(S) ->
	S =:= undefined orelse not is_list(S).

validate_length({eq, N}) when is_integer(N), N >= 0 ->
	validate_with(fun(SA) -> validate_p_and_str(SA) orelse length_eq_validate_fun(SA, N) end, estring:format("length not equal ~w", N));
validate_length({in, A, B}) when is_integer(A), is_integer(B), A >= 0, B > A ->
	validate_with(fun(SA) -> validate_p_and_str(SA) orelse length_in_validate_fun(SA, A, B) end, estring:format("length not in [~w, ~w]", A, B));
validate_length({gt, N}) when is_integer(N), N >= 0 ->
	validate_with(fun(SA) -> validate_p_and_str(SA) orelse length_gt_validate_fun(SA, N) end, estring:format("length less or equal then ~w", N));
validate_length({ge, N}) when is_integer(N), N >= 0 ->
	validate_with(fun(SA) -> validate_p_and_str(SA) orelse length_ge_validate_fun(SA, N) end, estring:format("length less then ~w", N));
validate_length({lt, N}) when is_integer(N), N >= 0 ->
	validate_with(fun(SA) -> validate_p_and_str(SA) orelse length_lt_validate_fun(SA, N) end, estring:format("length greater or equal then ~w", N));
validate_length({le, N}) when is_integer(N), N >= 0 ->
	validate_with(fun(SA) -> validate_p_and_str(SA) orelse length_le_validate_fun(SA, N) end, estring:format("length greater then ~w", N)).

validate_presents() ->
	validate_with(fun(V) -> V =/= undefined end, estring:format("should be present")).


validate_type(integer) ->
	validate_with(fun(V) -> is_integer(V) orelse V =:= undefined end, estring:format("should be integer"));
validate_type(float) ->
	validate_with(fun(V) -> is_float(V) orelse V =:= undefined end, estring:format("should be float"));
validate_type(string) ->
	validate_with(fun(V) -> is_list(V) orelse V =:= undefined end, estring:format("should be string"));
validate_type(date) ->
	validate_with(fun(V) -> V =:= undefined orelse calendar:valid_date(V)  end, estring:format("should be a date"));
validate_type(time) ->
	validate_with(fun(V) -> V =:= undefined orelse valid_time(V) end, estring:format("should be a time"));
validate_type(datetime) ->
	validate_with(fun(V) -> V =:= undefined orelse valid_datetime(V)  end, estring:format("should be a datetime")).

valid_time({H, M, S})
		when is_integer(H), is_integer(M), is_integer(S), 0 =< H, H =< 23, 0 =< M, M =< 59, 0 =< S, S =< 59 -> true;
valid_time(_) -> false.

valid_datetime({Date, Time}) -> calendar:valid_date(Date) andalso valid_time(Time);
valid_datetime(_) -> false.

validate_contains(Substr, Message) ->
	validate_with(fun(V) -> V =:= undefined orelse string:str(V, Substr) >= 0 end, Message).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


model_to_and_from_json_test() ->
	Model = model(test_model, [int_field(int_field), text_field(text_field)]),
	Model1 = set_fields(Model, [{int_field, 10}, {text_field, "test"}, {created, calendar:universal_time()}, {updated, calendar:universal_time()}]),
	Json = to_json(Model1),
	Model2 = from_json(Json, Model),
	?assertEqual(Model1, Model2).

create_model_and_get_field_test_() ->
	{setup,
		fun() -> model(test_model, [int_field(int), text_field(text), float_field(float)]) end,
		fun(M) -> [
			?_assertEqual(undefined, get_field_value(M, int)),
			?_assertEqual(undefined, get_field_value(M, text)),
			?_assertEqual(undefined, get_field_value(M, float))
		] end
	}.

create_model_set_fields_get_field_test_() ->
	{setup,
		fun() ->
				M = model(test_model, [int_field(int), text_field(text), float_field(float)]),
				set_fields(M, [{int, 10}, {text, "text"}, {float, 1.0}]) end,
		fun(M) -> [
			?_assertEqual(10, get_field_value(M, int)),
			?_assertEqual("text", get_field_value(M, text)),
			?_assertEqual(1.0, get_field_value(M, float))
		] end
	}.

create_model_single_set_fields_get_field_test_() ->
	{setup,
		fun() ->
				M = model(test_model, [int_field(int), text_field(text), float_field(float)]),
				M1 = set_field(M, int, 10),
				M2 = set_field(M1, text, "text"),
				set_field(M2, float, 1.0) end,
		fun(M) -> [
			?_assertEqual(10, get_field_value(M, int)),
			?_assertEqual("text", get_field_value(M, text)),
			?_assertEqual(1.0, get_field_value(M, float))
		] end
	}.

unknown_field_test() ->
	M = model(test_model, [int_field(int), text_field(text), float_field(float)]),
	?assertThrow({unknown_field, test_model, int2}, get_field_value(M, int2)).

validateion_default_test() ->
	try
	M = model(test_model, [text_field(text),
						   int_field(int),
						   float_field(float),
						   email_field(email),
						   password_field(password),
						   date_field(date),
						   time_field(time),
						   datetime_field(datetime)]),
	?assertEqual({ok, M}, validate(M))
	catch
		A:B -> erlang:display({A, B, erlang:get_stacktrace()})
	end.


validateion_2_test() ->
	M = model(test_model, [int_field(int)]),
	M1 = set_field(M, int, "lala"),
	{error, M2} = validate(M1),
	?assertEqual(1, length(get_field_errors(M2, int))).

validateion_3_test() ->
	M = model(test_model, [int_field(int, validate_presents())]),
	{error, M1} = validate(M),
	?assertEqual(1, length(get_field_errors(M1, int))).

length_validate_with_undefiend_and_non_string_test_() -> [
		?_assertEqual(ok, (validate_length({eq, 10}))(undefiend)),
		?_assertEqual(ok, (validate_length({eq, 10}))(10)),
		?_assertEqual(ok, (validate_length({in, 1, 10}))(undefined)),
		?_assertEqual(ok, (validate_length({in, 1, 10}))(atom)),
		?_assertEqual(ok, (validate_length({gt, 1}))(undefiend)),
		?_assertEqual(ok, (validate_length({gt, 10}))(1.0)),
		?_assertEqual(ok, (validate_length({ge, 10}))(undefined)),
		?_assertEqual(ok, (validate_length({ge, 10}))({some, tuple})),
		?_assertEqual(ok, (validate_length({lt, 1}))(undefiend)),
		?_assertEqual(ok, (validate_length({lt, 10}))(1.0)),
		?_assertEqual(ok, (validate_length({le, 10}))(undefined)),
		?_assertEqual(ok, (validate_length({le, 10}))({some, tuple}))
	].

length_validate_test_() -> [
		?_assertMatch(ok, (validate_length({eq, 10}))("aaaaaaaaaa")),
		?_assertMatch({error, _}, (validate_length({eq, 10}))("aaaaa")),
		?_assertMatch(ok, (validate_length({in, 3, 10}))("aaaa")),
		?_assertMatch({error, _}, (validate_length({in, 3, 10}))("aa")),
		?_assertMatch({error, _}, (validate_length({in, 3, 10}))("aaaaaaaaaaa")),
		?_assertMatch(ok, (validate_length({gt, 5}))("aaaaaa")),
		?_assertMatch({error, _}, (validate_length({gt, 5}))("aaaaa")),
		?_assertMatch(ok, (validate_length({ge, 5}))("aaaaa")),
		?_assertMatch({error, _}, (validate_length({ge, 5}))("aaaa")),
		?_assertMatch(ok, (validate_length({lt, 5}))("aaa")),
		?_assertMatch({error, _}, (validate_length({lt, 5}))("aaaaa")),
		?_assertMatch(ok, (validate_length({le, 5}))("aaaaa")),
		?_assertMatch({error, _}, (validate_length({le, 5}))("aaaaaa"))
	].

-endif.
