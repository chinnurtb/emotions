-module(edb_user).

-export([
	new/2,
	construct/1,
	find/1
]).

-record(user, {
		id :: string(),
		email :: string(),
		pass_hash :: string(),
		pass_salt :: binary(),
		created :: calendar:datetime(),
		updated :: calendar:datetine()

	}).

bucket() -> <<"users">>.
email_idx() -> "email".

-spec find(Key :: binary()) -> #user{}.
find(Key) ->
	{ok, O} = edb_db:fetch(bucket(), Key),
	{ok, from_object(O)}.


-spec construct(PropLists :: [{term(), term()}]) -> #user{}.
construct(PropLists) ->
	#user{
		id = proplists:get_value(id, PropLists, undefined),
		email = proplists:get_value(email, PropLists, undefined),
		pass_hash = proplists:get_value(pass_hash, PropLists, undefined),
		pass_salt = proplists:get_value(pass_salt, PropLists, undefined),
		created = proplists:get_value(created, PropLists, undefined),
		updated = proplists:get_value(updated, PropLists, undefined)
	}.

-spec new(Email :: string(), Password :: string()) ->
	{ok, #user{}} | {error, email_busy}.
new(Email, Password) ->
	User = create_new_user(Email, Password),
	UserObj = store(User),
	Othres = find_by_email(Email),
	reject_if_busy(User, UserObj, Othres).

find_by_email(Email) ->
	{ok, Others} = edb_db:get_by_index(bucket(), bin, email_idx(), Email),
	lists:map(fun(K) -> {ok, U} = ?MODULE:find(K), U end, Others).

store(User) ->
	UpdatedUser = update_user(User),
	Obj = to_object(UpdatedUser),
	{ok, UserObj} = edb_db:store(Obj),
	UserObj.

update_user(User) ->
	User#user{updated = calendar:universal_time()}.

reject_if_busy(User = #user{created = C, id = Id}, UserObj, Others) ->
	case lists:any(fun(#user{created = C1, id = Id1}) -> C1 < C andalso Id =/= Id1 end, Others) of
		true ->
			edb_db:delete(UserObj),
			{error, email_busy};
		false -> {ok, User}
	end.

create_new_user(Email, Password) ->
	Salt = edb_id:new(),
	#user { id = edb_id:new(),
			email = Email,
			pass_salt = Salt,
			pass_hash = salt_and_hash_password(Password, Salt),
			created = calendar:universal_time(),
			updated = calendar:universal_time()}.


to_object(User = #user{id = Id, email = Email}) ->
	Obj = edb_obj:new(bucket(), Id, to_json(User)),
	edb_obj:set_index(Obj, bin, email_idx(), Email).

from_object(Obj) ->
	Json = edb_obj:get_value(Obj),
	from_json(Json).

to_json(#user{id=Id, email=Email, pass_hash=P, pass_salt=S, created=C, updated=U}) ->
	edb_json:to_json([{id, Id},
					  {email,Email},
					  {pass_hash,P},
					  {pass_salt,S},
					  {created, calendar:datetime_to_gregorian_seconds(C)},
					  {updated, calendar:datetime_to_gregorian_seconds(U)}], fun is_string/1).

from_json(Json) ->
	PropList = edb_json:from_json(Json, fun is_string/1),
	#user{
		id = proplists:get_value(id, PropList),
		email = proplists:get_value(email, PropList),
		pass_hash = proplists:get_value(pass_hash, PropList),
		pass_salt = proplists:get_value(pass_salt, PropList),
		created = calendar:gregorian_seconds_to_datetime(proplists:get_value(created, PropList)),
		updated = calendar:gregorian_seconds_to_datetime(proplists:get_value(updated, PropList))
	}.

is_string(id) -> true;
is_string(email) -> true;
is_string(pass_hash) -> true;
is_string(pass_salt) -> true;
is_string(_) -> false.

salt_and_hash_password(Password, Salt) ->
	Data = list_to_binary(lists:flatten(io_lib:format("~p++SICRET++~s", [Salt, Password]))),
	<<Number:160/integer>> = crypto:sha(Data),
	integer_to_list(Number, 16).
