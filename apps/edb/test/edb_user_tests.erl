-module(edb_user_tests).

-include_lib("eunit/include/eunit.hrl").

new_user_test_() ->
	{foreach,

		fun() ->
				Modules = [edb_db, edb_id, edb_obj, edb_user],
				meck:new(Modules, [unstick, passthrough]),
				Modules end,
		fun(Modules) -> meck:unload(Modules) end,
		[
			fun(Ms) -> [?_test(
						begin
							meck:expect(edb_id, new, fun() -> <<1>> end),
							meck:expect(edb_db, store, fun(Obj) -> {ok, Obj} end),
							meck:expect(edb_db, get_by_index, fun(<<"users">>, bin, "email", "test@example.org") -> {ok, []} end),
							?assertMatch({ok, _}, edb_user:new("test@example.org", "123456")),
							?assert(meck:validate(Ms))
						end)]
			end,
			fun(Ms) -> [?_test(
						begin
							U = {ok, edb_user:construct([{id, <<2>>},{created, {{0, 1, 1}, {0, 0, 0}}}])},
							meck:expect(edb_user, find, fun(<<2>>) -> U end),
							meck:expect(edb_id, new, fun() -> <<1>> end),
							meck:expect(edb_obj, get_value, fun(O) -> O end),
							meck:expect(edb_db, store, fun(Obj) -> {ok, Obj} end),
							meck:expect(edb_db, get_by_index, fun(<<"users">>, bin, "email", "test@example.org") -> {ok, [<<2>>]} end),
							meck:expect(edb_db, fetch, fun(<<"users">>, <<2>>) -> U end),
							meck:expect(edb_db, delete, fun(_) -> ok end),
							?assertEqual({error, email_busy}, edb_user:new("test@example.org", "123456")),
							?assert(meck:validate(Ms))
						end)]
			end,
			fun(Ms) -> [?_test(
						begin
							U = {ok, edb_user:construct([{id, <<2>>},{created, {{99990, 1, 1}, {0, 0, 0}}}])},
							meck:expect(edb_user, find, fun(<<2>>) -> U end),
							meck:expect(edb_id, new, fun() -> <<1>> end),
							meck:expect(edb_obj, get_value, fun(O) -> O end),
							meck:expect(edb_db, store, fun(Obj) -> {ok, Obj} end),
							meck:expect(edb_db, get_by_index, fun(<<"users">>, bin, "email", "test@example.org") -> {ok, [<<2>>]} end),
							meck:expect(edb_db, fetch, fun(<<"users">>, <<2>>) -> U end),
							?assertMatch({ok, _}, edb_user:new("test@example.org", "123456")),
							?assert(meck:validate(Ms))
						end)]
			end
		]
	}.
