-module(libdb_datetime_field_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_DTV, {{2012, 8, 31}, {1, 34, 32}}).
-define(TEST_DTS, "31.08.2012 01:34:32").

default_params_test_() ->
	{foreach,
		fun() -> libdb_datetime_field:new(created) end,
		fun(_) -> ok end,
		[fun(Mod) -> [
						?_assertEqual(created, Mod:name()),
						?_assertEqual(63513596072, Mod:to_json({{2012, 8, 31},{1, 34, 32}})),
						?_assertEqual({{2012, 8, 31},{1, 34, 32}}, Mod:from_json(63513596072)),
						?_assertEqual(none, Mod:index_type()),
						?_assertEqual(false, Mod:is_indexed()),
						?_assertEqual(false, Mod:is_unique()),
						?_test(begin
								meck:new(edatetime),
								meck:expect(edatetime, format, fun(_, ?TEST_DTV) -> ?TEST_DTS end),
								?assertEqual(?TEST_DTS, Mod:to_html(?TEST_DTV)),
								?assert(meck:validate(edatetime)),
								meck:unload(edatetime)
							end),
						?_test(begin
								meck:new(edatetime),
								meck:expect(edatetime, parse, fun(_, ?TEST_DTS) -> ?TEST_DTV end),
								?assertEqual(?TEST_DTV, Mod:from_html(?TEST_DTS)),
								?assert(meck:validate(edatetime)),
								meck:unload(edatetime)
							end),
						?_test(begin
								meck:new(libdb_validators),
								meck:expect(libdb_validators, validate_type, fun(_) -> ok end),
								meck:expect(libdb_validators, validate,
									fun(?TEST_DTV, [_]) -> ok end),
								?assertEqual(ok, Mod:validate(?TEST_DTV)),
								?assert(meck:validate(libdb_validators)),
								meck:unload(libdb_validators)
							end)
		] end]
	}.
