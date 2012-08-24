-module(edb_cli).

-export([
	save/3,
	read/4,
	delete/3
]).

-type client() :: pid().
-type edb_obj() :: edb_obj:edb_obj().
-type edb_obj_key() :: edb_obj:key().
-type edb_obj_bucket() :: edb_obj:bucket().

-spec save(client(), edb_obj(), integer()) -> {ok, edb_obj()} | {error, term()}.
save(Client, EdbObj, Timeout) ->
	riakc_pb_socket:put(Client, EdbObj, Timeout).

-spec read(client(), edb_obj_bucket(), edb_obj_key(), integer()) -> {ok, edb_obj()} | {error, term()}.
read(Client, Bucket, Key, Timeout) ->
	riakc_pb_socket:get(Client, Bucket, Key, Timeout).

-spec delete(client(), edb_obj(), integer()) -> ok | {error, term()}.
delete(Client, EdbObj, Timeout) ->
	riakc_pb_socket:delete_obj(Client, EdbObj, [], Timeout).
