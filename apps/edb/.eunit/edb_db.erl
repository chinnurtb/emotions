-module(edb_db).

-export([
	fetch/2,
	store/1,
	get_by_index/4,
	delete/1
]).

fetch(Bucket, Key) ->
	{ok, _} = pooler:use_member(fetch_fun(Bucket, Key)).

fetch_fun(Bucket, Key) ->
	fun(Pid) -> riakc_pb_socket:get(Pid, Bucket, Key) end.

store(Obj) ->
	{ok, _} = pooler:use_member(store_fun(Obj)).

store_fun(Obj) ->
	fun(Pid) -> riakc_pb_socket:put(Pid, Obj, [return_body]) end.

get_by_index(Bucket, Type, Index, IndexValue) ->
	{ok, _} = pooler:use_member(get_by_index_fun(Bucket, Type, Index, IndexValue)).

get_by_index_fun(Bucket, Type, Index, IndexValue) ->
	fun(Pid) -> riakc_pb_socket:get_index(Pid, Bucket, edb_obj:index(Type, Index), IndexValue) end.

delete(Obj)->
	ok = pooler:use_member(delete_fun(Obj)).

delete_fun(Obj) ->
	fun(Pid) -> riakc_pb_socket:delete_obj(Pid, Obj) end.
