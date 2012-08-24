-module(edb_obj).

-export([
	new/3,
	new/4,
	update/2,
	update/3,
	set_index/4,
	set_indexes/2,
	get_index/3,
	get_value/1,
	index/2
]).

-define(INDEX_KEY, <<"index">>).
-define(INDEX_SUFFIX_INT, <<"_int">>).
-define(INDEX_SUFFIX_BIN, <<"_bin">>).

-type bucket() :: binary().
-type key() :: binary().
-type value() :: binary().
-type edb_obj() :: riakc_obj:riakc_obj().
-type index_type() :: int | bin.
-type index_name() :: string().
-type index_value() :: string() | binary().

-export_type([edb_obj/0, key/0]).

-spec new(bucket(), key(), value()) ->  edb_obj().
new(Bucket, Key, Data) ->
	new(Bucket, Key, Data, "application/json").

-spec new(bucket(), key(), value(), string()) -> edb_obj().
new(Bucket, Key, Data, MIMEType) ->
	riakc_obj:new(Bucket, Key, Data, MIMEType).

-spec update(edb_obj(), value()) -> edb_obj().
update(EdbObj, Data) ->
	riakc_obj:update_value(EdbObj, Data).

-spec update(edb_obj(), value(), string()) -> edb_obj().
update(EdbObj, Data, NewMIMEType) ->
	riakc_obj:update_value(EdbObj, Data, NewMIMEType).

-spec get_value(edb_obj()) -> value().
get_value(EdbObj) ->
	riakc_obj:get_value(EdbObj).

-spec set_index(edb_obj(), index_type(), index_name(), index_value()) -> edb_obj().
set_index(EdbObj, Type, Name, Value) ->
	Meta = riakc_obj:get_update_metadata(EdbObj),
	Index = case dict:find(?INDEX_KEY, Meta) of
		error -> [];
		{ok, I} -> I
	end,
	NewIndex = dict:to_list(dict:store(index(Type, Name), value(Value), dict:from_list(Index))),
	riakc_obj:update_metadata(EdbObj, dict:store(?INDEX_KEY, NewIndex, Meta)).

-spec set_indexes(edb_obj(), [{index_type(), index_name(), index_value()}]) -> edb_obj().
set_indexes(EdbObj, Indexes) ->
	Meta = riakc_obj:get_update_metadata(EdbObj),
	Index = case dict:find(?INDEX_KEY, Meta) of
		error -> [];
		{ok, I} -> I
	end,
	UpdatedIndexes = lists:foldl(fun({T, N, V}, I) ->
									dict:store(index(T, N), value(V), I) end,
								 dict:from_list(Index), Indexes),
	NewIndexes = dict:to_list(UpdatedIndexes),
	riakc_obj:update_metadata(EdbObj, dict:store(?INDEX_KEY, NewIndexes, Meta)).

-spec get_index(edb_obj(), index_type(), index_name()) -> index_value() | no_return().
get_index(EdbObj, Type, Name) ->
	Meta = riakc_obj:get_metadata(EdbObj),
	Indexes = dict:fetch(?INDEX_KEY, Meta),
	IndexKey = binary_to_list(index(Type, Name)),
	Value = proplists:get_value(IndexKey, Indexes),
	case Type of
		int -> list_to_integer(Value);
		bin -> Value
	end.


index(int, Name) ->
	iolist_to_binary([Name, ?INDEX_SUFFIX_INT]);
index(bin, Name) ->
	iolist_to_binary([Name, ?INDEX_SUFFIX_BIN]).

value(V) when is_list(V) -> list_to_binary(V);
value(V) -> V.


