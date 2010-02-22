-module(crud).

-export([create/4, get/2, delete/2, update/4]).

create(UserParams, ExpectedFields, ViewName, Type) ->
    CheckedParams = utils:check_params(UserParams, ExpectedFields, [], create),
    NewObject = 
	case db:view_access(?DB_NAME, ViewName, 
			    [{limit, 1}, {descending, true}]) of
	    {ok, []} ->
		[{"type", Type}, {"id", 1} | CheckedParams];
	    {ok, [LastObject]} ->
		LastKey = proplists:get_value(key, LastObject),
		[{"type", Type}, {"id", LastKey + 1} | CheckedParams];
	    {error, _} ->
		throw(db_error)
	end,
    db:doc_create(?DB_NAME, NewObject).

get(Id, ViewName) ->
    case db:view_access(?DB_NAME, ViewName, [{key, Id}]) of
	{ok, []} ->
	    throw(bad_uri);
	{ok, [Res]} ->
	    Res
    end.

delete(Id, ViewName) ->
    case db:doc_delete(?DB_NAME, ViewName, Id) of
	{error, bad_id} ->
	    throw(bad_uri);
	ok ->
	    ok
    end.

update(Id, UserParams, ExpectedFields, ViewName) ->
    CheckedParams = utils:check_params(UserParams, ExpectedFields, [], update),
    case db:view_access(?DB_NAME, ViewName, [{key, Id}], false) of
	{ok, []} ->
	    throw(bad_uri);
	{ok, [Res]} ->
	    UpdatedObject = merge(CheckedParams, Res),
	    DocId = proplists:get_value('_id', UpdatedObject),
	    db:doc_update(?DB_NAME, DocId, UpdatedObject, true)		    
    end.

merge([{K,V}|T], Params) ->
    KAtom = list_to_atom(K),
    merge(T, [{KAtom,V} | proplists:delete(KAtom, Params)]);
merge([], Params) ->
    proplists:delete(key, Params).

