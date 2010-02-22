-module(product).
-export([create/1, get/1, delete/1, update/2]).

-include("webapp.hrl").

-define(FIELDS, [{"name",string}, {"price",int}, 
		 {"amount",int}, {"description",string}]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% CREATE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create(Params) ->
    CheckedParams = utils:check_params(Params, ?FIELDS, [], create),
    NewProduct = 
	case db:view_access(?DB_NAME, ?PROD_VIEW, 
			    [{limit, 1}, {descending, true}]) of
	    {ok, []} ->
		[{"type", product}, {"id", 1} | CheckedParams];
	    {ok, [LastProduct]} ->
		LastKey = proplists:get_value(key, LastProduct),
		[{"type", product}, {"id", LastKey + 1} | CheckedParams];
	    {error, _} ->
		throw(db_error)
	end,
    db:doc_create(?DB_NAME, NewProduct).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% GET
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get(Id) ->
    try list_to_integer(Id) of
	Int ->
	    case db:view_access(?DB_NAME, ?PROD_VIEW, [{key, Int}]) of
		{ok, []} ->
		    throw(bad_uri);
		{ok, [Res]} ->
		    Res
	    end
    catch
	error:badarg ->
	    throw(bad_uri)
    end.
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% DELETE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
delete(Id) ->
    try list_to_integer(Id) of
	Int ->
	    case db:doc_delete(?DB_NAME, ?PROD_VIEW, Int) of
		{error, bad_id} ->
		    throw(bad_uri);
		ok ->
		    ok
	    end
    catch
	error:badarg ->
	    throw(bad_uri)
    end.
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% UPDATE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
update(Id, Params) ->
    CheckedParams = utils:check_params(Params, ?FIELDS, [], update),
    try list_to_integer(Id) of
	Int ->
	    case db:view_access(?DB_NAME, ?PROD_VIEW, [{key, Int}], false) of
		{ok, []} ->
		    throw(bad_uri);
		{ok, [Res]} ->
		    UpdatedProduct = merge(CheckedParams, Res),
		    DocId = proplists:get_value('_id', UpdatedProduct),
		    db:doc_update(?DB_NAME, DocId, UpdatedProduct, true)		    
	    end
    catch
	error:badarg ->
	    throw(bad_uri)
    end.

merge([{K,V}|T], Params) ->
    KAtom = list_to_atom(K),
    merge(T, [{KAtom,V} | proplists:delete(KAtom, Params)]);
merge([], Params) ->
    proplists:delete(key, Params).
