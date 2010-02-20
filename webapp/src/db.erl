-module(db).

-export([start/0, stop/0]).
-export([db_create/1, db_delete/1, db_info/1, db_list/0]).
-export([doc_create/2, doc_create/3, doc_get/2, doc_get/3, doc_delete/2, doc_update/3]).
-export([view_create/3, view_create/4, view_delete/2, view_get/2, view_get/3,
	 view_get/4, view_access/2, view_access/3, view_access/4]).

-define(KEYS_TO_REMOVE, ["_id", "_rev", "_deleted_conflicts"]).
-define(KEYS_TO_UNFRIENDLYAZE, [key, startkey, endkey, keys]).

start() ->
    ok = application:start(inets),
    ok = application:start(ecouch).

stop() ->
    ok = application:stop(inets),
    ok = application:stop(ecouch).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% database
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
db_create(Name) ->
    do_call_bool(ecouch, db_create, [Name]).

db_delete(Name) ->
    do_call_bool(ecouch, db_delete, [Name]).

db_info(Name) ->
    do_call_get(ecouch, db_info, [Name], false).

db_list() ->
    do_call_list(ecouch, db_list, []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% document
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

doc_create(DbName, Content) ->
    do_call_bool(ecouch, doc_create, [DbName, {obj, Content}]).

doc_create(DbName, DocName, Content) ->
    do_call_bool(ecouch, doc_create, [DbName, to_list(DocName), 
				      {obj, Content}]).

doc_get(DbName, DocName) ->
    doc_get(DbName, to_list(DocName), true).

doc_get(DbName, DocName, Remove) ->
    do_call_get(ecouch, doc_get, [DbName, to_list(DocName)], Remove).

doc_delete(DbName, DocName) ->
    case doc_get(DbName, DocName, false) of
	{ok, Data} ->
	    Rev = proplists:get_value('_rev', Data),
	    do_call_bool(ecouch, doc_delete, [DbName, to_list(DocName), Rev]);
	Err ->
	    Err
    end.

doc_update(DbName, DocName, Content) ->
    case doc_get(DbName, DocName, false) of
	{ok, Data} ->
	    Rev = proplists:get_value('_rev', Data),
	    do_call_bool(ecouch, doc_update, 
			 [DbName, to_list(DocName), 
			  {obj, [{"_rev", Rev} | Content]}]);
	Err ->
	    Err
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% View
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

view_create(DbName, DesignName, Map) ->
    do_call_bool(ecouch, view_create, 
		 [DbName, DesignName, 
		  {obj, [{DesignName, {obj, [{map, list_to_binary(Map)}]}}]}]).

view_create(DbName, DesignName, Map, Reduce) ->
    do_call_bool(ecouch, view_create, 
		 [DbName, DesignName, 
		  {obj, [{DesignName, {obj, [{map, list_to_binary(Map)}, 
					     {reduce, list_to_binary(Reduce)}]
				      }}]}]).

view_get(DbName, DesignName) ->
    view_get(DbName, DesignName, [], true).

view_get(DbName, DesignName, Options) ->
    view_get(DbName, DesignName, Options, true).

view_get(DbName, DesignName, Options, Remove) ->
    do_call_get(ecouch, view_get, [DbName, DesignName, Options], Remove).

view_delete(DbName, DesignName) ->
    case view_get(DbName, DesignName, [], false) of
	{ok, Data} ->
	    Rev = proplists:get_value('_rev', Data),
	    do_call_bool(ecouch, view_delete, [DbName, DesignName, Rev]);
	Err ->
	    Err
    end.

view_access(DbName, DesignName) ->
    view_access(DbName, DesignName, [], true).

view_access(DbName, DesignName, Options) ->
    view_access(DbName, DesignName, Options, true).

view_access(DbName, DesignName, Options, Remove) ->
    NOptions = user_unfriendlyaze(Options),
    do_call_get(ecouch, view_access, [DbName, DesignName, DesignName, NOptions],
	       Remove).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Handlers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
do_call_bool(M, F, A) ->
    case apply(M, F, A) of
	{ok, {obj, [{Res,Reason}|_]}} ->
	    case Res of
		"ok" ->
		    ok;
		"error" ->
		    {error, bta(Reason)}
	    end;
	Err ->
	    Err
    end.

do_call_get(M, F, A, Remove) ->
    case apply(M, F, A) of
	{ok, {obj, [{Res, Reason}|_] = L}} ->
	    case Res of
		"error" ->
		    {error, bta(Reason)};
		"total_rows" ->
		    {ok, user_friendlyaze_query_response(
			   proplists:get_value("rows", L), Remove)};
		_ ->
		    {ok, user_friendlyaze_proplist(L, Remove)}
		end;
	Err ->
	    Err
    end.

do_call_list(M, F, A) ->
    case apply(M, F, A) of
	{ok, L} ->
	    {ok, user_friendlyaze_list(L)};
	Err ->
	    Err
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Utilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
user_friendlyaze_list(L) ->
    user_friendlyaze_list(L, []).
user_friendlyaze_list([H|T], Res) ->
    user_friendlyaze_list(T, [bta(H) | Res]);
user_friendlyaze_list([], Res) ->
    Res.

user_friendlyaze_proplist(L, Remove) ->
    NL = case Remove of
	     true ->
		 lists:foldl(
		   fun(Key, PList) ->
			   case proplists:get_value(Key, PList) of
			       undefined ->
				   PList;
			       _ ->
				   proplists:delete(Key, PList)
			   end
		   end,
		   L,
		   ?KEYS_TO_REMOVE);
	     false ->
		 L
	 end,
    user_friendlyaze_proplist2(NL, []).
user_friendlyaze_proplist2([{K,V}|T], Res) ->
    user_friendlyaze_proplist2(T,[{list_to_atom(K), user_friendlyaze(V)}|Res]);
user_friendlyaze_proplist2([], Res) ->
    Res.    

user_friendlyaze(V) when is_binary(V) -> btx(V);
user_friendlyaze(V) -> V.

user_friendlyaze_query_response(ResponseData, Remove) ->
    user_friendlyaze_query_response(ResponseData, Remove, []).
user_friendlyaze_query_response([H|T], Remove, Res) ->
    user_friendlyaze_query_response(T, Remove, [extract(H, Remove) | Res]);
user_friendlyaze_query_response([], _Remove, Res) ->
    Res.

user_unfriendlyaze(Data) ->
    user_unfriendlyaze(Data, []).
user_unfriendlyaze([{K,V} = H|T], Res) ->
    case lists:member(K, ?KEYS_TO_UNFRIENDLYAZE) of
	true ->
	    case K of
		keys ->
		    user_unfriendlyaze(T, [{K,transform_keys(V)}|Res]);
		_ ->
		    user_unfriendlyaze(T, [{K,transform(V)}|Res])
	    end;
	false ->
	    user_unfriendlyaze(T, [H|Res])
    end;
user_unfriendlyaze([], Res) ->
    Res.

btx(B) ->
    L = binary_to_list(B),
    try
	list_to_integer(L)
    catch
	_:_ ->
	    list_to_atom(L)
    end.
bta(B) ->
    list_to_atom(binary_to_list(B)).

extract({obj, E}, Remove) ->
    {obj, Value} = proplists:get_value("value", E),
    Aux = [{"key", proplists:get_value("key", E)} | Value],
    user_friendlyaze_proplist(Aux, Remove).

transform_keys(V) ->
    lists:foldr(
      fun(E, Acc) ->
	      [list_to_binary(to_list(E)) | Acc]
      end,
      [],
      V).
transform(V) ->
    "\"" ++ to_list(V) ++ "\"".
to_list(V) when is_integer(V) ->
    integer_to_list(V);
to_list(V) when is_float(V) ->
    float_to_list(V);
to_list(V) when is_atom(V) ->
    atom_to_list(V);
to_list(V) when is_list(V) ->
    V;
to_list(V) when is_tuple(V) ->
    "{" ++ tuple_to_list(V) ++ "}";
to_list(V) ->
    erlang:error({unsupported_type, V}).
    
