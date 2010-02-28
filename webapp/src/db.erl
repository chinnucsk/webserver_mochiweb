-module(db).

-export([start/0, stop/0]).
-export([db_create/1, db_delete/1, db_info/1, db_list/0]).
-export([doc_create/2, doc_create/3, doc_get/2, doc_get/3, doc_delete/2, 
	 doc_delete/3, doc_update/4]).
-export([view_create/3, view_create/4, view_delete/2, view_get/2, view_get/3,
	 view_get/4, view_access/2, view_access/3, view_access/4,
	 view_access_c/3]).

-define(KEYS_TO_REMOVE, ["_id", "_rev", "_deleted_conflicts"]).
-define(KEYS_TO_UNFRIENDLYAZE, [key, startkey, endkey, keys]).

-include("webapp.hrl").

start() ->
    ok = application:start(inets),
    ok = application:start(ecouch),
    init().

init() ->
    {ok, DbList} = db_list(),
    case lists:member(?DB_NAME, DbList) of
	true ->
	    ok;
	false ->
	    db_create(?DB_NAME)
    end,
    init_views(views()).

init_views([{Name, Map}|T]) ->
    case view_get(?DB_NAME, Name) of
	{error, not_found} ->
	    view_create(?DB_NAME, Name, Map);
	_ ->
	    ok
    end,
    init_views(T);
init_views([{Name, Map, Reduce}|T]) ->
    case view_get(?DB_NAME, Name) of
	{error, not_found} ->
	    view_create(?DB_NAME, Name, Map, Reduce);
	_ ->
	    ok
    end,
    init_views(T);
init_views([]) ->
    ok.

views() ->
    [{?PROD_VIEW, product_view()},
     {?USER_VIEW, user_view()}].

product_view() ->
    "function(doc) {"
	"if(doc.type == 'product') {emit(doc.id, doc); }}".

user_view() ->
    "function(doc) {"
	"if(doc.type == 'user') {emit(doc.username, doc); }}".

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

doc_delete(DbName, ViewName, ObjectId) ->
    case view_access(DbName, ViewName, [{key, ObjectId}], false) of
	{ok, []} ->
	    {error, bad_id};
	{ok, [Res]} ->
	    DocId = proplists:get_value('_id', Res),
	    Rev = proplists:get_value('_rev', Res),
	    do_call_bool(ecouch, doc_delete, [DbName, to_list(DocId), Rev])
    end.

doc_update(DbName, DocName, Content, false) ->
    case doc_get(DbName, DocName, false) of
	{ok, Data} ->
	    Rev = proplists:get_value('_rev', Data),
	    do_call_bool(ecouch, doc_update, 
			 [DbName, to_list(DocName), 
			  {obj, [{"_rev", Rev} | Content]}]);
	Err ->
	    Err
    end;
doc_update(DbName, DocName, Content, true) ->
    do_call_bool(ecouch, doc_update, 
		 [DbName, to_list(DocName), {obj, Content}]).
    


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

view_access_c(DbName, DesignName, Options) ->
    NOptions = user_unfriendlyaze_c(Options),
    do_call_get(ecouch, view_access, [DbName, DesignName, DesignName, NOptions],
		true).
    


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
user_friendlyaze(V) when is_list(V) -> 
    lists:foldr(
      fun(E,Acc) ->
	      case is_binary(E) of
		  true -> [btx(E)|Acc];
		  false -> [E|Acc]
	      end
      end,
      [],
      V);
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
		    case is_list(V) of
			true ->
			    user_unfriendlyaze(
			      T, [{K,lists:flatten(io_lib:format("~w",[V]))}|Res]);
			false ->
			    user_unfriendlyaze(T, [{K,V}|Res])
		    end
	    end;
	false ->
	    user_unfriendlyaze(T, [H|Res])
    end;
user_unfriendlyaze([], Res) ->
    Res.

user_unfriendlyaze_c(Data) ->
    user_unfriendlyaze_c(Data, []).
user_unfriendlyaze_c([{K,V} = H|T], Res) ->
    case lists:member(K, ?KEYS_TO_UNFRIENDLYAZE) of
	true ->
	    case K of
		keys ->
		    user_unfriendlyaze_c(T, [{K,transform_keys(V)}|Res]);
		_ ->
		    NV = lists:foldl(
			   fun(E, Acc) ->
				   case is_list(E) of
				       true ->
					   ["\"" ++ E ++ "\""|Acc];
				       false ->
					   [E|Acc]
				   end
			   end, [], V),
		    NV2 = "[" ++ string:join(NV, ",") ++ "]",
		    user_unfriendlyaze_c(T, [{K, NV2}|Res])
	    end;
	false ->
	    user_unfriendlyaze_c(T, [H|Res])
    end;
user_unfriendlyaze_c([], Res) ->
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
    V = case proplists:get_value("value", E) of
	{obj, Value} -> Value;
	Value -> [{"value", Value}]
    end,
    Aux = [{"key", proplists:get_value("key", E)} | V],
    user_friendlyaze_proplist(Aux, Remove).

transform_keys(V) ->
    lists:foldr(
      fun(E, Acc) ->
	      [list_to_binary(E) | Acc]
      end,
      [],
      V).

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
    
