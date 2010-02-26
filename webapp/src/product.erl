-module(product).
-export([create/1, get/1, delete/1, update/2, get_list/1]).

-include("webapp.hrl").

-define(FIELDS, [{"name",string}, {"price",int}, {"amount",int}, 
		 {"description",string}, {"tag",string}]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% CREATE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create(Params) ->
    crud:create(Params, ?FIELDS, ?PROD_VIEW, product).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% GET
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get(Id) ->
    try list_to_integer(Id) of
	Int ->
	    crud:get(Int, ?PROD_VIEW)
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
	    crud:delete(Int, ?PROD_VIEW)
    catch
	error:badarg ->
	    throw(bad_uri)
    end.
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% UPDATE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
update(Id, Params) ->
    try list_to_integer(Id) of
	Int ->
	    crud:update(Int, Params, ?FIELDS, ?PROD_VIEW)
    catch
	error:badarg ->
	    throw(bad_uri)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% GET LIST
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_list([]) ->
    {ok, Res} = 
	db:view_access(?DB_NAME, ?PROD_VIEW, [{limit,10},{descending,true}]),
    Res;
get_list(QueryString) ->
    {Options,_} = build_options(QueryString, {[],[]}),
    {ok, Res} = db:view_access(?DB_NAME, ?PROD_VIEW, Options),
    Res.

build_options([{"id_gt", Value}|T], {Options, AfterCriteria}) ->
    try list_to_integer(Value) of
	Int ->
	    build_options(T, {[{startkey, Int+1}|Options], AfterCriteria})
    catch
	error:badarg ->
	    throw(bad_request)
    end;
build_options([{"id_lt", Value}|T], {Options, AfterCriteria}) ->
    try list_to_integer(Value) of
	Int ->
	    build_options(T, {[{endkey, Int-1}|Options], AfterCriteria})
    catch
	error:badarg ->
	    throw(bad_request)
    end;
build_options([{"limit", Value}|T], {Options, AfterCriteria}) ->
    try list_to_integer(Value) of
	Int ->
	    build_options(T, {[{limit, Int}|Options], AfterCriteria})
    catch
	error:badarg ->
	    throw(bad_request)
    end;
build_options([{"name", _Value}=Pair|T], {Options, AfterCriteria}) ->
    build_options(T, {Options, [Pair|AfterCriteria]});
build_options([{"amount_gt", _Value}=Pair|T], {Options, AfterCriteria}) ->
    build_options(T, {Options, [Pair|AfterCriteria]});
build_options([{"price_gt", _Value}=Pair|T], {Options, AfterCriteria}) ->
    build_options(T, {Options, [Pair|AfterCriteria]});
build_options([{"price_lt", _Value}=Pair|T], {Options, AfterCriteria}) ->
    build_options(T, {Options, [Pair|AfterCriteria]});
build_options([], Acc) ->
    Acc;
build_options(_, _) ->
    throw(bad_request).
