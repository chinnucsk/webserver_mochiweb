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
    {Options, AfterCriteria} = build_options(QueryString, {[],[]}),
    io:format("~p~n~p~n", [Options, AfterCriteria]),
    case AfterCriteria of
	[] ->
	    {ok, Res} = db:view_access(?DB_NAME, ?PROD_VIEW, Options),
	    Res;
	_ ->
	    {NOptions, AfterLimit} = 
		case proplists:get_value(limit, Options) of
		    undefined ->
			{Options, 1000};
		    Limit ->
			{proplists:delete(limit, Options), Limit}
		end,
	    io:format("~p~n~p~n", [NOptions, AfterLimit]),
	    {ok, PartialRes} = db:view_access(?DB_NAME, ?PROD_VIEW, NOptions),
	    filter_after_criteria(PartialRes, AfterCriteria, AfterLimit, [])
    end.

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
	    case Int > 0 andalso Int < 1000 of
		true ->
		    build_options(T, {[{limit, Int}|Options], AfterCriteria});
		false ->
		    throw(bad_request)
	    end
    catch
	error:badarg ->
	    throw(bad_request)
    end;
build_options([{"name", Value}|T], {Options, AfterCriteria}) ->
    build_options(T, {Options, [{name,Value}|AfterCriteria]});
build_options([{"amount_gt", Value}|T], {Options, AfterCriteria}) ->
    try list_to_integer(Value) of
	Int ->
	    build_options(T, {Options, [{amount_gt,Int}|AfterCriteria]})
    catch
	error:badarg ->
	    throw(bad_request)
    end;
build_options([{"price_gt", Value}|T], {Options, AfterCriteria}) ->
    try list_to_integer(Value) of
	Int ->
	    build_options(T, {Options, [{price_gt,Int}|AfterCriteria]})
    catch
	error:badarg ->
	    throw(bad_request)
    end;		
build_options([{"price_lt",Value}|T], {Options, AfterCriteria}) ->
    try list_to_integer(Value) of
	Int ->
	    build_options(T, {Options, [{price_lt,Int}|AfterCriteria]})
    catch
	error:badarg ->
	    throw(bad_request)
    end;
build_options([], Acc) ->
    Acc;
build_options(_, _) ->
    throw(bad_request).

filter_after_criteria(_Products, _Criteria, 0, Res) ->
    Res;
filter_after_criteria([Prod|Products], Criteria, Limit, Res) ->
    case meets_criteria(Prod, Criteria) of
	true ->
	    filter_after_criteria(Products, Criteria, Limit-1, [Prod|Res]);
	false ->
	    filter_after_criteria(Products, Criteria, Limit, Res)
    end;
filter_after_criteria([], _Criteria, _Limit, Res) ->
    Res.

meets_criteria(Prod, [{name, Value}|Criteria]) ->
    Name = proplists:get_value(name, Prod),
    case string:str(Name, Value) of
	0 ->
	    false;
	_ ->
	    meets_criteria(Prod, Criteria)
    end;
meets_criteria(Prod, [{amount_gt, Value}|Criteria]) ->
    Amount = proplists:get_value(amount, Prod),
    case Amount > Value of
	false ->
	    false;
	_ ->
	    meets_criteria(Prod, Criteria)
    end;
meets_criteria(Prod, [{price_gt, Value}|Criteria]) ->
    Price = proplists:get_value(price, Prod),
    case Price > Value of
	false ->
	    false;
	_ ->
	    meets_criteria(Prod, Criteria)
    end;
meets_criteria(Prod, [{price_lt, Value}|Criteria]) ->
    Price = proplists:get_value(price, Prod),
    case Price < Value of
	false ->
	    false;
	_ ->
	    meets_criteria(Prod, Criteria)
    end;
meets_criteria(_Prod, []) ->
    true.
