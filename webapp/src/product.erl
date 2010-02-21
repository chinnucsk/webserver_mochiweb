-module(product).
-export([create/1, get_product/1]).

-include("webapp.hrl").

-define(FIELDS, [{"name",string}, {"price",int}, 
		 {"amount",int}, {"description",string}]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% CREATE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create(Params) ->
    CheckedParams = utils:check_params(Params, ?FIELDS, []),
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

%% Handler for GET requests on URI /products
get_product(Id) ->
    try list_to_integer(Id) of
	Int ->
	    case db:view_access(?DB_NAME, ?PROD_VIEW, [{key, Int}]) of
		{ok, []} ->
		    throw(bad_uri);
		{ok, Res} ->
		    Res
	    end
    catch
	error:badarg ->
	    throw(bad_uri)
    end.
    







