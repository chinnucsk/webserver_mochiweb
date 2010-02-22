-module(product).
-export([create/1, get/1, delete/1, update/2, get_list/0]).

-include("webapp.hrl").

-define(FIELDS, [{"name",string}, {"price",int}, 
		 {"amount",int}, {"description",string}]).


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
get_list() ->
    db:view_access(?DB_NAME, ?PROD_VIEW, [{limit,10},{descending,true}]).
