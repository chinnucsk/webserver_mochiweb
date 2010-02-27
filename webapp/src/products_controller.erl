-module(products_controller).
-export([dispatch/1, top/1, product/1, new/1, search/1]).

-include_lib("webapp.hrl").

dispatch({_Req, Path, _ResContentType, _Meth} = Args) ->
    F = case Path of
	    "" ->
		top;
	    ["new"] ->
		new;
	    ["search"] ->
		search;
	    [_] ->
		product;
	    _ ->
		erlang:error(bad_uri)
	end,
    apply(?MODULE, F, [Args]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% /products
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
top({Req, _Path, ResContentType, get}) ->
    QueryString = Req:parse_qs(),
    try
	Data = product:get_list(QueryString),
	Response = product_render:get_list(Data, ResContentType),
	{200, [{?CT, ResContentType}], Response}
    catch
	throw:bad_request ->
	    {400, [{?CT, "text/plain"}], "Bad request"}
    end;    
top({Req, _Path, ResContentType, post}) ->
    Body = Req:parse_post(),
    try 
	product:create(Body),
	Response = product_render:create(ResContentType),
	{201, [{?CT, ResContentType}], Response}
    catch
	throw:bad_request ->
	    {400, [{?CT, "text/plain"}], "Bad request"}
    end;
top({_Req, _Path, _ResContentType, put}) ->
    {405,[{?CT, "text/plain"}], "Bad method"};
top({_Req, _Path, _ResContentType, delete}) ->
    {405,[{?CT, "text/plain"}], "Bad method"}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% /products/productId
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
product({_Req, [Path], ResContentType, get}) ->
    try
	Data = product:get(Path),
	Response = product_render:get(Data, ResContentType),
	{200, [{?CT, ResContentType}], Response}
    catch
	throw:bad_uri ->
	    {404, [{?CT, "text/plain"}], "Not found"}
    end;
product({_Req, _Path, _ResContentType, post}) ->
    {405,[{?CT, "text/plain"}], "Bad method"};
product({Req, [Path], ResContentType, put}) ->
    Body = Req:parse_post(),
    try
	product:update(Path, Body),
	{200, [{?CT, ResContentType}], "ok"}
    catch
	throw:bad_uri ->
	    {404, [{?CT, "text/plain"}], "Not found"};
	throw:bad_request ->
	    {400, [{?CT, "text/plain"}], "Bad request"}
    end;
product({_Req, [Path], ResContentType, delete}) ->
    try
	product:delete(Path),
	{200, [{?CT, ResContentType}], "ok"}
    catch
	throw:bad_uri ->
	    {404, [{?CT, "text/plain"}], "Not found"}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% /products/new
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
new({_Req, [_Path], ResContentType, get}) ->
    Response = product_render:new(ResContentType),
    {200, [{?CT, ResContentType}], Response};
new({_Req, _Path, _ResContentType, post}) ->
    {405,[{?CT, "text/plain"}], "Bad method"};
new({_Req, [_Path], _ResContentType, put}) ->
    {405,[{?CT, "text/plain"}], "Bad method"};
new({_Req, [_Path], _ResContentType, delete}) ->
    {405,[{?CT, "text/plain"}], "Bad method"}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% /products/search
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
search({_Req, [_Path], ResContentType, get}) ->
    io:format("soma"),
    Response = product_render:search(ResContentType),
    {200, [{?CT, ResContentType}], Response};
search({_Req, _Path, _ResContentType, post}) ->
    {405,[{?CT, "text/plain"}], "Bad method"};
search({_Req, [_Path], _ResContentType, put}) ->
    {405,[{?CT, "text/plain"}], "Bad method"};
search({_Req, [_Path], _ResContentType, delete}) ->
    {405,[{?CT, "text/plain"}], "Bad method"}.

