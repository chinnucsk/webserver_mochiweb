-module(products_controller).
-export([dispatch/1, top/1, product/1]).

-include_lib("webapp.hrl").

dispatch({_Req, Path, _ResContentType, _Meth} = Args) ->
    F = case Path of
	    "" ->
		top;
	    [_] ->
		product;
	    _ ->
		erlang:error(bad_uri)
	end,
    apply(?MODULE, F, [Args]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% /products
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
top({_Req, _Path, ResContentType, get}) ->
    {200, [{?CT, ResContentType}], ""};
top({Req, _Path, ResContentType, post}) ->
    Body = Req:parse_post(),
    try 
	product:create(Body),
	{200, [{?CT, ResContentType}], "ok"}
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
	    {404, [{?CT, "text/plain"}], "Not found"}
    end;
product({_Req, [Path], ResContentType, delete}) ->
    try
	product:delete(Path),
	{200, [{?CT, ResContentType}], "ok"}
    catch
	throw:bad_uri ->
	    {404, [{?CT, "text/plain"}], "Not found"}
    end.
