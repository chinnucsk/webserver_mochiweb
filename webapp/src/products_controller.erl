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
    {200, [{"Content-type", ResContentType}], ""};
top({Req, _Path, ResContentType, post}) ->
    Body = Req:parse_post(),
    try 
	product:create(Body),
	{200, [{"Content-type", ResContentType}], "ok"}
    catch
	throw:bad_request ->
	    {400, [{"Content-type", "text/plain"}], "Bad request"}
    end;
top({_Req, _Path, _ResContentType, put}) ->
    erlang:error(bad_method);
top({_Req, _Path, _ResContentType, delete}) ->
    erlang:error(bad_method).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% /products/productId
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
product({_Req, [Path], ResContentType, get}) ->
    try
	Res = product:get_product(Path),
	io:format("~p~n", [Res]),
	{200, [{"Content-type", ResContentType}], "ok"}
    catch
	throw:bad_uri ->
	    {404, [{"Content-type", "text/plain"}], "Not found"}
    end;
product({_Req, _Path, _ResContentType, post}) ->
    ok;
product({_Req, _Path, _ResContentType, put}) ->
    ok;
product({_Req, _Path, _ResContentType, delete}) ->
    ok.
