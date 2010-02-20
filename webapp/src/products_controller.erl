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
    Body = products:get_products(ResContentType),
    {200, [{"Content-type", ResContentType}], Body};
top({_Req, _Path, _ResContentType, post}) ->
    ok;
top({_Req, _Path, _ResContentType, put}) ->
    erlang:error(bad_method);
top({_Req, _Path, _ResContentType, delete}) ->
    erlang:error(bad_method).

product({Req, _Path, _ResContentType, _Method}) ->
    UserPass = mochiweb_headers:get_value('Authorization', Req:get(headers)),
    case UserPass of
	undefined ->
	    {401, [{"Content-type", "text/html"},
		   {"WWW-Authenticate", "Basic"}], ""};
	_ ->
	    {200, [{"Content-type", "text/html"}], "Logged in!!"}
    end.
