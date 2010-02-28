-module(users_controller).
-export([dispatch/1, top/1, user/1, new/1]).

-include_lib("webapp.hrl").

dispatch({_Req, Path, _ResContentType, _Meth} = Args) ->
    F = case Path of
	    "" ->
		top;
	    ["new"] ->
		new;
	    [_] ->
		user;
	    _ ->
		erlang:error(bad_uri)
	end,
    apply(?MODULE, F, [Args]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% /users
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
top({Req, _Path, ResContentType, get}) ->
    QueryString = Req:parse_qs(),
    try
	Data = user:get_list(QueryString),
	Response = user_render:get_list(Data, ResContentType),
	{200, [{?CT, ResContentType}], Response}
    catch
	throw:bad_request ->
	    {400, [{?CT, "text/plain"}], "Bad request"}
    end;    
top({Req, _Path, ResContentType, post}) ->
    Body = Req:parse_post(),
    try 
	user:create(Body),
	Response = user_render:create(ResContentType),
	{201, [{?CT, ResContentType}], Response}
    catch
	throw:bad_request ->
	    {400, [{?CT, "text/plain"}], "Bad request"};
	throw:user_exists ->
	    {200, [{?CT, ResContentType}], 
	     user_render:create(ResContentType, error)}
    end;
top({_Req, _Path, _ResContentType, put}) ->
    {405,[{?CT, "text/plain"}], "Bad method"};
top({_Req, _Path, _ResContentType, delete}) ->
    {405,[{?CT, "text/plain"}], "Bad method"}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% /users/userId
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
user({_Req, [Path], ResContentType, get}) ->
    try
	Data = user:get(Path),
	Response = user_render:get(Data, ResContentType),
	{200, [{?CT, ResContentType}], Response}
    catch
	throw:bad_uri ->
	    {404, [{?CT, "text/plain"}], "Not found"}
    end;
user({_Req, _Path, _ResContentType, post}) ->
    {405,[{?CT, "text/plain"}], "Bad method"};
user({Req, [Path], ResContentType, put}) ->
    Body = Req:parse_post(),
    try
	user:update(Path, Body),
	Response = user_render:update(ResContentType),
	{200, [{?CT, ResContentType}], Response}
    catch
	throw:bad_uri ->
	    {404, [{?CT, "text/plain"}], "Not found"};
	throw:bad_request ->
	    {400, [{?CT, "text/plain"}], "Bad request"}
    end;
user({_Req, [Path], ResContentType, delete}) ->
    try
	user:delete(Path),
	Response = user_render:delete(ResContentType),
	{200, [{?CT, ResContentType}], Response}
    catch
	throw:bad_uri ->
	    {404, [{?CT, "text/plain"}], "Not found"}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% /users/new
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
new({_Req, [_Path], ResContentType, get}) ->
    try
	Response = user_render:new(ResContentType),
	{200, [{?CT, ResContentType}], Response}
    catch
	error:function_clause ->
	    {406, [{?CT, "text/plain"}], "Not acceptable"}
    end;
new({_Req, _Path, _ResContentType, post}) ->
    {405,[{?CT, "text/plain"}], "Bad method"};
new({_Req, [_Path], _ResContentType, put}) ->
    {405,[{?CT, "text/plain"}], "Bad method"};
new({_Req, [_Path], _ResContentType, delete}) ->
    {405,[{?CT, "text/plain"}], "Bad method"}.

