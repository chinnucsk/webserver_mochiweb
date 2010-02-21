%%%-------------------------------------------------------------------
%%% File    : web_server.erl
%%% Author  : Jordi Chacon
%%% Description : 
%%%
%%% Created :  Thu Feb 11 23:33:03 2010
%%%-------------------------------------------------------------------

-module(web_server).
-behaviour(gen_server).
-include_lib("../include/webapp.hrl").

%% API
-export([start/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, 
	 handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% GEN_SERVER CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([Port]) ->
    db:start(),
    mochiweb_http:start([{port, Port}, {loop, fun dispatch_request/1}]).

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    mochiweb_http:stop(),
    db:stop(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% INTERNAL CALLS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dispatch_request(Req) ->
    ResContentType = parse_accept(Req:get(headers)),
    Path = Req:get(path),
    case Path of
	"/favicon.ico" ->
	    Req:respond({200, [{"Content-Type", "text/html"}], ""});
	_ ->
	    {Controller, ControllerPath} = parse_path(Path),
	    Meth = clean_method(Req:get(method)),
	    run_controller(Req, Controller, 
			   {Req, ControllerPath, ResContentType, Meth})
    end.

run_controller(Req, Controller, Args) ->
    case (catch erlang:apply(Controller, dispatch, [Args])) of
	{'EXIT', {bad_uri,_}} ->
	    Req:respond({404,[{"Content-type", "text/plain"}], "Not found"});
	{'EXIT', {bad_method,_}} ->
	    Req:respond({405,[{"Content-type", "text/plain"}], "Bad method"});
	Res -> 
	    Req:respond(Res)
    end.

% Parses the path and returns {top_controller, rest}
parse_path(Path) ->
    CleanedPath = clean_path(Path),
    case string:tokens(CleanedPath, "/") of
	[] ->
	    {index, no_path};
	[Top|Rest] ->
	    {list_to_atom(Top ++ "_controller"), Rest}
    end.

clean_path(Path) ->
    case string:str(Path, "?") of
	0 -> Path;
	N -> string:substr(Path, 1, N - 1)
    end.

clean_method(M) ->
    case M of
	'OPTIONS' -> get;
	_ -> erlang:list_to_atom(string:to_lower(erlang:atom_to_list(M)))
    end.

parse_accept(Headers) ->
    Accept = mochiweb_headers:get_value('Accept', Headers),
    ClientAcceptedEncs = mochiweb_util:parse_qvalues(Accept),
    Encs = mochiweb_util:pick_accepted_encodings(ClientAcceptedEncs,
						 ?SUPPORTED_MEDIA,
						 "text/plain"),
    case lists:member("text/html", Encs) of
	true ->
	    "text/html";
	false ->
	    hd(Encs)
    end.
