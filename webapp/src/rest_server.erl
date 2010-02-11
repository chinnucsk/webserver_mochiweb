%%%-------------------------------------------------------------------
%%% File    : rest_server.erl
%%% Author  : Jordi Chacón
%%% Description : 
%%%
%%% Created :  Thu Feb 11 23:33:03 2010
%%%-------------------------------------------------------------------

-module(rest_server).
-behaviour(gen_server).

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
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% INTERNAL CALLS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dispatch_request(Req) ->
    Path = Req:get(path),
    Action = clean_path(Path),
    handle(Action, Req).

% Handle the requests
handle("/favicon.ico", Req) -> Req:respond({200, [{"Content-Type", "text/html"}], "burru"});
handle(Path, Req) ->
    BaseController = lists:concat([top_level_request(clean_path(Path)), "_controller"]),
    CAtom = list_to_atom(BaseController),
    ControllerPath = parse_controller_path(clean_path(Path)),
    case CAtom of
	home ->
	    %IndexContents = ?ERROR_HTML("Uh oh"),
	    Req:ok({"text/html", []});
	ControllerAtom -> 
	    Meth = clean_method(Req:get(method)),
	    case Meth of
		get -> run_controller(Req, ControllerAtom, Meth, [ControllerPath]);
		_ -> run_controller(Req, ControllerAtom, Meth, [ControllerPath, decode_data_from_request(Req)])
	    end
    end.

% Call the controller action here
run_controller(Req, ControllerAtom, Meth, Args) ->
    case (catch erlang:apply(ControllerAtom, Meth, Args)) of
	{'EXIT', {undef, _}} = E ->
	    %?LOG(error, "(~p:~p) Error in rest server: ~p~n", [?MODULE, ?LINE,E]),
	    Req:ok({"text/html", "Unimplemented controller. There is nothing to see here, go back from where you came"});
	{'EXIT', E} -> 
	    %?LOG(error, "(~p:~p) Error in rest server: ~p~n", [?MODULE, ?LINE, E]),
	    Req:not_found();
	Body -> 
	    %Req:ok({"text/json", ?JSONIFY(Body)})
	    Req:ok({"text/json", Body})
    end.

% Find the method used as a request. 
% This turns 'GET' into get
clean_method(M) ->
    case M of
	% This is a hack... FOR NOW
	'OPTIONS' -> get;
	_ -> erlang:list_to_atom(string:to_lower(erlang:atom_to_list(M)))
    end.

% parse the controller path
parse_controller_path(CleanPath) ->
    case string:tokens(CleanPath, "/") of
	[] -> [];
	[_RootPath|Rest] -> Rest
    end.

% Get a clean path
% strips off the query string
clean_path(Path) ->
    case string:str(Path, "?") of
	0 -> Path;
	N -> string:substr(Path, 1, N - 1)
    end.

% Query about the top level request path is
top_level_request(Path) ->
    case string:tokens(Path, "/") of
	[CleanPath|_Others] -> CleanPath;
	[] -> "home"
    end.

% Convert each of the binary data proplists into a valid proplist
% from {<<name>>, <<value>>} to {name, value}
convert_to_struct(RawData) ->
    lists:map(fun({BinKey, BinVal}) ->
		      case BinVal of
			  {struct, Arr} -> 
			      Key = misc_utils:to_atom(BinKey),
			      {Key, convert_to_struct(Arr)};
			  _ ->
			      Key = misc_utils:to_atom(BinKey),
			      Val = misc_utils:to_list(BinVal),
			      {Key, Val}
		      end
	      end, RawData).

% Get the data off the request
decode_data_from_request(Req) ->
    RecvBody = Req:recv_body(),
    Data = case RecvBody of
	       undefined -> erlang:list_to_binary("{}");
	       <<>> -> erlang:list_to_binary("{}");
	       Bin -> Bin
	   end,
    {struct, Struct} = mochijson2:decode(Data),
    convert_to_struct(Struct).
