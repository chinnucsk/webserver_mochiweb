-module(utils).

-export([check_params/3]).

check_params(Params, [{FieldName, Type}|T], Res) ->
    case proplists:lookup(FieldName, Params) of
	none ->
	    throw(bad_request);
	{FieldName, Value} ->
	    NewParams = proplists:delete(FieldName, Params),
	    NewRes = [{FieldName, parse_param(Value, Type)}|Res],
	    check_params(NewParams, T, NewRes)
    end;
check_params([], [_|_], _) ->
    throw(bad_request);
check_params([_|_], [], _) ->
    throw(bad_request);
check_params([], [], Res) ->
    Res.

parse_param(Value, string) when is_list(Value) ->
    try list_to_integer(Value) of
	_ -> throw(bad_request)
    catch
	error:badarg -> Value
    end;
parse_param(Value, int) when is_list(Value) ->
    try 
	Int = list_to_integer(Value),
	Int
    catch
	error:badarg -> throw(bad_request)
    end;
parse_param(_, _) ->
    throw(bad_request).
    
