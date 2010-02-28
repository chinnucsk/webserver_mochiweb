-module(user_render).

-export([get/2, get_list/2, create/1, create/2, new/1, delete/1]).

-include_lib("webapp.hrl").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% /users/userId create
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create("application/xml") ->
    "ok";
create("text/html") ->
    "<html>ok</html>";
create("application/json") ->
    mochijson2:encode({struct,[{ok,true}]}).

create("application/xml", error) ->
    "error";
create("text/html", error) ->
    "<html>error</html>";
create("application/json", error) ->
    mochijson2:encode({struct,[{ok,false}]}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% /users/userId delete
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
delete("application/xml") ->
    "ok";
delete("text/html") ->
    "<html>ok</html>";
delete("application/json") ->
    mochijson2:encode({struct,[{ok,true}]}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% /users/userId get
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get(Data, "application/xml") ->
    {ok, Compiled} = sgte:compile_file("priv/template.xml"),
    {ok, Compiled2} = sgte:compile("\n<$field$>$value$</$field$>"),
    UserName = proplists:get_value(username, Data),
    Email = proplists:get_value(email, Data),
    sgte:render_str(Compiled, [{type, "user"},
			       {attr, Compiled2}, 
			       {attrs, [[{field,username},{value,UserName}],
					[{field,email},{value,Email}]]}]);
get(Data, "text/html") ->
    {ok, Compiled} = sgte:compile_file("priv/template.html"),
    {ok, CompiledElem} = sgte:compile_file("priv/template_resource_field.html"),
    {ok, CompiledBody} = sgte:compile_file("priv/template_resource.html"),
    {ok, CompiledMenu} = sgte:compile_file("priv/template_menu.html"),
    {List,Data} = lists:foldr(
	     fun(E, {Acc, PL}) ->
		     {[[{field, E}, {value, proplists:get_value(E,PL)}] | Acc], 
		      PL}
	     end,
	     {[],Data},
	     [username, email]),
    Id = proplists:get_value(username, Data),
    RenderedBody = sgte:render(CompiledBody, [{group, "users"}, {edit,0},
					      {element,CompiledElem},
					      {elems, List},{id,Id}]),
    sgte:render_str(Compiled,
		    [{title, "user"}, {body, RenderedBody}, 
		     {menu, CompiledMenu},
		     {active1, ""}, {active2, ""},
		     {active3, "class=\"current_page_item\""}, {active4, ""},  
		     {action, "/users"}, {id, "id"},
		     {elems, [[{link, "/users/new"}, {name,"New"}]]}]);
get(Data, "application/json") ->
    mochijson2:encode({struct, Data}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% /users
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_list(Data, "application/xml") ->
    {ok, Compiled} = sgte:compile_file("priv/template.xml"),
    {ok, Compiled2} = sgte:compile_file("\n<user><link>$link$</link>"
				   "<username>$name$</username></user>"),
    List = lists:foldl(
	     fun(Product, Acc) ->
		     Id = proplists:get_value(key, Product),
		     Link = ?HOSTNAME ++ "users/" ++ Id,
		     [[{link,Link}, {username,Id}] | Acc]
	     end,
	     [],
	     Data),
    sgte:render_str(Compiled, [{type, "list"}, {attr, Compiled2},
			       {attrs, List}]);
get_list(Data, "text/html") ->
    {ok, CompiledTemplate} = sgte:compile_file("priv/template.html"),
    {ok, CompiledElem} = sgte:compile_file("priv/template_list_element.html"),
    {ok, CompiledBody} = sgte:compile_file("priv/template_list.html"),
    {ok, CompiledMenu} = sgte:compile_file("priv/template_menu.html"),
    List = lists:foldl(
	     fun(Product, Acc) ->
		     Id = proplists:get_value(key, Product),
		     Link = ?HOSTNAME ++ "users/" ++ Id,
		     [[{link,Link}, {name,Id}] | Acc]
	     end,
	     [],
	     Data),
    RenderedBody = sgte:render(CompiledBody, [{element, CompiledElem},
					      {elems, List}]),    
    sgte:render_str(CompiledTemplate, 
		    [{title, "users"}, {body, RenderedBody},  
		     {menu, CompiledMenu},
		     {active1, ""}, {active2, ""},
		     {active3, "class=\"current_page_item\""}, {active4, ""},  
		     {action, "/users"}, {id, "id"},
		     {elems, [[{link, "/users/new"}, {name,"New"}]]}]);
get_list(Data, "application/json") ->
    NewData = 
	lists:foldl(
	  fun(PropList, Acc) ->
		  Id = proplists:get_value(key, PropList),
		  Link = ?HOSTNAME ++ "products/" ++ Id,
		  [{struct, [{link,Link},{username,Id}]} | Acc]
	  end,
	  [],
	  Data),
    mochijson2:encode(NewData).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% /users/new
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
new("text/html") ->
    {ok, CompiledTemplate} = sgte:compile_file("priv/template.html"),
    {ok, CompiledBody} = sgte:compile_file("priv/user_new.html"),
    {ok, CompiledMenu} = sgte:compile_file("priv/template_menu.html"),
    RenderedBody = sgte:render(CompiledBody, []),    
    sgte:render_str(CompiledTemplate, 
		    [{title, "new user"}, {body, RenderedBody},
		     {menu, CompiledMenu},
		     {active1, ""}, {active2, ""},
		     {active3, "class=\"current_page_item\""}, {active4, ""},  
		     {action, "/users"}, {id, "id"},
		     {elems, []}]).
