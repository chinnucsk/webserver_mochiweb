-module(product_render).

-export([get/2, get_list/2, create/1, new/1, search/1, edit/2, delete/1,
	 update/1]).

-include_lib("webapp.hrl").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% /products/productId
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create("application/xml") ->
    "ok";
create("text/html") ->
    "<html>ok</html>";
create("application/json") ->
    mochijson2:encode({struct,[{ok,true}]}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% /products/productId delete
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
delete("application/xml") ->
    "ok";
delete("text/html") ->
    "<html>ok</html>";
delete("application/json") ->
    mochijson2:encode({struct,[{ok,true}]}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% /products/productId put
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
update("application/xml") ->
    "ok";
update("text/html") ->
    "<html>ok</html>";
update("application/json") ->
    mochijson2:encode({struct,[{ok,true}]}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% /products/productId
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get(Data, "application/xml") ->
    {ok, Compiled} = sgte:compile_file("priv/template.xml"),
    {ok, Compiled2} = sgte:compile("\n<$field$>$value$</$field$>"),
    Id = proplists:get_value(key, Data),
    Name = proplists:get_value(name, Data),
    Price = proplists:get_value(price, Data),
    Amount = proplists:get_value(amount, Data),
    Description = proplists:get_value(description, Data),
    sgte:render_str(Compiled, [{type, "product"},
			       {attr, Compiled2}, 
			       {attrs, [[{field,id},{value,Id}],
					[{field,name},{value,Name}],
					[{field,price},{value,Price}],
					[{field,amount},{value,Amount}],
					[{field,description},
					 {value,Description}]]}]);
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
	     [name, tag, price, amount, description]),
    Id = proplists:get_value(id, Data),
    RenderedBody = sgte:render(CompiledBody, [{group, "products"},
					      {edit, "true"},
					      {element,CompiledElem},
					      {elems, List},{id,Id}]),
    sgte:render_str(Compiled, 
		    [{title, "product"}, {body, RenderedBody}, 
		     {menu, CompiledMenu},
		     {active1, ""}, {active2, "class=\"current_page_item\""},
		     {active3, ""}, {active4, ""},  
		     {action, "/products"},
		     {elems, [[{link, "/products/new"}, {name,"New"}],
			      [{link, "/products/search"}, {name,"Search"}]]}]);
get(Data, "application/json") ->
    mochijson2:encode({struct, Data}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% /products
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_list(Data, "application/xml") ->
    {ok, Compiled} = sgte:compile_file("priv/template.xml"),
    {ok, Compiled2} = sgte:compile_file("\n<product><link>$link$</link>"
				   "<name>$name$</name></product>"),
    List = lists:foldl(
	     fun(Product, Acc) ->
		     Id = proplists:get_value(key, Product),
		     Name = proplists:get_value(name, Product),
		     Link = ?HOSTNAME ++ "products/" ++ integer_to_list(Id),
		     [[{link,Link}, {name,Name}] | Acc]
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
		     Name = proplists:get_value(name, Product),
		     Link = ?HOSTNAME ++ "products/" ++ integer_to_list(Id),
		     [[{link,Link}, {name,Name}] | Acc]
	     end,
	     [],
	     Data),
    RenderedBody = sgte:render(CompiledBody, [{element, CompiledElem},
					      {elems, List}]),    
    sgte:render_str(CompiledTemplate, 
		    [{title, "products"}, {body, RenderedBody},  
		     {menu, CompiledMenu},
		     {active1, ""}, {active2, "class=\"current_page_item\""},
		     {active3, ""}, {active4, ""},  
		     {action, "/products"},
		     {elems, [[{link, "/products/new"}, {name,"New"}],
			      [{link, "/products/search"}, {name,"Search"}]]}]);
get_list(Data, "application/json") ->
    NewData = 
	lists:foldl(
	  fun(PropList, Acc) ->
		  Id = proplists:get_value(key, PropList),
		  Name = proplists:get_value(name, PropList),
		  Link = ?HOSTNAME ++ "products/" ++ integer_to_list(Id),
		  [{struct, [{link,Link},{name,Name}]} | Acc]
	  end,
	  [],
	  Data),
    mochijson2:encode(NewData).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% /products/new
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
new("text/html") ->
    {ok, CompiledTemplate} = sgte:compile_file("priv/template.html"),
    {ok, CompiledBody} = sgte:compile_file("priv/product_new.html"),
    {ok, CompiledMenu} = sgte:compile_file("priv/template_menu.html"),
    RenderedBody = sgte:render(CompiledBody, []),    
    sgte:render_str(CompiledTemplate, 
		    [{title, "new product"}, {body, RenderedBody},
		     {menu, CompiledMenu},
		     {active1, ""}, {active2, "class=\"current_page_item\""},
		     {active3, ""}, {active4, ""},  
		     {action, "/products"},
		     {elems, [[{link, "/products/search"}, {name,"Search"}]]}]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% /products/search
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
search("text/html") ->
    {ok, CompiledTemplate} = sgte:compile_file("priv/template.html"),
    {ok, CompiledBody} = sgte:compile_file("priv/product_search.html"),
    {ok, CompiledMenu} = sgte:compile_file("priv/template_menu.html"),
    RenderedBody = sgte:render(CompiledBody, []),    
    sgte:render_str(CompiledTemplate, 
		    [{title, "search products"}, {body, RenderedBody},
		     {menu, CompiledMenu},
		     {active1, ""}, {active2, "class=\"current_page_item\""},
		     {active3, ""}, {active4, ""},  
		     {action, "/products"},
		     {elems, [[{link, "/products/new"}, {name,"New"}]]}]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% /products/4/edit
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
edit(Data, "text/html") ->
    {ok, Compiled} = sgte:compile_file("priv/template.html"),
    {ok, CompiledBody} = sgte:compile_file("priv/template_product_edit.html"),
    {ok, CompiledMenu} = sgte:compile_file("priv/template_menu.html"),
    {List,Data} = lists:foldr(
	     fun(E, {Acc, PL}) ->
		     {[{E, proplists:get_value(E,PL)} | Acc], PL}
	     end,
	     {[],Data},
	     [id, name, tag, price, amount, description]),
    RenderedBody = sgte:render(CompiledBody, List),
    sgte:render_str(Compiled, 
		    [{title, "Edit product"}, {body, RenderedBody}, 
		     {menu, CompiledMenu},
		     {active1, ""}, {active2, "class=\"current_page_item\""},
		     {active3, ""}, {active4, ""},  
		     {action, "/products"},
		     {elems, [[{link, "/products/new"}, {name,"New"}],
			      [{link, "/products/search"}, {name,"Search"}]]}]).
