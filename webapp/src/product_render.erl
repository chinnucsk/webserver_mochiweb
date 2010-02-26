-module(product_render).

-export([get/2, get_list/2, new/1]).

-include_lib("webapp.hrl").


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
    {ok, Compiled2} = sgte:compile("priv/template_list.html"),
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
    {ok, CompiledBody} = sgte:compile("$map element elems$ "),
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
    sgte:render_str(CompiledTemplate, [{title, "products"},
				       {summary, ""},
				       {body, RenderedBody}]);
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
new("application/xml") ->
    "ok";
new("text/html") ->
    {ok, CompiledTemplate} = sgte:compile_file("priv/template.html"),
    {ok, CompiledBody} = sgte:compile_file("priv/product_new.html"),
    RenderedBody = sgte:render(CompiledBody, []),    
    sgte:render_str(CompiledTemplate, [{title, "new product"},
				       {summary, ""},
				       {body, RenderedBody}]).
