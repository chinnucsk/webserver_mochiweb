-module(db_test).

-export([test/0]).
-include("test.hrl").

test() ->
    ?assertEq(ok, db:start()),
    db:db_delete(products),
    db:db_delete(cart),
    db:db_delete(user),
    db(),
    doc(),
    viewMap(),
    ?assertEq(ok, db:db_delete(products)),
    ?assertEq(ok, db:db_delete(cart)),
    ?assertEq(ok, db:db_delete(user)),
    ?assertEq(ok, db:stop()).

db() ->
    ?assertEq(ok, db:db_create(products)),
    ?assertEq(ok, db:db_create(cart)),
    ?assertEq(ok, db:db_create(user)),
    ?assertMatch({ok, _}, db:db_info(products)),
    ?assertMatch({ok, _}, db:db_info(cart)),
    ?assertMatch({ok, _}, db:db_info(user)),
    ?assertEq({ok, [products,user,cart,webapp]}, db:db_list()).
        
doc() ->
    ?assertEq(ok, db:doc_create(products, 1, product(10, 20))),
    ?assertEq(ok, db:doc_create(products, 2, product(40, 2))),
    ?assertEq(ok, db:doc_create(products, 3, product(35, 50))),
    ?assertEq(ok, db:doc_delete(products, 1)),
    ?assertEq({error, not_found}, db:doc_get(products, 1)),
    ?assertEq({error, not_found}, db:doc_delete(products, 1)),
    ?assertEq(ok, db:doc_create(products, 1, product(5, 10))),
    ?assertEq({ok, [{amount, 2}, {price, 40}]},
	      db:doc_get(products, 2)),
    ?assertEq(ok, db:doc_update(products, 2, product(40, 3))),
    ?assertEq({ok, [{amount, 3}, {price, 40}]},
	      db:doc_get(products, 2)).

viewMap() ->
    viewMapNullKey(),
    viewMapStringKey(),
    viewMapNotStringKey(),
    viewMapOneValue(),
    viewMapOneValueWithoutName(),
    viewMapComplexKey(),
    viewMapStringValue().

viewMapNullKey() ->
    ?assertEq(ok, db:view_create(products, products, 
				 "function(doc){emit(null,doc);}")),
    {ok, Res1} = db:view_access(products, products),
    check_res1(Res1),
    ?assertEq(ok, db:view_delete(products, products)).

viewMapStringKey() ->
    ?assertEq(ok, db:view_create(products, products, 
				 "function(doc){emit(doc._id, doc);}")),
    {ok, Res2} = db:view_access(products, products),
    check_res2(Res2),
    {ok, Res3} = db:view_access(products, products, [{key, "1"}]),
    check_res3(Res3),
    {ok, Res4} = db:view_access(products, products, [{startkey, "1"},
						     {endkey,"2"}]),
    check_res4(Res4),
    {ok, Res5} = db:view_access(products, products,
				[{startkey, "1"},{endkey,"3"},{limit,2}]),
    check_res5(Res5),
    {ok, Res6} = db:view_access(products, products, [{keys, ["1", "3"]}]),
    check_res6(Res6),
    ?assertEq(ok, db:view_delete(products, products)).

viewMapNotStringKey() ->
    ?assertEq(ok, db:view_create(products, products, 
				 "function(doc){emit(doc.price, doc);}")),
    {ok, Res13} = db:view_access(products, products, [{key, 5}]),
    check_res13(Res13),
    ?assertEq(ok, db:view_delete(products, products)).

viewMapOneValue() ->
    ?assertEq(ok, db:view_create(products, products, 
				 "function(doc){emit(doc._id, "
				 "{'amount': doc.amount});}")),
    {ok, Res7} = db:view_access(products, products),
    check_res7(Res7),
    {ok, Res8} = db:view_access(products, products, [{key, "1"}]),
    check_res8(Res8),
    ?assertEq(ok, db:view_delete(products, products)).

viewMapOneValueWithoutName() ->
    ?assertEq(ok, db:view_create(products, products, 
				 "function(doc){emit(doc.price, doc.amount);}")),
    {ok, Res9} = db:view_access(products, products),
    check_res9(Res9),
    {ok, Res10} = db:view_access(products, products, [{key, 5}]),
    check_res10(Res10),
    ?assertEq(ok, db:view_delete(products, products)).

viewMapComplexKey() ->
    ?assertEq(ok, db:view_create(products, products, 
				 "function(doc){emit([doc._id, doc.amount],"
				 "doc.price);}")),
    {ok, Res11} = db:view_access(products, products),
    check_res11(Res11),
    {ok, Res12} = db:view_access_c(products, products, [{startkey, ["3"]}]),
    check_res12(Res12),
    ?assertEq(ok, db:view_delete(products, products)),
    ?assertEq({error, not_found}, db:view_delete(products, products)).

viewMapStringValue() ->
    ?assertEq(ok, db:doc_delete(products, 1)),
    ?assertEq(ok, db:doc_delete(products, 2)),
    ?assertEq(ok, db:doc_delete(products, 3)),
    ?assertEq(ok, db:doc_create(products, 1, product2("poli", 50))),
    ?assertEq(ok, db:doc_create(products, 2, product2("felix", 20))),
    ?assertEq(ok, db:view_create(products, products, 
				 "function(doc){emit(doc._id, "
				 "{name: doc.name});}")),
    {ok, Res14} = db:view_access(products, products),
    check_res14(Res14),
    {ok, Res15} = db:view_access(products, products, [{key, "1"}]),
    check_res15(Res15),
    ?assertEq(ok, db:view_delete(products, products)).


check_res1(Res) ->
    Expected = [[{amount, 3}, {price,40}, {key, null}], 
		[{amount, 10}, {price, 5}, {key, null}],
		[{amount, 50}, {price, 35}, {key, null}]],
    ?assertEq(Expected, lists:sort(Res)).

check_res2(Res) ->
    Expected = [[{amount, 3}, {price,40}, {key, 2}], 
		[{amount, 10}, {price, 5}, {key, 1}],
		[{amount, 50}, {price, 35}, {key, 3}]],
    ?assertEq(Expected, lists:sort(Res)).

check_res3(Res) ->
    Expected = [[{amount, 10}, {price, 5}, {key, 1}]],
    ?assertEq(Expected, lists:sort(Res)).
    
check_res4(Res) ->
    Expected = [[{amount, 3}, {price,40}, {key, 2}], 
		[{amount, 10}, {price, 5}, {key, 1}]],
    ?assertEq(Expected, lists:sort(Res)).

check_res5(Res) ->
    Expected = [[{amount, 3}, {price,40}, {key, 2}], 
		[{amount, 10}, {price, 5}, {key, 1}]],
    ?assertEq(Expected, lists:sort(Res)).

check_res6(Res) ->
    Expected = [[{amount, 10}, {price,5}, {key, 1}], 
		[{amount, 50}, {price,35}, {key, 3}]],
    ?assertEq(Expected, lists:sort(Res)).

check_res7(Res) ->
    Expected = [[{amount, 3}, {key, 2}], 
		[{amount, 10}, {key, 1}],
		[{amount, 50}, {key, 3}]],
    ?assertEq(Expected, lists:sort(Res)).

check_res8(Res) ->
    Expected = [[{amount, 10}, {key, 1}]],
    ?assertEq(Expected, lists:sort(Res)).

check_res9(Res) ->
    Expected = [[{value, 3}, {key, 40}], 
		[{value, 10}, {key, 5}],
		[{value, 50}, {key, 35}]],
    ?assertEq(Expected, lists:sort(Res)).

check_res10(Res) ->
    Expected = [[{value, 10}, {key, 5}]],
    ?assertEq(Expected, lists:sort(Res)).

check_res11(Res) ->
    Expected = [[{value, 5}, {key, [1,10]}], 
		[{value, 35}, {key, [3,50]}],
		[{value, 40}, {key, [2,3]}]],
    ?assertEq(Expected, lists:sort(Res)).

check_res12(Res) ->
    Expected = [[{value, 35}, {key, [3,50]}]],
    ?assertEq(Expected, lists:sort(Res)).

check_res13(Res) ->
    Expected = [[{amount, 10}, {price, 5}, {key, 5}]],
    ?assertEq(Expected, lists:sort(Res)).

check_res14(Res) ->
    Expected = [[{name, "felix"}, {key, 2}],
		[{name, "poli"}, {key, 1}]],
    ?assertEq(Expected, lists:sort(Res)).

check_res15(Res) ->
    Expected = [[{name, "poli"}, {key, 1}]],
    ?assertEq(Expected, lists:sort(Res)).
    
product(Price, Amount) ->
    [{"price", Price}, {"amount", Amount}].

product2(Price, Amount) ->
    [{"name", Price}, {"amount", Amount}].


