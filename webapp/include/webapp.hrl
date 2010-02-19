-record(product,
       {id,
	name,
	price,
	amount,
	description
       }).

-record(user,
	{name,
	 password
	}).

-record(cart,
	{id,
	 user,
	 goods
	}).

-record(counter,
	{table,
	 id}).

-define(SUPPORTED_MEDIA, ["text/html","application/xml","text/plain",
			  "application/xhtml+xml"]).
