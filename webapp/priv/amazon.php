<?php
include("aws_signed_request.php");
 
$public_key = "AKIAIUYQ5C3HFXJIPKEQ";
$private_key = "RWz4kI3cmDAjIX90p0xGLDDAYQEZytYuRhGjDLrc";
$pxml = aws_signed_request("com", 
			   array("Operation"=>"ItemSearch",
				 "SearchIndex"=>"Books",
				 "Author" => "larsson",
				 "ResponseGroup"=>"Small, ItemAttributes"), 
			   $public_key, $private_key);
?>
