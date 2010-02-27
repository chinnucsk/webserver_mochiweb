$(document).ready(function() {
  $("#search-submit").click(function() {
    var id = $("#search-text").val();
    window.location.replace("/products/" + id);
    return false;
  });

  $("#new_form").submit(function() {
    var action = $(this).attr("action");
    $.ajax(
      {type: "POST",
      url: action,
       dataType: "json",
       data: $(this).serialize(),
       success:
	 function(data) {
	   if(data["ok"] == true) {
	     window.location.replace("/products");
	   } else {
	     alert("The parameters provided are not correct");
	   }
	 },
       error: function(xhr,ajaxOptions,thrownError) {
	 alert("Error " + xhr.status + ". " + xhr.statusText);
       }
      });
    return false;
  });
});
