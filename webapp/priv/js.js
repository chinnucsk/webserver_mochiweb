$(document).ready(function() {

  //Tweak for the quick search form
  $("#search-submit").click(function() {
    var id = $("#search-text").val();
    var url = $("#quick_search").attr("action");
    window.location.replace(url + "/" + id);
    return false;
  });

  //Tweak for the new resource form, so that we can ask for JSON back
  //and redirect to /products if it went OK
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
	     window.location.replace(action);
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

  //Disable empty fields in the search form so that they don't appear in the URI
  $("#search_form").submit(function() {
    //TODO: check params
    $(":input", this).each(function() {
      if(this.value == "") $(this).attr("disabled", "disabled");
    });
    $(this).submit();
  });


  //Create a new window when clicking a link in a list of resources
  $(".link").click(function() {
    window.open(this.href);
    return false;
  });


  //Delete a product
  $("#delete").click(function() {
    $.ajax({
      type: "delete",
      url: $(this).attr("href"),
      dataType: "json",
      success: function(data) {
	if(data["ok"] == true) {
	  window.location.replace("/products");
	} else {
	  alert("An error occurred");
	}
      },
      error: function(xhr, textStatus, errorThrown) {
	alert("Oooops! Request failed with status: " +
	      xhr.status + " " + xhr.responseText);
      }
    });
    return false;
  });


  //Update a product
  $("#update_form").bind("submit", function() {
    var url = $(this).attr("action");
    $.ajax({
      type: "put",
      url: url,
      dataType: "json",
      data: $(this).serialize(),
      processData: false,
      success: function(data) {
	if(data["ok"] == true) {
	  window.location.replace(url);
	} else {
	  alert("An error occurred");
	}
      },
      error: function(xhr, textStatus, errorThrown) {
	alert("Oooops! Request failed with status: " +
	      xhr.status + " " + xhr.responseText);
      }
    });
    return false;
  });


});
