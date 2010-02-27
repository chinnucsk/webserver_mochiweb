$(document).ready(function() {
  $("#search-submit").click(function() {
    var id = $("#search-text").val();
    window.location.replace("/products/" + id);
    return false;
  });
});