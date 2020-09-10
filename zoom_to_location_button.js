$(document).keyup(function(event) {
    if ($("#target").is(":focus") && (event.key == "Enter")) {
        $("#zoom_to_location").click();
    }
});