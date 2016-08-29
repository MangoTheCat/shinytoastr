
Shiny.addCustomMessageHandler(
    "toastr",
    function(message) {
	toastr[message.type](
	    message.message,
	    message.title,
	    message.options
	);
    }
);

Shiny.addCustomMessageHandler(
    "toastr_clear",
    function(message) {
	if (message.with_animation) {
	  toastr.clear();
	} else {
	  toastr.remove();
	}
    }
);

