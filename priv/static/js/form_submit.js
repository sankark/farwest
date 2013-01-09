$(document).ready(function() {
	$('#form').submit(function(){
		var url;
			url = window.location.pathname;
			alert(url);
		var self = this;
		$(this).ajaxSubmit({
			url: url,
			error: function(xhr){
				var errors = $.parseJSON(xhr.responseText);
				for (name in errors){
					$('<p class="alert alert-error">' + errors[name] + '</p>')
						.insertBefore('[name=' + name + ']');
				}
			},
			success: function(){
				$('.alert', self).remove();
				$(self).prepend('<p class="alert alert-success">Success!</p>');
			}
		});

		return false;
	});

	$('a.delete').bind('click', function(event){
		event.preventDefault();
		$.ajax({
			type: 'DELETE',
			url: this.href
		});
	});
});
