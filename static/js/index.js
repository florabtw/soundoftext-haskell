$(document).ready(function() {
  $('option[value=en]').prop('selected', true);

  $('#submit-text').on('click', function(e) {
    e.preventDefault();

    var data = $('.content form').serialize();

    $.ajax({
      type: 'POST',
      url: '/sounds',
      data: data,
      success: loadResult
    });
  });

  function loadResult(res) {
    if (res.success) {
      getSound(res.id);
    } else {
      // handle error
      console.log("Error occurred creating sound");
    }
  }

  function getSound(id) {
    $.ajax({
      type: 'GET',
      url: '/sounds/' + id,
      success: showSound
    });
  }

  function showSound(res) {
    $('#results').prepend( res );
  }
});
