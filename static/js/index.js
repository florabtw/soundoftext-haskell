$(document).ready(function() {
  $('option[value=en]').prop('selected', true);
  $('input[name=text]').focus();

  $('body').on('click', 'button.play', function() {
    var audio = $(this).siblings('audio');
    audio.trigger('play');
  });

  $('body').on('click', 'button.save', function() {
    window.open( $(this).data('sound'), '_blank' );
  });

  $('#submit-text').on('click', function(e) {
    e.preventDefault();

    var data = $('.content form').serialize();

    $.ajax({
      type: 'POST',
      url: '/sounds',
      data: data,
      success: loadResult
    });

    $('input[name=text]').val('');
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
    if ( $('#results').length == 0 ) {
      showResults(res);
    } else {
      $('#results').prepend(res);
    }
  }

  function showResults(child) {
    $.ajax({
      type: 'GET',
      url: '/results',
      success: function(res) {
        $('.content').after(res);
        $('#results').prepend(child);
      }
    });
  }
});
