$(document).ready(function() {
  $('option[value=en]').prop('selected', true);
  $('input[name=text]').focus();

  $('#sound-form').isHappy({
    fields: {
      '#input-text': {
        message: 'Input is over 100 characters! It will be truncated.',
        test: validateLength,
        trim: false,
        when: 'input keyup'
      },
    }
  });

  function validateLength(text) {
    return text.length <= 100;
  }

  $('body').on('click', 'button.play', function() {
    var audio = $(this).siblings('audio');
    audio.trigger('play');
  });

  $('body').on('click', 'button.save', function() {
    window.open( $(this).data('sound'), '_blank' );
  });

  $('#submit-text').on('click', function(e) {
    e.preventDefault();

    if ( $('#input-text').val().length == 0) {
      // don't submit if input text is empty
      return false;
    }

    var data = $('.content form').serialize();

    $.ajax({
      type: 'POST',
      url: '/sounds',
      data: data,
      success: loadResult,
      error: function() {
        showError( 'Unable to create that sound. '
                 + 'Please send me and email if this continues to occur.'
                 );
      }
    });

    // clear input box
    $('input[name=text]').val('');
    $('input[name=text]').keyup(); // satisfy isHappy
  });

  function loadResult(res) {
    if (res.success) {
      getSound(res.id);
    } else {
      console.log("Error occurred creating sound");
    }
  }

  function getSound(id) {
    $.ajax({
      type: 'GET',
      url: '/sounds/' + id,
      success: showSound,
      error: function() {
        showError( 'Unable to retrieve sound. '
                 + 'Please send me an email if this continues to occur.'
                 );
      }
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
      },
      error: function() {
        showError( 'Something really bad happened. '
                 + 'You should probably reload the page. '
                 + 'Please send me an email if this continues to occur.'
                 );
      }
    });
  }

  function showError(text) {
    noty({
      layout: 'top',
      theme: 'relax',
      type: 'error',
      text: text,
      killer: true
    });
  }
});
