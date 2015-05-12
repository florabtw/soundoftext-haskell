<!DOCTYPE html>
<html>
  <head>
    <title>Sound of Text | Download Google Translate MP3 Audio</title>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <script src="https://ajax.googleapis.com/ajax/libs/jquery/2.1.4/jquery.min.js"></script>
    <script src="/static/js/happy.js"></script>
    <script src="/static/js/jquery.noty.packaged.min.js"></script>
    <script src="/static/js/index.js"></script>
    <script>
      (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
      (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
      m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
      })(window,document,'script','//www.google-analytics.com/analytics.js','ga');
      ga('create', 'UA-45359244-2', 'auto');
      ga('send', 'pageview');
    </script>
    <link rel="stylesheet" href="/static/stylesheets/index.css">
    <!--[if IE 9]>
    <link rel="stylesheet" href="/static/stylesheets/ie.css">
    <![endif]-->
    <!--[if lte IE 8]>
    <script src="/static/js/nosupport.js"></script>
    <![endif]-->
  </head>
  <body>
    <div class="outer">
      <div class="inner">
        <div class="wrapped content">
          <h1>
            Sound of Text
          </h1>
          <p>
            Allowing you to download the MP3 audio from
            <a href="https://translate.google.com">Google Translate</a>.
            Simply type what you want to hear - up to 100 characters - and hit submit.
          </p>
          <form id="sound-form" action="/sounds" method="post">
            <div class="field">
              <label for="text">Text:</label>
              <input id="input-text" name="text" type="text" />
            </div>
            <div class="field">
              <label for="lang">Language:</label>
              <select name="lang">
                <languages>
                <option value="${key}"><name /></option>
                </languages>
              </select>
            </div>
            <div class="field">
              <input id="submit-text" type="submit" value="Submit" />
            </div>
          </form>
        </div>
        <div class="links">
          <a href="mailto:nick.c.pierson@gmail.com">
            nick.c.pierson@gmail.com
          </a>
          |
          <a href="https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=TTRBWFEKUSRSU">
            donate via paypal
          </a>
          |
          <a href="http://coinbase.com/ncpierson">
            donate via bitcoin
          </a>
        </div>
      </div>
    </div>
  </body>
</html>
