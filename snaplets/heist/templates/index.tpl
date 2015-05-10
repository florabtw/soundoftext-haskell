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
    <link rel="stylesheet" href="/static/stylesheets/index.css">
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
      </div>
    </div>
  </body>
</html>
