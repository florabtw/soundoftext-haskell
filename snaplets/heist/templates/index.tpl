<!DOCTYPE html>
<html>
  <head>
    <title>Sound of Text | Download Google Translate MP3 Audio</title>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
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
          <form action="/sounds" method="post">
            <div class="field">
              <label for="text">Text:</label>
              <input name="text" type="text" />
            </div>
            <div class="field">
              <label for="lang">Language:</label>
              <select name="lang">
                <option value="af">Afrikaans</option>
                <option value="sq">Albanian</option>
                <option value="ar">Arabic</option>
                <option value="az">Azerbaijani</option>
                <option value="eu">Basque</option>
                <option value="bn">Bengali</option>
                <option value="be">Belarusian</option>
                <option value="bg">Bulgarian</option>
                <option value="ca">Catalan</option>
                <option value="zh-CN">Chinese Simplified</option>
                <option value="zh-TW">Chinese Traditional</option>
                <option value="hr">Croatian</option>
                <option value="cs">Czech</option>
                <option value="da">Danish</option>
                <option value="nl">Dutch</option>
                <option value="en" selected>English</option>
                <option value="eo">Esperanto</option>
                <option value="et">Estonian</option>
                <option value="tl">Filipino</option>
                <option value="fi">Finnish</option>
                <option value="fr">French</option>
                <option value="gl">Galician</option>
                <option value="ka">Georgian</option>
                <option value="de">German</option>
                <option value="el">Greek</option>
                <option value="gu">Gujarati</option>
                <option value="ht">Haitian Creole</option>
                <option value="iw">Hebrew</option>
                <option value="hi">Hindi</option>
                <option value="hu">Hungarian</option>
                <option value="is">Icelandic</option>
                <option value="id">Indonesian</option>
                <option value="ga">Irish</option>
                <option value="it">Italian</option>
                <option value="ja">Japanese</option>
                <option value="kn">Kannada</option>
                <option value="ko">Korean</option>
                <option value="la">Latin</option>
                <option value="lv">Latvian</option>
                <option value="lt">Lithuanian</option>
                <option value="mk">Macedonian</option>
                <option value="ms">Malay</option>
                <option value="mt">Maltese</option>
                <option value="no">Norwegian</option>
                <option value="fa">Persian</option>
                <option value="pl">Polish</option>
                <option value="pt">Portuguese</option>
                <option value="ro">Romanian</option>
                <option value="ru">Russian</option>
                <option value="sr">Serbian</option>
                <option value="sk">Slovak</option>
                <option value="sl">Slovenian</option>
                <option value="es">Spanish</option>
                <option value="sw">Swahili</option>
                <option value="sv">Swedish</option>
                <option value="ta">Tamil</option>
                <option value="te">Telugu</option>
                <option value="th">Thai</option>
                <option value="tr">Turkish</option>
                <option value="uk">Ukrainian</option>
                <option value="ur">Urdu</option>
                <option value="vi">Vietnamese</option>
                <option value="cy">Welsh</option>
                <option value="yi">Yiddish</option>
              </select>
            </div>
            <div class="field">
              <input id="submit-text" type="submit" value="Submit" />
            </div>
          </form>
        </div>
        <div class="wrapped">
          <div class="results-header">
            <h1>Results</h1>
            <button>Save all</button>
          </div>
          <ol class="results">
          </ol>
        </div>
      </div>
    </div>
  </body>
</html>
