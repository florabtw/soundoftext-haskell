# Sound of Text

Sound of Text gives you the ability to listen to or download audio clips generated from Google Translate
directly from your browser.

## Tech Stack

- Haskell
- [Snap](http://snapframework.com/) - web framework
- [Heist](https://github.com/snapframework/heist) - templating engine
- [Snaplet Sqlite Simple](https://github.com/nurpax/snaplet-sqlite-simple) - SQLite interaction
- [jQuery](https://jquery.com/)
- [HappyJS](http://happyjs.com/) - form validation
- [Noty](http://ned.im/noty/#/about) - popup notifications

## Execution Instructions

### Environment setup

At minimum you need GHC and Cabal. You can install both of these with the
[Haskell Platform](https://www.haskell.org/platform/).

Or, if you're on debian Linux:

    $ sudo apt-get install haskell-platform

### Acquisition

    $ git clone https://github.com/ncpierson/soundoftext.git

### The Build

It is recommended that you have more than 512MB memory free for this, as cabal can be quite memory-intensive.
If you don't have more than 512MB memory, [this StackOverflow link might be helpful]
(http://stackoverflow.com/questions/20769183/cabal-update-failed-due-to-out-of-memory).

    $ cd soundoftext
    $ cabal sandbox init
    $ cabal install --only-dependencies
    $ cabal build
    $ cabal install

### Run it

To run on default port (8000), run this command from the root directory:

    $ soundoftext

If you would like to run on port 80, or any other port, run the command with the `-p` option

    $ soundoftext -p 80

The executable itself is found at `dist/build/soundoftext/soundoftext`.

### See it

Navigate to `localhost:8000` or `localhost` in your browser and hope that everything worked.

## Other notes

- This site is already hosted at [soundoftext.com](http://soundoftext.com).

- Google Analytics is included in the `snaplets/heist/templates/soundoftext.tpl` index page. Please remove that
  if you intend on hosting this site for any significant amount of time.

- This is a rebuild of my [previous project](https://github.com/ncpierson/soundoftext-old), which was written in PHP.
