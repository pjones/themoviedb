# TheMovieDB API for Haskell

This is a simple library that provides functions for retrieving movie
metadata from [TheMovieDB][] API.  To use this library you need to
request an API key from TheMovieDB.  Follow the directions on the
[API][] page.

[TheMovieDB]: http://themoviedb.com
[API]: http://docs.themoviedb.apiary.io

# Documentation

See the [Network.API.TheMovieDB][] module for complete documentation.

[Network.API.TheMovieDB]: https://github.com/pjones/themoviedb/blob/master/Network/API/TheMovieDB.hs

# Example

There's an [example][] application in the `example` directory.
Surprising, I know.

[example]: https://github.com/pjones/themoviedb/blob/master/example/Main.hs

# Warning

This library currently uses HTTP and not *HTTPS* while sending your
API key to TheMovieDB.  In order to fix this we'll need to use
something other than `Network.HTTP`.  Patches welcome!
