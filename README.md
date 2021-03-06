This Haskell script turns a [get_iplayer][gi] download history file into a podcast feed.

There are other tools that do the same or similar things, such as [Martin O'Hanlon's get_iplayer_genrss][martin],
but this one is mine and it's what I use.

These days, the BBC Sounds app exists and supports downloads,
but I'm still using this because it works and I guess I just really like my podcast app/breaking the law.

## Usage

First, edit the various configuration things at the top of `iplayer-podcast.hs`, and compile by running `cabal install`.
Then:

    ./iplayer-podcast > output.xml

It goes without saying that you'll need to download some things using get_iplayer for this to be useful.
I've added all sorts of things to [the PVR][pvr], and a line in my `crontab` runs `get_iplayer --pvr-run` periodically.

[gi]: http://squarepenguin.co.uk/
[martin]: https://github.com/martinohanlon/get_iplayer_genrss
[app]: http://www.bbc.co.uk/blogs/internet/entries/4eba16ca-f1e8-4744-a305-7a31b3804535
[pvr]: https://squarepenguin.co.uk/wiki/documentation/#pvr-usage
