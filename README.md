# Glancer

> **glancer**: 

> NOUN _informal_ a person who glances

> **glance**: 

> VERB If you glance at something or someone, you look at them very quickly and then look away again immediately.

> VERB If you glance through or at a newspaper, report, or book, you **spend a short time looking at it without reading it very carefully**.

---

- [Glancer](#glancer)
  - [Installation](#installation)
  - [Usage](#usage)
  - [Notes and TODOs](#notes-and-todos)

---

The amount of online conferences has skyrocketed lately, I wonder why. This has caused my _Pending to watch_ list to balloon from 30-ish pending technical videos (which are already a lot) to more than 100.

There are then 2 problems:
1. I have too many techie conference videos to watch
2. In a lot of cases I realise halfway through that the subject wasn't that interesting or that I already know the area to be covered.

For a long while I have had a similar problem with written articles. I solved it by:
1. Forcing me to read a substantial amount by [writing a weekly list of the best ones](https://mostlymaths.net/tags/readings/)
2. Brutally stop reading any article that is not good enough to _possibly be_ in that list.

This is easy in writing: you can quickly scan the text and decide if it looks interesting enough for a deep dive in a few seconds (tech article reads range from a few minutes to around half an hour, depending of how technical it may get). But there is no way of doing it in videos! You need to watch maybe 15-20 minutes to then realise "meh".

_Glancer_ should help with this. Given a YouTube url, it will:
- Download the corresponding video (to a temporary folder),
- Download the auto-generated subtitles (assumes English, hardcoded),
- Capture images from the video every N=30 seconds (hardcoded for the moment),
- Convert the images to base64,
- Create a standalone webpage with the screenshots on the left and the corresponding text on the right.

The goal is to be able to glance at the talk to decide if you really want to watch it or not. The _standalone_ part of the created webpage is to make it easier to "watch"/"share" to my iPad/iPhone without having to move a folder full of images. The whole talk becomes just a 5-15 Mb HTML file.

A couple of additional neat (for me at least) features:
- Clicking/tapping on the image will enlarge it, in case you want to see some code block larger (I wanted hover, but it was too tricky on mobile).
- Clicking on the arrow on the lower-right of the slid block will open the video on Youtube, at that moment in time.

## Installation

```bash
git clone https://github.com/rberenguel/glancer
cd glancer
stack install
```

You will need to have installed/available in the path:

- The `base64` executable (should be in all IX systems by default)
- `cat` in `/bin/cat` (likewise)
- [`youtube-dlc`](https://github.com/blackjack4494/yt-dlc) installed. Note the _c_. There is currently a bug in the normal one with downloading auto-generated subtitles.

## Usage

```
Usage: glancer URL FILEPATH
  Glancer

Available options:
  URL                      Youtube URL
  FILEPATH                 HTML file name (don't add extension)
  -h,--help                Show this help text
```

In other words, `glancer https://www.youtube.com/watch?v=JWQxd3YKWhs internals-pyspark-arrow` would create the webpage `internals-pyspark-arrow.html` in the current folder, after processing the talk I gave at Spark Summit 2019. You can see the generated file [here](https://www.mostlymaths.net/glancer/example/internals-pyspark-arrow.html).

Sometimes `youtube-dlc` won't be able to find the embedded youtube video (I've seen this happen randomly in Spark Summit North America 2020 videos in databricks.com), in this case the process will fail. Try to feed it youtube urls directly.

## Notes and TODOs

- [ ] Making the time between images customizable via the CLI (if I find out 30 is not good enough in general).
- [ ] Add a test suite to harden subtitle parsing. I always think parsers will be small enough and that it will be "obvious" they work. It's never the case, at least I did it [right](https://github.com/rberenguel/haskset/blob/master/test/Spec.hs) [twice](https://github.com/rberenguel/bear-note-graph/blob/master/tests/test_parser.py).
- [ ] Make the still images video-dependent (so several `glancer` commands can run concurrently, even if it's a bad idea)
- [x] ~Some additional tweaks to the HTML/CSS (possibly adding some JS as well)~

_Note_: This README is long and winding on purpose.

