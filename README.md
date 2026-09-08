# TrailBlazer

An interactive R Shiny dashboard for exploring GPS running activities. It
turns timestamped latitude, longitude, and elevation samples into route maps,
pace and speed summaries, elevation profiles, comparative tables, and featured
run views.

**[Open the deployed dashboard](https://tinomuzambi.shinyapps.io/TrailBlazer/)**

## Run locally

Install R and the packages listed at the top of `MZMTIN002.R`, then run:

```r
shiny::runApp(source("MZMTIN002.R")$value)
```

The checked-in `data/example-runs.csv` is synthetic and makes a fresh clone
immediately explorable. To analyse your own exports, add CSV files with these
columns:

```text
date,time,lat,lng,elevation
```

Each file is treated as one run. Personal activity exports and shinyapps.io
deployment metadata are ignored by Git.

## Engineering notes

- distance uses the Haversine formula between consecutive GPS samples
- ascent sums only positive point-to-point elevation changes
- calculations are grouped by source file before route summaries are produced
- the repository contains no production credentials or personal activity data

Code and synthetic example data are MIT licensed.
