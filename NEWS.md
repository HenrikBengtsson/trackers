# Version 0.0.3 [2024-07-10]

## New Features

* Add `track_gc()`, which report on changes in `base::gc.time()`.

* Add `track_time()`, which report on changes in
  `base::process.time()`.

* Now `track_packages()` also report on changes in attached packages.


# Version 0.0.2 [2024-03-11]

## New Features

 * Add `track_connections()`.

 * Add `trace_rm()` to prevent `rm(list = ls())` calls.


# Version 0.0.1 [2022-11-30]

## New Features

 * Add `trace_closeAllConnections()` and `trace_rng_on_load()`
   formerly part of my personal `.Rprofile` startup process.

 * Add `track_*()` functions.

 * Add internal `tracker_*()` callback functions formerly part of my
   personal `.Rprofile` startup process.

