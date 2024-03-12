<div id="badges"><!-- pkgdown markup -->
<a href="https://github.com/HenrikBengtsson/trackers/actions?query=workflow%3Acheck-full"><img border="0" src="https://github.com/HenrikBengtsson/trackers/actions/workflows/check-full.yaml/badge.svg?branch=develop" alt="R CMD check status"/></a>
</div>

# trackers: Track Changes in R

## Features

Callback handlers that tracks changes in various states of R:

 * `track_globalenv()` - track changes to the global environment
 * `track_rng()` - track changes to the state of the random number
   generator (RNG)
 * `track_rplots_files()` - detect when `Rplots*.pdf` files are created
 * `track_sink()` - track changes in R "output" and "message" sinks
 * `track_files()` - track changes in files (only names; not
   content)
 * `track_options()` - track changes in R options
 * `track_envvars()` - track changes in environment variables
 * `track_locale()` - track changes in the R locale
 * `track_packages()` - track changes in the set of loaded R
   packages


Function tracers that are activated when a specific R function is
called:

 * `trace_closeAllConnections()` - trigger an error or a warning
   whenever `base::closeAllConnections()` is called.

 * `trace_rng_on_load()` - generate a warning if the RNG state is
   changed from a package being loaded


## Installation

R package **trackers** is only available on
[GitHub](https://github.com/HenrikBengtsson/trackers) and can be
installed in R as:

```r
remotes::install_github("HenrikBengtsson/trackers", ref = "develop")
```

This installs the package from source.
