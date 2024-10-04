## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(StratPal)

## ----fig.alt="plot of strict stasis"------------------------------------------
library(StratPal)
library(paleoTS)   # needed for plotting

strict_stasis_sl(t = 1:4) |>   # simulate strict stasis on specimen level in `pre_paleoTS` format
  reduce_to_paleoTS() |>       # convert pre_paleoTS to paleoTS
  plot()                       # plot

## ----fig.alt="plot of a random walk after stratigraphic transformation"-------
library(admtools)                                  # load admtools for stratigraphic transformation
adm = tp_to_adm(t = scenarioA$t_myr,               # define age-depth model
                h = scenarioA$h_m[,"2km"],
                L_unit = "m",
                T_unit = "Myr")    
set.seed(42)                                      # set seed for reproducibility
seq(min_time(adm), max_time(adm), by = 0.01) |>   # sample every 0.01 Myr
  random_walk_sl(n_per_sample = 10) |>            # simulate random walk on specimen level
  time_to_strat(adm) |>                           # transform into stratigraphic domain
  reduce_to_paleoTS() |>                          # transform into paleoTS format
  plot()                                          # plot

## -----------------------------------------------------------------------------
set.seed(42)                                      # set seed for reproducibility
seq(min_time(adm), max_time(adm), by = 0.01) |>   # sample every 0.01 Myr
  random_walk_sl(n_per_sample = 10) |>            # simulate random walk on specimen level
  time_to_strat(adm) |>                           # transform into stratigraphic domain
  reduce_to_paleoTS() |>                          # transform into paleoTS format
  fit3models()                                    # fit 3 models to time series

