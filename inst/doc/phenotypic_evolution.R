## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
#  vignette("paleoTS_functionality")

## ----setup--------------------------------------------------------------------
library(StratPal)
library(admtools)

## ----fig.alt="plot of a random walk"------------------------------------------
seq(0, 1, by = 0.01) |>             # times of simulation in Myr. Simulate over 1 Myr years with 10 kyr resolution
  random_walk(sigma = 1, mu = 3) |> # simulate random walk with increasing trait values
  plot(type = "l",                  # plot results
       xlab = "Time [Myr]",
       ylab = "Trait value")

## ----figures-side, fig.show="hold", out.width="50%", fig.alt = "2 plots of age-depth models, one 2 km from shore and one 12 km from shore"----
adm_2km = tp_to_adm(t = scenarioA$t_myr,    # 2 km from shore
                h = scenarioA$h_m[,"2km"],
                T_unit = "Myr",
                L_unit = "m")

adm_12km = tp_to_adm(t = scenarioA$t_myr,   # 12 km from shore
                h = scenarioA$h_m[,"12km"],
                T_unit = "Myr",
                L_unit = "m")

plot(adm_2km,       # plot age-depth model 2 km from shore
     lwd_acc = 2,   # plot thicker lines for intervals with sediment accumulation (lwd = line width)
     lty_destr = 0) # don't plot destructive intervals/gaps (lty = line type)
T_axis_lab()        # add time axis label
L_axis_lab()        # add length axis label
title("Age-depth model 2 km from shore")

plot(adm_12km,      # plot age-depth model 12 km from shore
     lwd_acc = 2,   
     lty_destr = 0) 
T_axis_lab() 
L_axis_lab() 
title("Age-depth model 12 km from shore")

## ----fig.alt="plot of the stratigraphic experssion of a random walk 2 km from shore."----

seq(from = min_time(adm_2km), to = max_time(adm_2km), by = 0.01) |> # sample every 10 kyr over the interval covered by the adm 
  random_walk(sigma = 1, mu = 3) |>                                 # simulate random walk
  time_to_strat(adm_2km, destructive = FALSE) |>                    # transform data from time to strat domain
  plot(type = "l",                                                  # plot
       orientation = "lr",
       xlab = paste0("Stratigraphic height [", get_L_unit(adm_2km), "]"),
       ylab = "Trait value",
       main = "Trait evolution 2 km from shore")

## ----fig.alt="plot of the stratigraphic expression of a random walk 12 km from shore."----
seq(from = min_time(adm_12km), to = max_time(adm_12km), by = 0.01) |> # sample every 10 kyr over the interval covered by the adm 
  random_walk(sigma = 1, mu = 3) |>                                   # simulate random walk
  time_to_strat(adm_12km, destructive = FALSE) |>                     # transform data from time to strat domain
  plot(type = "l",                                                    # plot results
       orientation = "lr",
       xlab = paste0("Stratigraphic height [", get_L_unit(adm_12km), "]"),
       ylab = "Trait value",
       main = "Trait evolution 12 km from shore")

## ----fig.alt = "plot of the stratigraphic expression of a random walk 2 km from shore, sampled every 2 km."----
dist_between_samples_m = 2
sampling_loc_m = seq(from = 0.5 * dist_between_samples_m,
                     to = max_height(adm_2km),
                     by = dist_between_samples_m)

sampling_loc_m |>                                  # sampling locations
  strat_to_time(adm_2km) |>                        # determine times where lineage is sampled
  random_walk(sigma = 1, mu = 3) |>                # simulate trait values at these times
  time_to_strat(adm_2km, destructive = FALSE) |>   # transform trait values to stratigraphic domain
  plot(orientation = "lr",                         # plot stratigraphic data
       type = "l",        
       ylab = "Trait Value",
       xlab = paste0("Stratigraphic height [", get_L_unit(adm_2km), "]"),
       main = "Trait evolution 2 km from shore")

## ----eval=FALSE---------------------------------------------------------------
#  vignette("paleoTS_functionality")

## ----eval=FALSE---------------------------------------------------------------
#  vignette("event_data")

## ----eval=FALSE---------------------------------------------------------------
#  vignette("advanced_functionality")

