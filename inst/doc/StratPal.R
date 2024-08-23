## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include=FALSE-----------------------------------------------------
library(StratPal)
library(admtools)

## ----eval=FALSE---------------------------------------------------------------
#  library(admtools)

## ----eval=FALSE---------------------------------------------------------------
#  browseVignettes(package = "admtools")

## -----------------------------------------------------------------------------
plot(x = scenarioA$t_myr,
     y = scenarioA$sl_m,
     type = "l",
     xlab = "Time [Myr]",
     ylab = "Eustatic sea level [m]",
     main = "Sea level curve used as model input")

## -----------------------------------------------------------------------------
set.seed(42)             # set seed for computational reproducibility
t = seq(0, 1, by = 0.01) # times where we evaluate the random walk
l = random_walk(t)       # simulate the random walk
plot(l, type = "l")      # line plot of the results

## -----------------------------------------------------------------------------
set.seed(42)            # set seed for computational reproducibility
seq(0, 1, by = 0.01) |> # define times of simulation  
  random_walk() |>      # simulate random walk
  plot(type = "l")      # plot

## -----------------------------------------------------------------------------
# calculate deciles of normal distribution
seq(0, 1, by = 0.1) |>
  quantile(x = runif(100), p = _) # pass left hand side to the p argument

## ----eval=FALSE---------------------------------------------------------------
#  browseVignettes(package = "admtools")

## -----------------------------------------------------------------------------
library("admtools")

## -----------------------------------------------------------------------------
t = scenarioA$t_myr       # extract time tie points
h = scenarioA$h_m[,"2km"] # get height tie points 2 km offshore in scenario A

# define age-depth model
# h[i] is the stratigraphic position at time t[i]
adm = tp_to_adm(t = t,          # tie points in time
                h = h,          # tie points at height
                T_unit = "Myr", # add time unit 
                L_unit = "m")   # add length unit

## -----------------------------------------------------------------------------
# plot age-depth model, see ?plot.adm for details
plot(adm,
     lwd_acc = 2,   # plot thicker lines for intervals with sediment accumulation (lwd = line width)
     lty_destr = 0) # don't plot destructive intervals/gaps (lty = line type)
T_axis_lab()        # add time axis label
L_axis_lab()        # add length axis label
title("Age-depth model 2 km from shore")

## -----------------------------------------------------------------------------
get_total_duration(adm) # time interval covered by adm
get_total_thickness(adm) # sediment accumulated 
get_completeness(adm) # stratigraphic completeness
summary(adm) # some summary statistics

## -----------------------------------------------------------------------------

# plot histogram of hiatus durations
adm |>    
  get_hiat_duration() |>
  hist( xlab = paste("Hiatus duration", "[", get_T_unit(adm),"]"))

## ----eval=FALSE---------------------------------------------------------------
#  vignette("phenotypic_evolution")

## ----eval=FALSE---------------------------------------------------------------
#  vignette("event_data")

## ----eval=FALSE---------------------------------------------------------------
#  vignette("advanced_functionality")

