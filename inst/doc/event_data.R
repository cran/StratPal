## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(StratPal)
library(admtools)

## -----------------------------------------------------------------------------
# simulate fossil occurrences over one Myr with an average of 15 occurrences per Myr
p3(rate = 15, from = 0, to = 1) |>
  hist(main = "Fossil abundance",
       xlab = "Time [Myr]",
       ylab = "# Specimens")

## -----------------------------------------------------------------------------
# return 100 occurrences by setting n parameter
# note that negative rates (where sin < 0) are ignored
p3_var_rate(x = sin, from = 0, to = 9, n = 100) |>
  hist(xlab = "Time [Myr]",
       ylab= "# Specimens",
       main = "Fossil abundance")

## -----------------------------------------------------------------------------
# decline in last occurrences from 50 to 0 over 1 Myr
p3_var_rate(x = c(0,1), y = c(50, 0), from = 0, to = 1, f_max = 50) |>
  hist(xlab = "Time [Myr]",
       main = "Last occurrences",
       ylab = "# Last occurrences")

## ----figures-side, fig.show="hold", out.width="50%"---------------------------
adm_2km = tp_to_adm(t = scenarioA$t_myr,   # 2 km from shore
                h = scenarioA$h_m[,"2km"],
                T_unit = "Myr",
                L_unit = "m")

adm_12km = tp_to_adm(t = scenarioA$t_myr,  # 12 km from shore
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

## -----------------------------------------------------------------------------
p3(rate = 200, from = min_time(adm_2km), to = max_time(adm_2km)) |> # constant rate in time domain
  time_to_strat(adm_2km, destructive = TRUE) |>                     # transform into depth domain
  hist(xlab = "Stratigraphic height [m]",                           # plot
       main = "Fossil abundance 2 km offshore",
       ylab = "# Fossils",
       breaks = seq(from = min_height(adm_2km), to = max_height(adm_2km), length.out = 20))

## -----------------------------------------------------------------------------
p3(rate = 200, from = min_time(adm_12km), to = max_time(adm_12km)) |>  # same rate as 2 km from shore
  time_to_strat(adm_12km, destructive = TRUE) |>                       # use different adm for transformation
  hist(xlab = "Stratigraphic height [m]",                              # plot histogram
       main = "Fossil abundance 12 km offshore",
       ylab = "# Fossils",
       breaks = seq(from = min_height(adm_12km), to = max_height(adm_12km), length.out = 20))

## -----------------------------------------------------------------------------
p3(rate = 200, from = min_time(adm_2km), to = max_time(adm_2km)) |> # constant rate of last occ
  time_to_strat(adm_2km, destructive = FALSE) |>                    # non-destructive transformation!
  hist(xlab = "Stratigraphic height [m]",                           # plot histogram
       main = "Last occurrences 2 km offshore",
       ylab = "# Last occurrences",
       breaks = seq(from = min_height(adm_2km), to = max_height(adm_2km), length.out = 20))

## -----------------------------------------------------------------------------
p3(rate = 200, from = min_time(adm_12km), to = max_time(adm_12km)) |>
  time_to_strat(adm_12km, destructive = TRUE) |>
  hist(xlab = "Stratigraphic height [m]",
       main = "Last occurrences 12 km offshore",
       ylab = "# Last occurrences",
       breaks = seq(from = min_height(adm_12km), to = max_height(adm_12km), length.out = 20))

## -----------------------------------------------------------------------------
t_ext = 1.5 # time of "true" extinction
r = 30      # rate of fossil occurrences
# simulate rate fossil occurrences of taxon 
f_occ = p3(r, from = min_time(adm_2km), to = t_ext)
hist(f_occ,
     xlab = "Time [Myr]",
     ylab = "# Fossils",
     main = "Fossil abundance",
     breaks = seq(from = min_time(adm_2km), to = max_time(adm_2km), length.out = 20))

## -----------------------------------------------------------------------------
highest_occ = f_occ |>                          # take fossil occ. in time domain
  time_to_strat(adm_2km, destructive = TRUE) |> # transform into stratigraphic domain, destroying fossils that coincide with hiatuses
  max(na.rm = TRUE)                             # find highest preserved fossil (destroyed fossils are NA)

## -----------------------------------------------------------------------------
h_true_ext = t_ext |>                           # stratigraphic position of "true" extinction
  time_to_strat(adm_2km, destructive = FALSE) 

# distance between last occurrence of taxon and location where they actually go extinct
strat_range_offset_m = h_true_ext - highest_occ

## -----------------------------------------------------------------------------
t_last_occ = highest_occ |> # time when last preserved fossil lived
  strat_to_time(adm_2km)
# time offset between true extinction and time when last fossil lived.
time_range_offset_myr = t_ext - t_last_occ

## -----------------------------------------------------------------------------
t = scenarioA$t_myr           # time steps of the model
wd = scenarioA$wd_m[,"2km"]   # water depth 2 km offshore at model time steps
gc = approxfun(t, wd)         # define function that defines how the gradient changes with time (gc = *G*radient *C*hange)

plot(t, gc(t), 
     type = "l", 
     xlab = "Time [Myr]",
     ylab = "Water depth [m]",
     main = "Water depth 2 km offshore")

## -----------------------------------------------------------------------------
my_niche = snd_niche(opt = 10,       # preferred water depth 
                     tol = 5,        # tolerance to water depth fluctuations
                     cutoff_val = 0) # cut off negative values - the taxon does not survive on land
x = seq(-2, 30, by = 0.1)
plot(x, my_niche(x),
     type = "l",
     xlab = "Water depth [m]",
     ylab = "Collection probability",
     main = "Collection probability of taxon")

## -----------------------------------------------------------------------------
p3(rate = 300, from = min_time(adm_2km), to = max_time(adm_2km)) |> # model occurrences of taxon based on a constant rate
  apply_niche(niche_def = my_niche, gc = gc) |>                     # apply the niche model
  hist(xlab = "Time [Myr]",
       main = "Fossil abundance",
       ylab = "# Fossils",
       breaks = seq(from = min_time(adm_2km), to = max_time(adm_2km), length.out = 40))

## -----------------------------------------------------------------------------
p3(rate = 300, from = min_time(adm_2km), to = max_time(adm_2km)) |> # model occurrences based on constant rate
  apply_niche(niche_def = my_niche, gc = gc) |>                     # apply niche model
  time_to_strat(adm_2km, destructive = TRUE) |>                     # transform into strat. domain, destroy fossils that coincide with hiatuses 
  hist(xlab = "Stratigraphic height [m]",                           # plot results
       main = "Fossil abundance 2 km from shore",
       ylab = "# Fossils",
       breaks = seq(from = min_height(adm_2km), to = max_height(adm_2km), length.out = 40))


## -----------------------------------------------------------------------------
list("t" = t, "y" = wd) |>    # create list with time - water depth information
  time_to_strat(adm_2km) |>   # transform into the strat. domain
  plot(orientation = "lr",    # plot water depth information in the stratigraphic domain
       type = "l",
       xlab = "Stratigraphic position [m]",
       ylab = "Water depth [m]",
       main = "Water depth in section 2 km from shore")

## ----eval=FALSE---------------------------------------------------------------
#  vignette("phenotypic_evolution")

## ----eval=FALSE---------------------------------------------------------------
#  vignette("advanced_functionality")

