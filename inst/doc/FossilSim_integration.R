## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(StratPal)
library(admtools)

## ----fig.alt="Age-depth model and water depth 12 km offshore (proximal slope)"----
# construct age-depth model
adm_12km = tp_to_adm(t = scenarioA$t_myr,   # 12 km from shore
                h = scenarioA$h_m[,"12km"],
                T_unit = "Myr",
                L_unit = "m")
plot(adm_12km, lty_destr = 0,
     main = "Age-depth model 12 km from shore",
     lwd_destr = 3)
T_axis_lab()
L_axis_lab()

# water depth 12 km from shore
t = scenarioA$t_myr
wd = scenarioA$wd_m[,"12km"]
gc = approxfun(t, wd)
plot(t, gc(t),
     xlab = "Time [Myr]",
     ylab = "Water depth [m]",
     type = "l",
     lwd = 3)

## ----fig.alt="Phylogenetic tree with sampled fossils"-------------------------
set.seed(42)
# simulate phylogenetic tree
tree = ape::rbdtree(birth = 3, death = 1, Tmax = max_time(adm_12km))
# simulate taxonomy along the tree
s = FossilSim::sim.taxonomy(tree = tree)
# simulate fossils based on taxonomy
sampling_rate = 40 # high sampling rate
f = FossilSim::sim.fossils.poisson(rate = sampling_rate, taxonomy = s)
# plot tree with taxonomy and fossil times
FossilSim:::plot.fossils(f, tree = tree, taxonomy = s, show.taxonomy = TRUE, rho = 0)

## ----fig.alt="Niche of taxa and collection probability with time"-------------
# define niche
my_niche = snd_niche(opt = 60, tol = 40, cutoff_val = 0)

# plot niche
wd_p = seq(-2, 100, by = 1)
plot(x = wd_p,
     y = my_niche(wd_p),
     xlab = "Water depth [m]",
     ylab = "Collection probability",
     type = "l",
     main = "Collection probability with water depth",
     lwd = 3)

plot(x = get_T_tp(adm_12km),
     y = adm_12km |> get_T_tp() |> gc() |> my_niche(),
     type = "l",
     xlab = "Time [Myr]",
     ylab = "Collection probability",
     main = "Collection probability with time",
     lwd = 3)

## ----fig.alt="Tree with fossils after accounting for niche preference"--------
f_niche = f |>
  rev_dir(ref = max_time(adm_12km)) |>
  apply_niche(niche_def = my_niche, gc = gc) |>
  rev_dir(ref = max_time(adm_12km))
# plot tree after accounting for taxon niches
FossilSim:::plot.fossils(f_niche, tree, taxonomy = s, show.taxonomy = TRUE, rho = 0)

## ----fig.alt="Tree with fossils in the stratigraphic domain"------------------
## transform tree, fossils, and taxonomy into stratigraphyc domain
# using the age-depth model
tree_strat = time_to_strat(tree, adm_12km) # no transformation of time to age required
s_strat = s |>  # taxonomy object in the time domain
  rev_dir(ref = max_time(adm_12km)) |> # convert age to time
  time_to_strat( adm_12km , destructive = FALSE) |> # transform using age-depth model
  rev_dir(ref = max_height(adm_12km)) # transform back into age
f_strat =  f |> # same here
  rev_dir(ref = max_time(adm_12km )) |>
  time_to_strat( adm_12km , destructive = TRUE)|> # destroy fossils coinciding with gaps
  rev_dir(ref = max_height(adm_12km ))

FossilSim:::plot.fossils(f_strat, 
                         tree = tree_strat,
                         taxonomy = s_strat,
                         show.taxonomy = TRUE, 
                         rho = 0,
                         show.axis = FALSE)
axis(1)
mtext(1, text = "Stratigraphic position [m]", line = 2)

## -----------------------------------------------------------------------------
## transform tree, fossils, and taxonomy into stratigraphyc domain
# using the age-depth model
tree_strat = time_to_strat(tree, adm_12km) # no transformation of time to age required
s_strat = s |>  # taxonomy object in the time domain
  rev_dir(ref = max_time(adm_12km)) |> # convert age to time
  time_to_strat( adm_12km , destructive = FALSE) |> # transform using age-depth model
  rev_dir(ref = max_height(adm_12km)) # transform back into age
f_strat =  f |> # same here
  rev_dir(ref = max_time(adm_12km )) |>
  apply_niche(niche_def = my_niche, gc = gc) |> # apply niche model 
  time_to_strat( adm_12km , destructive = TRUE)|> # destroy fossils coinciding with gaps
  rev_dir(ref = max_height(adm_12km ))

FossilSim:::plot.fossils(f_strat,
                         tree = tree_strat,
                         taxonomy = s_strat,
                         show.taxonomy = TRUE, 
                         rho = 0,
                         show.axis = FALSE)

axis(1)
mtext(1, text = "Stratigraphic position [m]", line = 2)

## ----eval=FALSE---------------------------------------------------------------
# vignette("paleotree", package = "FossilSim")

