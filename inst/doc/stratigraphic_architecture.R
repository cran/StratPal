## ----setup--------------------------------------------------------------------
library(StratPal)

## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
# ?scenarioA

## ----echo=FALSE, fig.cap="Production profile of the three carbonate factories", out.width="70%"----
knitr::include_graphics("platform_production_curve.png")

## ----echo=FALSE, out.width="70%"----------------------------------------------
plot(x = scenarioA$t_myr, 
     y = scenarioA$sl_m,
     xlab = "Time [Myr]",
     ylab = "Sea level [m]",
     main = "Eustatic Sea Level",
     type = "l")

## ----echo=FALSE, out.width='100%', fig.cap="Transect through the carbonate platform. Black lines separate systems tracts."----
knitr::include_graphics("platform_profile.png")

## ----echo=FALSE, out.width='100%', fig.cap="Wheeler diagram of the platform, showing both dominant facies and seidimentation rate."----
knitr::include_graphics("platform_wheeler_diagram.png")

