test_that("returns correct number of events", {
  n = 10
  x = runif(n, 0, 1)
  # reduces number of elements for intermediate preservation
  expect_lt(length(apply_taphonomy(x, pres_potential = function(x) 0.5, ctc =  function(x) 1)), n)
  # returns identical data for perfect preservation
  pres_potential = function(x) rep(1, length(x))
  ctc = function(x) rep(1, length(x))
  expect_equal(length(apply_taphonomy(x, pres_potential, ctc)), n)
  # all events are destroyed for pres. potential of 0
  pres_potential = function(x) rep(0, length(x))
  ctc = function(x) rep(0, length(x))
  expect_equal(length(apply_taphonomy(x, pres_potential, ctc)), 0)
})

test_that("pre_paleoTS object remains unchanged for perfect preservation", {
  x = stasis_sl(1:3)
  pres_potential = function(x) rep(1, length(x))
  ctc = function(x) rep(1, length(x))
  expect_identical(apply_taphonomy(x, pres_potential, ctc), x)
})


test_that("works with stratigraphic data", {
  x = stasis_sl(1:5)
  x$h = x$t
  x$t = NULL
  class(x) = c("pre_paleoTS", "stratlist", "list")
  pres_potential = function(x) rep(1, length(x))
  ctc = function(x) rep(1, length(x))
  expect_identical(apply_taphonomy(x, pres_potential, ctc), x)
})

test_that("fossils object remains identical for perfect preservation",{
  pres_potential = function(x) rep(1, length(x))
  ctc = function(x) rep(1, length(x))
  t = ape::rlineage(birth = 1, death = 0, Tmax = 1)
  rate = 2
  f = FossilSim::sim.fossils.poisson(rate = rate, tree = t)
  expect_equal(apply_taphonomy(f, pres_potential, ctc), f)
})
