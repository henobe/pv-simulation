# Testen der Funktion zur Berechnung des Normalenvektors des Solarpanels
library(testthat)
source('funktionen_winkel_solarpanel.R')


test_that("Standardwert ist flach liegend", {
  expect_equal(berechne_normalenvektor_panel(), list(0,0,-1))
})

test_that("Drehung bei flach liegend ist irrelevant", {
  expect_equal(berechne_normalenvektor_panel(1, 0), list(0,0,-1))
  expect_equal(berechne_normalenvektor_panel(2, 0), list(0,0,-1))
  expect_equal(berechne_normalenvektor_panel(3, 0), list(0,0,-1))
})

test_that("Normalenvektor ist Drehwinkel bei 90-Kippung", {
  expect_equal(berechne_normalenvektor_panel(1, pi), polar_zu_kartesisch(1,0))
  expect_equal(berechne_normalenvektor_panel(2, pi), polar_zu_kartesisch(2,0))
  expect_equal(berechne_normalenvektor_panel(3, pi), polar_zu_kartesisch(3,0))
})

