# Testen der Funktion zur Berechnung des Normalenvektors des Solarpanels
library(testthat)
source('funktionen_winkel_solarpanel.R')
source('funktionen_helper_winkel.R') # für Funktionen innerhalb der Funktion


test_that("Datenformat ist korrekt", {
  expect_true(is.vector(berechne_normalenvektor_panel()))
  expect_true(is.numeric(berechne_normalenvektor_panel()))
  expect_equal(length(berechne_normalenvektor_panel()), 3)
  expect_equal(names(berechne_normalenvektor_panel()), c("north", "east", "down"))
})


test_that("Standardwert ist flach liegend", {
  expect_equal(unname(berechne_normalenvektor_panel()), c(0,0,-1))
            # 'unname' damit die Vektoren verglichen werden koennen
})


test_that("Drehung bei flach liegend ist irrelevant", {
  expect_equal(unname(berechne_normalenvektor_panel(1, 0)), c(0,0,-1))
  expect_equal(unname(berechne_normalenvektor_panel(2, 0)), c(0,0,-1))
  expect_equal(unname(berechne_normalenvektor_panel(3, 0)), c(0,0,-1))
})


test_that("Normalenvektor ist Drehwinkel + 180 Grad bei 90-Kippung", {
  expect_equal(unname(berechne_normalenvektor_panel(0, pi/2)), c(-1,0,0))
  expect_equal(unname(berechne_normalenvektor_panel(pi/2, pi/2)), c(0,-1,0))
  expect_equal(unname(berechne_normalenvektor_panel(pi, pi/2)), c(1,0,0))
  expect_equal(unname(berechne_normalenvektor_panel(3/2*pi, pi/2)), c(0,1,0))
})


test_that("Normalenvektor ist mathematisch korrekt", {
  expect_equal(berechne_normalenvektor_panel(1,  0.1), polar_zu_kartesisch(pi + 1, pi - 0.1))
  expect_equal(berechne_normalenvektor_panel(1, -0.1), polar_zu_kartesisch(pi + 1, pi + 0.1))
  expect_equal(berechne_normalenvektor_panel(-0.5,  0.2), polar_zu_kartesisch(pi - 0.5, pi - 0.2))
  expect_equal(berechne_normalenvektor_panel(-0.5, -0.2), polar_zu_kartesisch(pi - 0.5, pi + 0.2))
})


test_that("Umrechnung bei Benutzung von 'Grad' funktioniert", {
  expect_equal(berechne_normalenvektor_panel(rad_zu_grad(1), rad_zu_grad(0.1), radiant = FALSE),
               polar_zu_kartesisch(pi + 1, pi - 0.1))
  expect_equal(berechne_normalenvektor_panel(rad_zu_grad(1), rad_zu_grad(-0.1), radiant = FALSE), 
               polar_zu_kartesisch(pi + 1, pi + 0.1))
  expect_equal(berechne_normalenvektor_panel(rad_zu_grad(-0.5), rad_zu_grad(0.2), radiant = FALSE), 
               polar_zu_kartesisch(pi - 0.5, pi - 0.2))
  expect_equal(berechne_normalenvektor_panel(rad_zu_grad(-0.5), rad_zu_grad(-0.2), radiant = FALSE), 
               polar_zu_kartesisch(pi - 0.5, pi + 0.2))
})


test_that("Output ist korrekt bei vektorisiertem Input", {
  drehwinkel <- c(1,1,-0.5, -0.5)
  kippwinkel <- c(0.1,-0.1,0.2, -0.2)
  
  erwartete_koords <- list(polar_zu_kartesisch(pi + 1, pi - 0.1),
                           polar_zu_kartesisch(pi + 1, pi - - 0.1),
                           polar_zu_kartesisch(pi - 0.5, pi - 0.2),
                           polar_zu_kartesisch(pi - 0.5, pi - -0.2))
  
  expect_equal(berechne_normalenvektor_panel(drehwinkel, kippwinkel),
               erwartete_koords)
})
