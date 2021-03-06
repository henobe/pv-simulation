# Testen der Winkelfunktionen (helper functions) 
library(testthat)
source('funktionen_helper_winkel.R')


test_that("Angle conversion deg->rad", {
  expect_equal(grad_zu_rad(  0), 0)
  expect_equal(grad_zu_rad( 90), pi/2)
  expect_equal(grad_zu_rad(180), pi)
  expect_equal(grad_zu_rad(270), 3*pi/2)
  expect_equal(grad_zu_rad(360), 2*pi)
  expect_equal(grad_zu_rad(27), 3/20*pi)
  expect_equal(grad_zu_rad(-27), -3/20*pi)
  expect_equal(grad_zu_rad(167), 2.914699851)
})


test_that("Angle conversion rad->deg", {
  expect_equal(rad_zu_grad(  0), 0)
  expect_equal(rad_zu_grad(pi/2), 90)
  expect_equal(rad_zu_grad(pi), 180)
  expect_equal(rad_zu_grad(3/2*pi), 270)
  expect_equal(rad_zu_grad(2*pi), 360)
  expect_equal(rad_zu_grad(3/20*pi), 27)
  expect_equal(rad_zu_grad(-3/20*pi), -27)
  expect_equal(rad_zu_grad(2.914699851), 167)
})


test_that("Angle conversion polar->kartesian", {
  #Datenformat
  expect_true(is.vector(polar_zu_kartesisch(0,0)))
  expect_true(is.numeric(polar_zu_kartesisch(0,0)))
  expect_equal(length(polar_zu_kartesisch(0,0)), 3)
  expect_equal(names(polar_zu_kartesisch(0,0)), c("north", "east", "down"))
  
  # Keine Elevation -> keine "n" und "e" Koordinate
  expect_equal(unname(polar_zu_kartesisch(0, 0)), c(0,0,1))
  expect_equal(unname(polar_zu_kartesisch(pi/2, 0)), c(0,0,1))
  expect_equal(unname(polar_zu_kartesisch(-pi/2, 0)), c(0,0,1))
  
  # Flacher Kreis in Ebene
  expect_equal(unname(polar_zu_kartesisch(0, pi/2)), c(1,0,0))
  expect_equal(unname(polar_zu_kartesisch(pi/2, pi/2)), c(0,1,0))
  expect_equal(unname(polar_zu_kartesisch(pi, pi/2)), c(-1,0,0))
  expect_equal(unname(polar_zu_kartesisch(3/2*pi, pi/2)), c(0,-1,0))
  expect_equal(unname(polar_zu_kartesisch(2*pi, pi/2)), c(1,0,0))
  expect_equal(unname(polar_zu_kartesisch(-pi/2, pi/2)), c(0,-1,0))
  expect_equal(unname(polar_zu_kartesisch(1, pi/2)), c( 0.5403023059,0.8414709848,0))
  expect_equal(unname(polar_zu_kartesisch(2, pi/2)), c(-0.4161468365,0.9092974268,0))
  expect_equal(unname(polar_zu_kartesisch(3, pi/2)), c(-0.9899924966,0.1411200081,0))

  # Frei Winkel im Raum
  expect_equal(unname(polar_zu_kartesisch(0, 3)), c(0.1411200081, 0, -0.9899924966))
  expect_equal(unname(polar_zu_kartesisch(1, 2)), c(0.4912954964, 0.7651474012, -0.4161468365))
  expect_equal(unname(polar_zu_kartesisch(2, 1)), c(-0.3501754884, 0.7651474012, 0.5403023059))
  
  # Länge ungleich 1
  expect_equal(unname(polar_zu_kartesisch(0, pi/2, 2.5)), c(2.5,0,0))
  expect_equal(unname(polar_zu_kartesisch(0, 0, 3.71)), c(0,0,3.71))
  expect_equal(unname(polar_zu_kartesisch(1, 2, 1.5)), c(1.5*0.4912954964,1.5*0.7651474012,1.5*-0.4161468365))
})


test_that("Skalarprodukt ist korrekt", {
  vektor_a <- c(1,2,3)
  vektor_b <- c(2.4,3.1,4.7)
  vektor_c <- c(3,4,5)
  
  expect_true(is.vector(skalarprodukt(vektor_b, vektor_c)))
  expect_true(is.numeric(skalarprodukt(vektor_b, vektor_c)))
  
  expect_equal(skalarprodukt(vektor_a, vektor_b), 22.7)
  expect_equal(skalarprodukt(vektor_b, vektor_a), 22.7)
  expect_equal(skalarprodukt(vektor_a, vektor_c), 26)
  expect_equal(skalarprodukt(vektor_b, vektor_c), 43.1)
})
