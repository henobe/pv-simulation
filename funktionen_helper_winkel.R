# Hilfsfunktionen für die Rechnung mit Winkeln und Vektoren

grad_zu_rad <- function(value) {
  value * pi / 180
}


rad_zu_grad <- function(x) {
  x * 180 / pi
}


polar_zu_kartesisch <- function(azimuth, elevation, length = 1) {
  # INPUT:  3D-Polarkoordinaten, in Radiant
  # OUTPUT: Liste kartesischer Koordinaten, (NED-System)
  #           n --> "Norden", also azimuth = 0, elevation = 90
  #           e --> "Osten", also azimuth = 90, elevation = 90
  #           d --> "Unten", also azimuth egal und elevation = 0
  
  n <- length * cos(azimuth) * sin(elevation)
  e <- length * sin(azimuth) * sin(elevation)
  d <- length *                cos(elevation)
  
  rlang::set_names(c(n, e, d), c("north", "east", "down"))
}


skalarprodukt <- function(vector_a, vector_b) {
  # Input: Zwei Vektoren/Listen, bestehend aus 3 Elementen
  # Output: Skalarprodukt als Vektor (len=1)
  
  # Skalarprodukt im kart. Raum: sp = a1*b1 + a2*b2 + a3*b3
  
  vector_a <- unlist(vector_a)
  vector_b <- unlist(vector_b)
  
  drop(vector_a %*% vector_b) # Vektor anstatt Matrix
}
