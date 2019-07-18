# Hilfsfunktionen für die Rechnung mit Winkeln und Vektoren

grad_zu_rad <- function(value){
  value*pi/180
}


rad_zu_grad <- function(x){
  x*180/pi
}


polar_zu_kartesisch <- function(azimuth, elevation, length=1){
  # INPUT:  3D-Polarkoordinaten, in Radiant
  # OUTPUT: Kartesische Koordinaten, NED (als Liste)
  
  # Funktionsweise NED-System:
  # x zeigt nach Norden, also azimuth = 0, elevation = 90
  # y zeigt nach Osten, also azimuth = 90, elevation = 90
  # z zeigt nach "Unten", also azimuth egal und elevation = 0
  
  x <- length * cos(azimuth) * sin(elevation)
  y <- length * sin(azimuth) * sin(elevation)
  z <- length *                cos(elevation)
  
  list(x, y, z)
}


skalarprodukt <- function(vector_a, vector_b){
  # Input: Zwei Vektoren/Listen, bestehend aus 3 Elementen
  # Output: Skalarprodukt als Vektor (len=1)
  
  # Skalarprodukt im kart. Raum: sp = a1*b1 + a2*b2 + a3*b3
  
  vector_a <- unlist(vector_a)
  vector_b <- unlist(vector_b)
  
  drop(vector_a %*% vector_b) # Vektor anstatt Matrix
}
