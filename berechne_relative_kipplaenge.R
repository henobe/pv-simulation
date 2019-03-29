berechne_relative_kipplaenge <- function(einstrahlwinkel, kippwinkel = (pi/2 - einstrahlwinkel)) {
  # ohne Input wird direkt die relative Länge bei optimaler Ausrichtung berechnet.
  
  if (einstrahlwinkel > (pi/2) || kippwinkel > (pi/2)) 
    warning("Make sure to use radiant, input is greater than right angle")
  
  sin(einstrahlwinkel + kippwinkel) / sin(einstrahlwinkel)
  # see documentation for reason behind calculation
}
