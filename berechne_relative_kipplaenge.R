berechne_relative_kipplaenge <- function(einstrahlwinkel, kippwinkel = (pi/2 - einstrahlwinkel)) {
  # ohne Input wird direkt die relative Länge bei optimaler Ausrichtung berechnet.
  # Winkel in Radiant
  # Das Lot der liegenden Fläche entspricht 90 Grad Winkel (nicht 0°)
  
  einstrahlwinkel <- ifelse((einstrahlwinkel %% pi) == 0, NaN, einstrahlwinkel)
  # Division mit 0 vermeiden
  
  abs(sin(einstrahlwinkel + kippwinkel) / sin(einstrahlwinkel))
  # see documentation for reason behind calculation
  # Der Betrag, weil Länge immer positiv ist (tritt auf bei Winkel > 180 oder <0)

  # Issues: 
  # - bei Mehrfachen von pi wird die Berechnung unzulässig ungenau
}
