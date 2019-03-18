berechne_relative_einstrahlflaeche <- function(kippwinkel, einstrahl_hoehenwinkel, einstrahl_azimuthwinkel, rad = TRUE){
  
  if (rad == FALSE) kippwinkel <- wechsel_grad_zu_rad(kippwinkel)
  
  relativer_optimaler_kippwinkel <- pi/2 - einstrahl_hoehenwinkel - kippwinkel
  # 90 Grad + Einstrahlwinkel + "optimaler Ausrichtwinkel" ergeben stets 180 Grad
  # der relative optimale Kippwinkel berücksichtigt zusätzlich die Kippung
  
  relativer_optimaler_azimuthwinkel <- einstrahl_azimuthwinkel - pi
  # der Azimuthwinkel wird ausgehend von einem Kreismit 0 Richtung Norden angegeben. 
  # um den Richtwert "0" im Süden zu haben, wird der Winkel um 180° gedreht
  # Eine Einstellung des Azimuthwinkels ist nicht vorgesehen,
  # es wird von einer Standardausrichtugn nach Süden ausgegangen
  
  abs(cos(relativer_optimaler_kippwinkel) * cos(relativer_optimaler_azimuthwinkel))
}
