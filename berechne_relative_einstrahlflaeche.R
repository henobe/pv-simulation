berechne_relative_einstrahlflaeche <- function(einstrahl_hoehenwinkel, kipp_hoehenwinkel = 0, einstrahl_azimuthwinkel, kipp_azimuthwinkel = 0, radiant = TRUE, optimal = FALSE){
  # Kein Input der Kippwinkel -> Flachliegend
  # Flag auf Optimal -> Berechnung mit optimaler Nachführung
  
  if (radiant == FALSE) {
    einstrahl_hoehenwinkel <- wechsel_grad_zu_rad(einstrahl_hoehenwinkel)
    kipp_hoehenwinkel <- wechsel_grad_zu_rad(kipp_hoehenwinkel)
    einstrahl_azimuthwinkel <- wechsel_grad_zu_rad(einstrahl_azimuthwinkel)
    kipp_azimuthwinkel <- wechsel_grad_zu_rad(kipp_azimuthwinkel)
  }
  
  if (optimal) 
    {
    berechne_relative_kipplaenge(einstrahl_hoehenwinkel) * berechne_relative_kipplaenge(einstrahl_azimuthwinkel)
  } else {
    berechne_relative_kipplaenge(einstrahl_hoehenwinkel, kipp_hoehenwinkel) * berechne_relative_kipplaenge(einstrahl_azimuthwinkel, kipp_azimuthwinkel)
  }
}
