# Verschiedene Funktionen zu Winkelrechnungen des Solanpanels

berechne_normalenvektor_zur_sonne <- function(kippwinkel=0, diffsuedwinkel=0){
  elevation <- wechsel_grad_zu_rad(90 - kippwinkel)
  azimuth <- wechsel_grad_zu_rad(180 + diffsuedwinkel)
}


berechne_panelwinkel_in_skalar <- function(kippwinkel = 0){
  
  transpose((wechsel_polar_zu_kartesisch(pi, wechsel_grad_zu_rad(180-kippwinkel))))
  
  
  #transpose((wechsel_polar_zu_kartesisch(wechsel_grad_zu_rad(180 + diffsuedwinkel), wechsel_grad_zu_rad(kippwinkel-90))))
  
  #data <- ((wechsel_polar_zu_kartesisch(wechsel_grad_zu_rad(180 + diffsuedwinkel), wechsel_grad_zu_rad(kippwinkel))))
  
  #data <- lapply(data, function(x) -x)
  
  #transpose(data)
}
