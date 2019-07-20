# Verschiedene Funktionen zu Winkelrechnungen des Solarpanels

berechne_normalenvektor_panel <- function(drehwinkel=pi, 
                                          kippwinkel=0, 
                                          radiant = TRUE){
  # INPUT: Kippwinkel und Drehwinkel eines Panels
  #         Standardmaessig in Radiant angegebenen
  #         drehwinkel bez. Norden, kippwinkel bez. Boden 
  # OUTPUT: Der Normalenvektor des beschriebenen Panels
  #           als kartesische Koordinaten, Länge = 1

  # drehwinkel entspricht azimuth, 0° -> Norden
  #   Dabei betrachtet man den Winkel von der gekippten Kante
  #   zur am Boden liegenden Kante. Ein Panel, gekippt und nach
  #   Süden ausgerichtet, hat also einen Drehwinkel von 180°.
  # kippwinkel entspricht Kippung des Panels zum Boden:
  #   also 0° -> flach auf Boden, 90° -> senkrecht auf Boden
  
  if(!radiant){
    drehwinkel <- grad_zu_rad(drehwinkel)
    kippwinkel <- grad_zu_rad(kippwinkel)
  }
  
  # Dadurch dass der Drehwinkel bereits nach Definition "vom
  # Panel wegezeigt", muss dieser nicht mehr verändert werden,
  # sondern ist identisch dem Azimuth des Normalenvektor
  normalenvektor_azimuth = drehwinkel
  
  # Der Höhenwinkel ist aufgrund der verschiedenen Definitionen bzw.
  # Bezugssysteme noch von 180° abzuziehen. Der Normalenvektor
  # steht daraufhin mit 90° vom Panel ab.
  normalenvektor_elevation = pi - kippwinkel

  (normalenvektor <- polar_zu_kartesisch(azimuth = normalenvektor_azimuth,
                                         elevation = normalenvektor_elevation))
}


berechne_panelwinkel_in_skalar <- function(kippwinkel = 0){
  
  transpose((polar_zu_kartesisch(pi, grad_zu_rad(180-kippwinkel))))
  
  #transpose((wechsel_polar_zu_kartesisch(wechsel_grad_zu_rad(180 + diffsuedwinkel), wechsel_grad_zu_rad(kippwinkel-90))))
  
  #data <- ((wechsel_polar_zu_kartesisch(wechsel_grad_zu_rad(180 + diffsuedwinkel), wechsel_grad_zu_rad(kippwinkel))))
  
  #data <- lapply(data, function(x) -x)
  
  #transpose(data)
}
