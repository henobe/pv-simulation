# Verschiedene Funktionen zu Winkelrechnungen des Solarpanels

berechne_normalenvektor_panel <- function(drehwinkel=0, 
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
  #   Süden ausgerichtet, hat also einen Drehwinkel von 0°!.
  # kippwinkel entspricht Kippung des Panels zum Boden:
  #   also 0° -> flach auf Boden, 90° -> senkrecht auf Boden
  
  if(!radiant){
    drehwinkel <- grad_zu_rad(drehwinkel)
    kippwinkel <- grad_zu_rad(kippwinkel)
  }
  
  # Dadurch dass der Drehwinkel bereits nach Definition "vom
  # Panel wegzeigt", also um 180° gedreht ist,
  # muss der Normalenvektor im NED-System nur um 180° gedreht werden.
  normalenvektor_azimuth = pi + drehwinkel
  
  # Der Höhenwinkel ist aufgrund der verschiedenen Definitionen bzw.
  # Bezugssysteme noch von 180° abzuziehen. Der Normalenvektor
  # steht daraufhin mit 90° vom Panel ab.
  normalenvektor_elevation = pi - kippwinkel

  if(length(drehwinkel) > 1){
    return(vectorised_polar_zu_kartesisch(azimuth = normalenvektor_azimuth,
                               elevation = normalenvektor_elevation,
                               length = 1))
  } else {
    return(polar_zu_kartesisch(azimuth = normalenvektor_azimuth,
                               elevation = normalenvektor_elevation,
                               length = 1))
  }
}
