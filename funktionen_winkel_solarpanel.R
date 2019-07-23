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


berechne_relative_einstrahlflaeche <- function(einstrahlvektor_sonne, normalenvektor_panel, nachfuehrung = FALSE){
  # INPUT: - Einstrahlvektor der Sonne, 
  #           def. vom Nullpunkt ZUR Sonne,
  #           auch als Liste dieser Vektoren
  #        - Normanlenvektor des Panels,
  #           als einfacher Vektoraus kart. Koord.
  # OUTPUT: Die Einstrahlflaeche relativ zu einem liegenden Panel 
  #          als Vektor der gleichen Laenge wie einstrahlvektor 
  
  vektor_liegendes_panel <- berechne_normalenvektor_panel()
  
  # Erklaerung Skalarprodukt:
  # 
  skalare_referenz <- mapply(skalarprodukt, einstrahlvektor_sonne, list(vektor_liegendes_panel))
  
  if(!nachfuehrung){
    skalare_panel <- (mapply(skalarprodukt, einstrahlvektor_sonne, list(normalenvektor_panel)))
    return(skalare_panel/skalare_referenz)
  } else {
    return(1/skalare_referenz) # perfekte Nachfuehrung hat stets Skalarprodukt von "1"
  }
}
