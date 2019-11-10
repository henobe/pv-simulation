# Verschiedene Funktionen zu Winkelrechnungen des Solarpanels

berechne_normalenvektor_panel <- function(drehwinkel = 0, 
                                          kippwinkel = 0, 
                                          radiant = TRUE) {
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
  
  if(!radiant) {
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

  purrr::map2(normalenvektor_azimuth, normalenvektor_elevation,
              polar_zu_kartesisch,
              length = 1)
}


berechne_relative_einstrahlflaeche <- function(einstrahlvektor_sonne,
                                               normalenvektor_panel = NULL){
  # INPUT: - Einstrahlvektor der Sonne, 
  #           def. vom Nullpunkt ZUR Sonne, Laenge 1
  #           auch als Liste dieser Vektoren
  #        - Normanlenvektor des Panels,
  #           als einfacher Vektor aus kart. Koord., Laenge 1,
  #           wenn "null" wird von Nachfuehrung ausgegangen.
  # OUTPUT: Die Einstrahlflaeche relativ zu einem liegenden Panel 
  #          als Vektor der gleichen Laenge wie einstrahlvektor_sonne
  
  # Das Skalarprodukt in kartesischen Raum:
  # Da beide Vektoren die Laenge 1 besitzen (wird hier nicht geprüft!),
  # ergibt das Skalarprodukt den Cosinus des Winkels, den die Vektoren aufspannen.
  # Dieser Wert verhält sich wie der Anteil an "aufgefangener" Sonnenstrahlung.
  
  vektor_liegendes_panel <- berechne_normalenvektor_panel()
  einstrahlung_flach <- skalarprodukt(einstrahlvektor_sonne,
                                      vektor_liegendes_panel)
  
  if(is.null(normalenvektor_panel)){
    einstrahlung_panel <- 1 # perfekte Nachfuehrung fängt 100% Strahlung auf
  } else {
    einstrahlung_panel <- skalarprodukt(einstrahlvektor_sonne,
                                        normalenvektor_panel)
  }
  einstrahlung_panel / einstrahlung_flach
}
