# Funktionen zur Berechnung der optimalen Dreh- und Kippwinkel

berechne_strahlungsenergie_bei_panelwinkel <- function(x, sw, irr) {
  # INPUT: - x: Vektor mit Azimuth und Elevationwinkel
  #        - Sonnenwinkel: Liste der Sonnenwinkel in kart. Koord
  #        - Strahlungsenergie: Vektor mit passenden Strahlungswerten
  # OUTPUT: Gesamte Strahlungsenergie (Skalar),
  #          die bei dieser Konfiguration aufgefangen worden wäre
  
  relative_flaeche <- berechne_relative_einstrahlflaeche(sw,
                                                         berechne_normalenvektor_panel(
                                                           drehwinkel = x[1],
                                                           kippwinkel = x[2],
                                                           radiant = FALSE
                                                         ))
  
  berechne_gesamte_strahlungsenergie(irr, relative_flaeche)
}

berechne_optimale_panelwinkel <- function(sonnenwinkel,
                                          strahlungsenergie_pro_flaeche,
                                          startwinkel = c(0,30)){
  # INPUT: - Sonnenwinkel: Liste der Sonnenwinkel in kart. Koord
  #        - Strahlungsenergie: Vektor mit passenden Strahlungswerten
  #        - Startwinkel: Start für den Optimierungsalgorithmus
  # OUTPUT: Optimaler Azimuth und Elevation Winkel
  #          Vektor, mit diesen beiden Elementen
  
  result <- optim(startwinkel,
                  berechne_strahlungsenergie_bei_panelwinkel,
                  sw = sonnenwinkel,
                  irr = strahlungsenergie_pro_flaeche,
                  control = list(fnscale = -1) # für Maximierung
  )
  
  drehwinkel <- result$par
  names(drehwinkel) <- c("azimuth", "elevation")
  
  return(drehwinkel)
}
