# Funktionen zur Berechnung der Strahlungsenergie auf einem Panel

berechne_gesamte_strahlungsenergie <- function(strahlungsenergie_pro_flaeche, 
                                               relative_flaeche) {
  # INPUT: relative Einstrahlflaeche, Einstrahlenergie pro Flaeche,
  #        beide Werte müssen gleich lang sein
  # OUTPUT: Summe der gesamten Strahlungsenergie, 
  
  strahlungswerte <- strahlungsenergie_pro_flaeche * relative_flaeche
  
  sum(strahlungswerte[strahlungswerte > 0]) # neg. Werte nicht beachten
}


berechne_strahlungsenergie_bei_panelwinkel <- function(elevation,
                                                       azimuth = 0,
                                                       sw,
                                                       irr) {
  # INPUT: - x, Angles: Vektor mit Azimuth und Elevationwinkel in Grad
  #        - sw, Sonnenwinkel: Liste der Sonnenwinkel in kart. Koord
  #        - irr, Irridation: Vektor mit passenden Strahlungswerten
  # OUTPUT: Gesamte Strahlungsenergie (Skalar),
  #         die bei dieser Konfiguration aufgefangen worden wäre
  
  relative_flaeche <- berechne_relative_einstrahlflaeche(
    einstrahlvektor_sonne = sw,
    normalenvektor_panel = berechne_normalenvektor_panel(kippwinkel = elevation,
                                                         drehwinkel = azimuth,
                                                         radiant = FALSE),
    nachfuehrung = FALSE
  )
  
  berechne_gesamte_strahlungsenergie(irr, relative_flaeche)
}
