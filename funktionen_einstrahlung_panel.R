# Funktionen zur Berechnung der Strahlungsenergie auf einem Panel

berechne_strahlungsenergie_bei_panelwinkel <- function(elevation,
                                                       azimuth = 0,
                                                       sw,
                                                       irr,
                                                       tracking = FALSE) {
  # INPUT: - x, Angles: Vektor mit Azimuth und Elevationwinkel in Grad
  #        - sw, Sonnenwinkel: Liste der Sonnenwinkel in kart. Koord
  #        - irr, Irridation: Vektor mit passenden Strahlungswerten
  #        - tracking: does the panel adjust its angels optimally to the sun?
  # OUTPUT: Gesamte Strahlungsenergie (Skalar),
  #         die bei dieser Konfiguration aufgefangen worden wäre
  
  if (!tracking){
    normalenvektor_panel <- berechne_normalenvektor_panel(kippwinkel = elevation,
                                                          drehwinkel = azimuth,
                                                          radiant = FALSE)
    
    relative_flaeche <- berechne_relative_einstrahlflaeche(sw,
                                                           normalenvektor_panel)
  } else {
    relative_flaeche <- berechne_relative_einstrahlflaeche(sw)
  }
  if_else(relative_flaeche < 0, 0, irr * relative_flaeche)
}
