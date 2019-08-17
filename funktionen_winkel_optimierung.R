# Funktionen zur Berechnung der optimalen Dreh- und Kippwinkel

berechne_strahlungsenergie_bei_panelwinkel <- function(x, sw, irr) {
  # INPUT: - x, Angles: Vektor mit Azimuth und Elevationwinkel in Grad
  #        - sw, Sonnenwinkel: Liste der Sonnenwinkel in kart. Koord
  #        - irr, Irridation: Vektor mit passenden Strahlungswerten
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
                  control = list(fnscale = -1)) # für Maximierung
  
  drehwinkel <- result$par
  names(drehwinkel) <- c("azimuth", "elevation")
  
  return(drehwinkel)
}


berechne_optimale_panelwinkel_gesamt <- function(start_date = now(), end_date = now() - days(1), 
                                                 position = c(53.6332, 9.9881), 
                                                 intervall_length = 10){
  # INPUT: start_date und end_date als POSIXct-Wert,
  #        postion als vector mit zwei Dezimalkoordinaten,
  #        intervall_length: Abstand zwischen zwei Messpunkten, in Minuten 
  # OUTPUT: Listenobjekt mit drei Elementen:
  #           winkel: Vektor mit den beiden optimalen Einstellwinkeln
  #           data: ein Dataframe mit den wichtigsten Berechnungsdaten zur weiteren Verwendung
  #           relative_gain: eine Kennzahl der prozentualen Verbesserung mit gekipptem Panel zu liegendem.
  
  df <- tibble(
    datetime = with_tz(as.POSIXct(generiere_zeitreihe(start_date, end_date, intervall_length)), "UTC"),
    coordinates = list(position)) %>%
    mutate(sonnen_winkel = vectorised_berechne_sonnenposition(datetime, position[1], position[2])) %>%
    mutate(zenith_angle = unname(pi - get_elevation(sonnen_winkel))) %>%
    mutate(winkel_kartesisch = vectorised_polar_zu_kartesisch(azimuth   = sapply(sonnen_winkel, `[[`, 1), 
                                                              elevation = sapply(sonnen_winkel, `[[`, 2))) %>%
    mutate(sonnen_strahlung = vectorised_berechne_sonnenstrahlung(datetime, position[1], position[2],
                                                                  zenith_angle = zenith_angle))
  
  optimale_winkel <- berechne_optimale_panelwinkel(df$winkel_kartesisch, df$sonnen_strahlung)  
  
  df <- df %>% mutate(eingefangene_strahlung = 
                        sonnen_strahlung * berechne_relative_einstrahlflaeche(
                          winkel_kartesisch, 
                          berechne_normalenvektor_panel(drehwinkel = optimale_winkel[1], 
                                                        kippwinkel = optimale_winkel[2],
                                                        radiant = FALSE)))
  
  df$eingefangene_strahlung[df$eingefangene_strahlung < 0] <- 0  # negative values are not useful
  
  gain = ((sum(df$eingefangene_strahlung) / sum(df$sonnen_strahlung)) - 1) * 100
  
  return(list(winkel = optimale_winkel, 
              data = df, 
              relative_gain = gain))
}
