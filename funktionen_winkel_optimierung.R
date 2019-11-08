# Funktionen zur Berechnung der optimalen Dreh- und Kippwinkel

berechne_optimale_panelwinkel <- function(sonnenwinkel,
                                          strahlungsenergie_pro_flaeche) {
  # INPUT: - Sonnenwinkel: Liste der Sonnenwinkel in kart. Koord
  #        - Strahlungsenergie: Vektor mit passenden Strahlungswerten
  #        - Startwinkel: Start für den Optimierungsalgorithmus
  # OUTPUT: Optimaler Azimuth und Elevation Winkel
  #         Vektor, mit diesen beiden Elementen
  
  result <- optimise(berechne_strahlungsenergie_bei_panelwinkel,
                     interval = c(-90, 90),
                     tol = 0.01,
                     maximum = TRUE,
                     sw = sonnenwinkel,
                     irr = strahlungsenergie_pro_flaeche) # für Maximierung
  
  drehwinkel <- c(0, result[[1]])
  names(drehwinkel) <- c("azimuth", "elevation")
  
  return(drehwinkel)
}


get_optimised_intervall_length <- function(start_date, end_date) {
  days_label <- c("below 20", "20-30", "30-60", "60-90",
                  "90-120", "120-150", 
                  "above 150")
  days_number <- c(0, 20, 30, 60, 90, 120, 150, 9999)
  
   lookuptable <- tibble(
    number_days = days_label,
    intervall_length = c(10, 20, 30, 40, 50, 60, 90)  # minutes
  ) %>% mutate_at("number_days", factor)
  
  
  deltap <- cut(as.numeric(end_date - start_date), 
                breaks = days_number, labels = days_label)

  index <- match(deltap, lookuptable$number_days)
  
  return(lookuptable$intervall_length[index])
}


berechne_optimale_panelwinkel_gesamt <- function(start_date = now(),
                                                 end_date = now() + days(1),
                                                 position = c(53.6332, 9.9881), 
                                                 intervall_length = 10, 
                                                 simplify_return = FALSE){
  # INPUT: start_date und end_date als POSIXct-Wert,
  #        postion als vector mit zwei Dezimalkoordinaten,
  #        intervall_length: Abstand zwischen zwei Messpunkten, in Minuten 
  # OUTPUT: Listenobjekt mit drei Elementen:
  #         - winkel: Vektor mit den beiden optimalen Einstellwinkeln
  #         - data: ein Dataframe mit den wichtigsten Berechnungsdaten zur weiteren Verwendung
  #         - relative_gain: eine Kennzahl der prozentualen Verbesserung mit gekipptem Panel zu liegendem.
  
  time_zone <- tz_lookup_coords(lat = position[1],
                                lon = position[2],
                                method = "fast",
                                warn = FALSE)
  
  df <- tibble(
      datetime = force_tz(as.POSIXct(generiere_zeitreihe(start_date, 
                                                         end_date,
                                                         intervall_length)),
                          time_zone),
      coordinates = list(position)) %>%
    # filter missing data bei Zeitumstellung Winter auf Sommer
    filter(!is.na(datetime)) %>%
    mutate_at("datetime", with_tz, "UTC") %>%
    mutate(sonnen_winkel = purrr::map(datetime, berechne_sonnenposition,
                                      lat = position[1],
                                      long = position[2]),
           zenith_angle = get_zenith_angle(sonnen_winkel),
           winkel_kartesisch = purrr::map2(get_azimuth(sonnen_winkel),
                                           get_elevation(sonnen_winkel),
                                           polar_zu_kartesisch),
           sonnen_strahlung = purrr::pmap_dbl(list(when = datetime,
                                                   lat = position[1],
                                                   long = position[2],
                                                   zenith_angle = zenith_angle,
                                                   seasonal_accuracy = FALSE),
                                              berechne_direkte_sonnenstrahlung))
  
  optimale_winkel <- berechne_optimale_panelwinkel(df$winkel_kartesisch, df$sonnen_strahlung)  
  
  if(simplify_return) return(optimale_winkel["elevation"])
  
  df <- df %>%
    mutate(eingefangene_strahlung = 
             sonnen_strahlung * berechne_relative_einstrahlflaeche(
               winkel_kartesisch, 
               berechne_normalenvektor_panel(drehwinkel = optimale_winkel[1], 
                                             kippwinkel = optimale_winkel[2],
                                             radiant = FALSE))) %>%
    mutate_at("datetime", with_tz, time_zone)
  
  df$eingefangene_strahlung[df$eingefangene_strahlung < 0] <- 0  # negative values are not useful
  
  gain <- ((sum(df$eingefangene_strahlung) / sum(df$sonnen_strahlung)) - 1) * 100
  
  return(list(winkel = optimale_winkel, 
              data = df, 
              relative_gain = gain))
}
