# Funktionen zur Berechnung der optimalen Dreh- und Kippwinkel

berechne_optimale_panelwinkel <- function(sonnenwinkel,
                                          strahlungsenergie_pro_flaeche) {
  # INPUT: - Sonnenwinkel: Liste der Sonnenwinkel in kart. Koord
  #        - Strahlungsenergie: Vektor mit passenden Strahlungswerten
  #        - Startwinkel: Start für den Optimierungsalgorithmus
  # OUTPUT: Optimaler Azimuth und Elevation Winkel
  #         Vektor, mit diesen beiden Elementen
  
  gesamte_strahlungsenergie <- function(elevation,
                                        azimuth,
                                        sw,
                                        irr) {
    
    eingefangene_strahlung <- map2_dbl(sw,
                                       irr,
                                       berechne_strahlungsenergie_bei_panelwinkel,
                                       elevation = elevation,
                                       azimuth = azimuth)
    
    sum(eingefangene_strahlung)
  }
  
  result <- optimise(gesamte_strahlungsenergie,
                     interval = c(-90, 90),
                     tol = 0.01,
                     maximum = TRUE,
                     azimuth = 0,
                     sw = sonnenwinkel,
                     irr = strahlungsenergie_pro_flaeche)
  
  rlang::set_names(c(result[[1]], 0), c("elevation", "azimuth"))
}


get_optimised_intervall_length <- function(start_date, end_date) {
  days_label <- c("below 20", "20-30", "30-60", "60-90",
                  "90-120", "120-150", "above 150")
  days_number <- c(0, 20, 30, 60, 90, 120, 150, 9999)
  
  lookuptable <- tibble(number_days = days_label,
                        # minutes:
                        intervall_length = c(10, 20, 30, 40, 50, 60, 90)) %>%
    mutate_at("number_days", factor)
  
  deltap <- cut(as.numeric(end_date - start_date), 
                breaks = days_number, labels = days_label)
  
  index <- match(deltap, lookuptable$number_days)
  
  lookuptable$intervall_length[index]
}


berechne_optimale_panelwinkel_gesamt <- function(start_date = now(),
                                                 end_date = now() + days(1),
                                                 lat = 53.6332,
                                                 lon = 9.9881,
                                                 intervall_length = 10) {
  # INPUT: start_date und end_date als POSIXct-Wert,
  #        postion als vector mit zwei Dezimalkoordinaten,
  #        intervall_length: Abstand zwischen zwei Messpunkten, in Minuten 
  # OUTPUT: Listenobjekt mit drei Elementen:
  #         - winkel: Vektor mit den beiden optimalen Einstellwinkeln
  #         - data: ein Dataframe mit den wichtigsten Berechnungsdaten
  #         - relative_gain: prozentualen Differenz gekippt zu liegend
  
  time_zone <- tz_lookup_coords(lat = lat,
                                lon = lon,
                                method = "fast",
                                warn = FALSE)

  df <- tibble(datetime = force_tz(generiere_zeitreihe(start_date,
                                                       end_date,
                                                       intervall_length),
                                   time_zone),
               coordinates = list(c(lat, lon))) %>%
    # filter missing data bei Zeitumstellung Winter auf Sommer
    filter(!is.na(datetime)) %>%
    mutate(dt_utc = with_tz(datetime, "UTC"),
           sonnen_winkel = purrr::map(dt_utc, berechne_sonnenposition,
                                      lat = lat,
                                      lon = lon),
           zenith_angle = get_zenith_angle(sonnen_winkel),
           winkel_kartesisch = purrr::map2(get_azimuth(sonnen_winkel),
                                           get_elevation(sonnen_winkel),
                                           polar_zu_kartesisch),
           sonnen_strahlung = berechne_direkte_sonnenstrahlung(
             when = dt_utc,
             lat = lat,
             lon = lon,
             zenith_angle = zenith_angle,
             seasonal_accuracy = FALSE)
           )
  
  optimale_winkel <- berechne_optimale_panelwinkel(df$winkel_kartesisch,
                                                   df$sonnen_strahlung)  
  
  df <- mutate(df,
               eingefangene_strahlung = map2_dbl(
                 winkel_kartesisch,
                 sonnen_strahlung,
                 berechne_strahlungsenergie_bei_panelwinkel,
                 elevation = optimale_winkel["elevation"],
                 azimuth = optimale_winkel["azimuth"]),
               eingefangene_strahlung_nachgefuehrt = map2_dbl(
                 winkel_kartesisch,
                 sonnen_strahlung,
                 berechne_strahlungsenergie_bei_panelwinkel,
                 elevation = 0,  # setting to 0 so "map"
                 azimuth = 0,    # uses correct arguments
                 tracking = TRUE)
               )
  
  gain <- (sum(df$eingefangene_strahlung) / sum(df$sonnen_strahlung) - 1) * 100
  
  list(winkel = optimale_winkel,
       data = df,
       relative_gain = gain)
}
