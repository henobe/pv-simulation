
berechne_optimale_panelwinkel_fast <- function(sonnenwinkel,
                                               strahlungsenergie_pro_flaeche) {
  # INPUT: - Sonnenwinkel: Liste der Sonnenwinkel in kart. Koord
  #        - Strahlungsenergie: Vektor mit passenden Strahlungswerten
  # OUTPUT: Optimaler Elevation Winkel
  
  gesamte_strahlungsenergie <- function(elevation,
                                        azimuth,
                                        sw,
                                        irr) {
    
    eingefangene_strahlung <- map2_dbl(sw,
                                       irr,
                                       berechne_strahlungsenergie_bei_panelwinkel,
                                       elevation = elevation,
                                       azimuth = 0)
    
    sum(eingefangene_strahlung)
  }
  
  result <- optimise(gesamte_strahlungsenergie,
                     interval = c(-90, 90),
                     tol = 0.05,
                     maximum = TRUE,
                     sw = sonnenwinkel,
                     irr = strahlungsenergie_pro_flaeche)
  
  c("elevation" = result[[1]], 
    "azimuth" = 0)
}



berechne_optimale_panelwinkel_gesamt_fast <- function(start_date = now(),
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
  t1 <- Sys.time()
  time_zone <- tz_lookup_coords(lat = lat,
                                lon = lon,
                                method = "fast",
                                warn = FALSE)
  t2 <- Sys.time()
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
  t3 <- Sys.time()
  optimale_winkel <- berechne_optimale_panelwinkel_fast(df$winkel_kartesisch,
                                                        df$sonnen_strahlung)  
  t4 <- Sys.time()
  df <- mutate(df,
               eingefangene_strahlung = map2_dbl(
                 winkel_kartesisch,
                 sonnen_strahlung,
                 berechne_strahlungsenergie_bei_panelwinkel,
                 elevation = optimale_winkel["elevation"],
                 azimuth = optimale_winkel["azimuth"])
  )
  t5 <- Sys.time()
  gain <- (sum(df$eingefangene_strahlung) / sum(df$sonnen_strahlung) - 1) * 100
  
  timestamps <- c("init" = t1,
                  "tz_lookup" = t2,
                  "gen_sun_data" = t3,
                  "optim_angle" = t4,
                  "calc_solar_on_ground" = t5)
  
  list(winkel = optimale_winkel,
       data = df,
       relative_gain = gain,
       timestamps = timestamps)
}
