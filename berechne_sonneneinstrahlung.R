# Berechnung der Sonneneinstrahlung

# Sources for calculatoin method: 
# - Kharseh, M. 2015. Solar Radiation Calculation.

# DISCLAIMER: The calculations are currently only optimised for
#             northern hemisphere, mid-latitude locations

berechne_direkte_sonnenstrahlung <- function(when = Sys.time(), 
                                             zenith_angle,
                                             lat = 53.57840,
                                             lon = 9.94080,
                                             seasonal_accuracy = TRUE) {
  # Input: when: Zeitpunkt als POSIXct
  #        lat: Breitengrad des Ortes
  #        long: Laengengrad des Ortes
  #        zenith_angle: OPTIONAL um Doppelberechnung zu vermeiden
  #        seasonal_accuracy: T/F-Wert, ob Sommer/Winter Anpasung
  # Output: Direktstrahlung auf liegende Flaeche
  #         Einheit: Watt/m^2
  
  if(is.character(when)) when <- strptime(when, format)
  when <- with_tz(when, "UTC")
  
  if(missing(zenith_angle)){
    pos <- berechne_sonnenposition(when, lat, lon)
    zenith_angle <- unname(pi - pos[2])
  }
  
  et_radiation <- calculate_theoretical_radiation(when)
  
  et_radiation_on_tangent <- calculate_theoretical_radiation_on_tangent(et_radiation, 
                                                                        zenith_angle)

  transmittance <- calculate_transmittance(zenith_angle, 
                                           switch(seasonal_accuracy + 1,
                                                  NULL,
                                                  when,))
  # switch statement is a work-around for ifelse
  # ifelse does not accept NULL as return value
  
  pmax(transmittance * et_radiation_on_tangent, 0)
}


# Berechnungsfunktionen -----------------------------

calculate_theoretical_radiation <- function(when){
  #solar_constant = 1367 # W/m^2
  1367 * (1 + 0.033 * cos(2 * pi * yday(when) / 365))
}

calculate_theoretical_radiation_on_tangent <- function(theoretical_radiation,
                                                       zenith_angle){
  # zenith angle in radians!
  unname(theoretical_radiation * cos(zenith_angle))
}


# constants for further functions:
r_0_map <- c(0.97, 1.03)
names(r_0_map) <- c("summer", "winter")

r_1_map <- c(0.99, 1.01)
names(r_1_map) <- c("summer", "winter")

r_k_map <- c(1.02, 1.00)
names(r_k_map) <- c("summer", "winter")

calculate_transmittance <- function(zenith_angle, when = NULL){
  # Beam Radiation on Ground Surface Clear day
  # Definining Constans
  
  r_0 <- c(1)
  r_1 <- c(1)
  r_k <- c(1.01)
  
  # overwrite standards if seasonal adjustments are wanted
  if(!is.null(when)){
    season <- get_season(when)
    
    r_0 <- r_0_map[season]
    r_1 <- r_1_map[season]
    r_k <- r_k_map[season]
  }
  
  # unsure about concrete definition of "A"
  # "2" is an optimal value compared to actual data from Hamburg
  A <- 2
  
  a_0 <- r_0 * (0.4237 - 0.00821 * (6.0 - A) ^ 2)
  a_1 <- r_1 * (0.5055 + 0.00595 * (6.5 - A) ^ 2)
  k   <- r_k * (0.2711 + 0.01858 * (2.5 - A) ^ 2)
  
  unname(a_0 + a_1 * exp(-k / cos(zenith_angle)))
}

get_season <- function(DATES) {
  #  WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2012-3-20",  format = "%Y-%m-%d") # Spring Equinox
  #  SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2012-9-22",  format = "%Y-%m-%d") # Fall Equinox
  
  # Convert dates from any year to 2012 dates, 2012 is leap year
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  
  ifelse(d >= SE & d < FE, "summer", "winter")
}
