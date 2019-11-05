# Berechnung der Sonneneinstrahlung

# Sources for calculatoin method: 
# - Kharseh, M. 2015. Solar Radiation Calculation.

# DISCLAIMER: The calculations are currently only optimised for
#             northern hemisphere, mid-latitude locations

berechne_direkte_sonnenstrahlung <- function(when = Sys.time(), 
                                             lat = 53.57840, long = 9.94080,
                                             zenith_angle,
                                             seasonal_accuracy = TRUE){
  # Input: when: Zeitpunkt als POSIXct
  #        lat: Breitengrad des Ortes
  #        long: Laengengrad des Ortes
  #        zenith_angle: OPTIONAL um Doppelberechnung zu vermeiden
  # Output: Direktstrahlung auf liegende Flaeche
  #          Einheit: Watt/m^2
  
  if(is.character(when)) when <- strptime(when, format)
  when <- with_tz(when, "UTC")
  
  if(missing(zenith_angle)){
    pos <- berechne_sonnenposition(when, lat, long)
    zenith_angle <- unname(pi - pos[2])
  }
  
  et_radiation <- calcualte_theoretical_radiation(when)
  
  et_radiation_on_tangent <- calculate_theoretical_radiation_on_tangent(et_radiation, zenith_angle)

  season <- get_season(when)
  
  transmittance <- calculate_transmittance(season, zenith_angle, seasonal_accuracy)
  
  pmax(transmittance * et_radiation_on_tangent, 0)
}

vectorised_berechne_sonnenstrahlung <- Vectorize(berechne_direkte_sonnenstrahlung)


# Berechnungsfunktionen -----------------------------

calcualte_theoretical_radiation <- function(when){
  #solar_constant = 1367 # W/m^2
  1367 * (1 + 0.033*cos(2*pi*yday(when)/365))
}

calculate_theoretical_radiation_on_tangent <- function(theoretical_radiation, zenith_angle){
  # zenith angle in radians!
  unname(theoretical_radiation * cos(zenith_angle))
}

calculate_transmittance <- function(season, zenith_angle, seasonal_accuracy){
  # Beam Radiation on Ground Surface Clear day
  # Definining Constans
  
  if(seasonal_accuracy){
  r_0 <- c(0.97, 1.03)
  names(r_0) <- c("summer", "winter")
  
  r_1 <- c(0.99, 1.01)
  names(r_1) <- c("summer", "winter")
  
  r_k <- c(1.02, 1.00)
  names(r_k) <- c("summer", "winter")
  } else {
    r_0 <- c(1)
    names(r_0) <- c(season)
    
    r_1 <- c(1)
    names(r_1) <- c(season)
    
    r_k <- c(1.01)
    names(r_k) <- c(season)
  }
  
  A <- 2 # unsure about concrete definition of "A", optimised value for actual data Hamburg
  
  a_0 <- r_0[season] * (0.4237 - 0.00821 * (6.0 - A)^2)
  a_1 <- r_1[season] * (0.5055 + 0.00595 * (6.5 - A)^2)
  k   <- r_k[season] * (0.2711 + 0.01858 * (2.5 - A)^2)
  
  unname(a_0 + a_1 * exp(-k/cos(zenith_angle)))
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
