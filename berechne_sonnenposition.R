# Berechnung der Sonnenposition

# Code Adapted from Richie Cotton,
# Answer in Stackoverflow "Position of the sun given time of day, latitude and longitude"
# https://stackoverflow.com/questions/8708048/position-of-the-sun-given-time-of-day-latitude-and-longitude

# Sources for calculatoin method: 
# - Michalsky, J.J. 1988. The Astronomical Almanac's algorithm for
#      approximate solar position (1950-2050). Solar Energy. 40(3):227-235.
# - Michalsky, J.J. 1989. Errata. Solar Energy. 43(5):323.
# - Spencer, J.W. 1989. Comments on "The Astronomical Almanac's Algorithm
#     for Approximate Solar Position (1950-2050)". Solar Energy. 42(4):353.
# - Walraven, R. 1978. Calculating the position of the sun. Solar Energy. 20:393-397.

# Disclaimer: Michalsky’s calculations are limited to the period from 1950 to 2050
#             with uncertainty of greater than ± 0.01°

berechne_sonnenposition <- function(when = Sys.time(), lat = 53.57840, long = 9.94080){    
  # INPUT: when: Zeitpunkt als POSIXct
  #        lat: Breitengrad des Ortes
  #        long: Laengengrad des Ortes
  # OUTPUT: Position der Sonne im NED-System 
  #          Kugelkoordinaten radiant, Vektor len=2
  
  if(is.character(when)) when <- strptime(when, format)
  when <- lubridate::with_tz(when, "UTC")
  time <- astronomers_alamanc_time(when)
  hour <- hour_of_day(when)
  
  # Ecliptic coordinates  
  mnlong <- mean_longitude(time)   
  mnanom <- mean_anomaly(time)  
  eclong <- ecliptic_longitude(mnlong, mnanom)     
  oblqec <- ecliptic_obliquity(time)
  
  # Celestial coordinates
  ra <- right_ascension(oblqec, eclong)
  dec <- right_declination(oblqec, eclong)
  
  # Local coordinates
  gmst <- greenwhich_mean_sidereal(time, hour)  
  lmst <- local_mean_sidereal(gmst, long)
  
  # Hour angle
  ha <- hour_angle(lmst, ra)
  
  # Latitude to radians
  lat <- grad_zu_rad(lat)
  
  # Azimuth and elevation
  el <- calculate_elevation(lat, dec, ha)
  az <- calculate_azimuth(lat, dec, ha, el)

  position <- c(az, el + pi/2) # Anpassung an NED-System
  names(position) <- c("azimuth", "elevation")
  
  return(position)
}


# Hilfsfunktionen fuer den Umgang mit Input und Output -------------------------
get_azimuth <- function(x) {
  unname(sapply(x, `[[`, 1))
}

get_elevation <- function(x) {
  unname(sapply(x, `[[`, 2))
}

get_zenith_angle <- function(x) {
  unname(pi - sapply(x, `[[`, 2))
}


# Berechnugsfunktionen: -----------------------

astronomers_alamanc_time <- function(x){
  # Astronomer's almanach time is the number of 
  # days since (noon, 1 January 2000)
  origin <- as.POSIXct("2000-01-01 12:00:00")
  as.numeric(difftime(x, origin, units = "days"))
}

hour_of_day <- function(x){
  x <- as.POSIXlt(x)
  with(x, hour + min / 60 + sec / 3600)
}

mean_longitude <- function(time){
  (280.460 + 0.9856474 * time) %% 360
}

mean_anomaly <- function(time){
  grad_zu_rad((357.528 + 0.9856003 * time) %% 360)
}

ecliptic_longitude <- function(mnlong, mnanom){
  grad_zu_rad(
    (mnlong + 1.915 * sin(mnanom) + 0.020 * sin(2 * mnanom)) %% 360
  )
}

ecliptic_obliquity <- function(time){
  grad_zu_rad(23.439 - 0.0000004 * time)
}

right_ascension <- function(oblqec, eclong){
  num <- cos(oblqec) * sin(eclong)
  den <- cos(eclong)
  ra <- atan(num / den)
  ra[den < 0] <- ra[den < 0] + pi
  ra[den >= 0 & num < 0] <- ra[den >= 0 & num < 0] + 2 * pi 
  ra
}

right_declination <- function(oblqec, eclong){
  asin(sin(oblqec) * sin(eclong))
}

greenwhich_mean_sidereal <- function(time, hour){
  (6.697375 + 0.0657098242 * time + hour) %% 24
}

local_mean_sidereal <- function(gmst, long){
  grad_zu_rad(15 * ((gmst + long / 15) %% 24))
}

hour_angle <- function(lmst, ra){
  ((lmst - ra + pi) %% (2 * pi)) - pi
}

calculate_elevation <- function(lat, dec, ha){
  asin(sin(dec) * sin(lat) + cos(dec) * cos(lat) * cos(ha))
}

calculate_azimuth <- function(lat, dec, ha, el) {
  az <- asin(-cos(dec) * sin(ha) / cos(el))
  cosAzPos <- (0 <= sin(dec) - sin(el) * sin(lat))
  sinAzNeg <- (sin(az) < 0)
  az[cosAzPos & sinAzNeg] <- az[cosAzPos & sinAzNeg] + 2 * pi
  az[!cosAzPos] <- pi - az[!cosAzPos]
  az
}
