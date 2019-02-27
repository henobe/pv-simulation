berechne_winkeleffizienz <- function(elevation_winkel, elevation, azimuth){
  
  new_el_winkel <- elevation_winkel + elevation
  
  x <- 1/sin(new_el_winkel)
  y <- 1/cos(azimuth)
  
  x*y
}
