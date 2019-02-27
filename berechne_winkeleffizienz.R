berechne_winkeleffizienz <- function(elevation_winkel, elevation, azimuth){
  
  new_el_winkel <- elevation - elevation_winkel
  
  x <- cos(new_el_winkel)
  y <- cos(azimuth)
  
  x*y
}
