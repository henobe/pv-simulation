mapWorld <- borders("world", colour="grey", fill="gray50") # create a layer of borders

visualisere_koordinaten <- function(latitude, longitude){
  # INPUT: Zwei Geokoordinaten
  # OUTPUT: GGPLOT-Object das auf der Weltkarte, die Koordinaten darstellt
  
  mp <- ggplot() +  
    mapWorld +
    geom_point(aes(x=longitude, y=latitude) ,color="red", size=3) +
#    geom_point(aes(x=longitude, y=latitude) ,color="blue", shape=1, size=7.5) +
#    geom_point(aes(x=longitude, y=latitude) ,color="blue", shape=1, size=12) +
    labs(
      x = "Längengrad",
      y = "Breitengrad"
    ) +
    scale_x_continuous(breaks = c(-180, -90, 0, 90, 180)) +
    scale_y_continuous(breaks = c(-90, -45, 0, 45, 90)) +
    coord_fixed(1.3)
}
