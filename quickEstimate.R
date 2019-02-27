library(tidyverse)

gradToRad <- function(value){
  value/360*(2*pi)
}

diff180 <- function(value){
  value-180
}

calculateEfficiency <- function(alpha, beta){
  x <- 1/sin(alpha) # hier sinus, weil dieser Winkel aus Daten kommt
  y <- 1/cos(beta) # hier Cosinus, weil dieser Winkel anders ist
  
  x*y
}

# https://www.sunearthtools.com/dp/tools/pos_sun.php

solar_winkel <- read_delim('SunPath.csv', delim = ";", skip = 3) %>%
  filter(Elevation > 0) %>%
  mutate(time_diff_sec = Stunde - Stunde[[1]]) %>%
  #mutate_at(c("Azimuth"), diff180) %>%
  mutate(kippwinkel = 90 - Elevation) %>%
  mutate_at(c("Azimuth"), function(x) x-180) %>% # Lösung über anonyme Funktion, um auszuprobieren
  mutate(ElevationRad = gradToRad(Elevation)) %>%
  mutate(AzimuthRad = gradToRad(Azimuth)) %>%
  mutate(kippwinkel_rad = gradToRad(kippwinkel))
#  mutate(projectedArea = calculateEfficiency(ElevationRad, AzimuthRad))

ggplot(solar_winkel, aes(x = Azimuth, y = Elevation)) +
  geom_label(data = solar_winkel[seq(1, nrow(solar_winkel), 10),], aes(label = Stunde)) +
  geom_line(alpha = 0.5) +
  geom_point(size = 0.5) +
  scale_y_continuous(limits = c(0,90)) +
  scale_x_continuous(limits = c(-180,180), breaks = c(-180,-90,0,90,180))

#ggsave("sonnenstand.png", width = 10, height = 5, dpi = 200)

ggplot(solar_winkel, aes(x = Stunde, y = projectedArea)) +
  geom_line() +
  geom_hline(yintercept = 1) +
  labs(
    x = "Uhrzeit",
    y = "Flächenunterschied"
  ) +
  scale_y_continuous(limits = c(0,10))

#ggsave("einstrahlfläche.png", width = 10, height = 5, dpi = 200)

nachfuehr_wert <- berechne_kurvenflaeche(solar_winkel$time_diff_sec, solar_winkel$projectedArea)
liegen_wert <- berechne_kurvenflaeche(solar_winkel$time_diff_sec, 1)
# "Tagesbilanz", also Prozentualer Gesamtunterschied berechnen
# Optimierungsfunktion für besten Starren Winkel
# Strahlung mit "1 Watt"
