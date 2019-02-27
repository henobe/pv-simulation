berechne_optimalen_winkel <- function(time, elevation, azimuth){
  # verschiedene elevation winkelvergleichen, besten nehmen
  
}

winkelsammlung <- c(0,50,60,70,80,90)

winkelsammlung_Rad <- winkelsammlung*pi/180

flaeche_a <- berechne_winkeleffizienz(winkelsammlung_Rad[[1]], solar_winkel$kippwinkel_rad, solar_winkel$AzimuthRad)
flaeche_b <- berechne_winkeleffizienz(winkelsammlung_Rad[[2]], solar_winkel$kippwinkel_rad, solar_winkel$AzimuthRad)
flaeche_c <- berechne_winkeleffizienz(winkelsammlung_Rad[[3]], solar_winkel$kippwinkel_rad, solar_winkel$AzimuthRad)
flaeche_d <- berechne_winkeleffizienz(winkelsammlung_Rad[[4]], solar_winkel$kippwinkel_rad, solar_winkel$AzimuthRad)
flaeche_e <- berechne_winkeleffizienz(winkelsammlung_Rad[[5]], solar_winkel$kippwinkel_rad, solar_winkel$AzimuthRad)
flaeche_f <- berechne_winkeleffizienz(winkelsammlung_Rad[[6]], solar_winkel$kippwinkel_rad, solar_winkel$AzimuthRad)

winkelvergleich <- tibble(
  time = solar_winkel$Stunde,
  a = flaeche_a,
  b = flaeche_b,
  c = flaeche_c,
  d = flaeche_d,
  e = flaeche_e,
  f = flaeche_f
) %>%
  mutate(time_diff_sec = time - time[[1]])
  

winkelvergleich_gathered <- gather(winkelvergleich, "winkel", "flaeche", -time, -time_diff_sec)

ggplot(winkelvergleich_gathered, aes(x = time, y = flaeche, colour = winkel)) +
  geom_line()

wert_a <- berechne_kurvenflaeche(winkelvergleich$time_diff_sec, winkelvergleich$a)
wert_b <- berechne_kurvenflaeche(winkelvergleich$time_diff_sec, winkelvergleich$b)
wert_c <- berechne_kurvenflaeche(winkelvergleich$time_diff_sec, winkelvergleich$c)
wert_d <- berechne_kurvenflaeche(winkelvergleich$time_diff_sec, winkelvergleich$d)
wert_e <- berechne_kurvenflaeche(winkelvergleich$time_diff_sec, winkelvergleich$e)
wert_f <- berechne_kurvenflaeche(winkelvergleich$time_diff_sec, winkelvergleich$f)

winkelwerte <- c(wert_a, wert_b, wert_c, wert_d, wert_e, wert_f)

ggplot(mapping = aes(x = c("a","b","c","d","e","f"), y = winkelwerte)) +
  geom_point()

# Plan des Ablaufes: --------------------------------------
# man hat elevation und azimuth zu bestimmten Zeitpunkten
# man kann einen elevation winkel voreinstellen

# daraus: Wie viel relative "Einstrahlfläche" man zu welchem Zeitpunkt hat
# man nimmt die gemessene Stahlungsenergie/m²_flach
# daraus: Wie viel Strahlungsenergie/m²_Solarpanel zur Verfügung steht
# man nimmt das Datenblatt des Solarpanels
# daraus: Wie viel Leistung aus Strahlungsenergie/m²_Solarpanel generiert wird
# daraus: Bilanzierung der produzierten Enegiermenge/m²_Solarpanel über einen Zeitraum
# --> bis hier nur ein einziger Kippwinkel

# daraus: Vergleich zu perfekter Nachführung sowie daraus: Auswahl des besten festen Winkels