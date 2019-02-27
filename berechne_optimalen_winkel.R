berechne_optimalen_winkel <- function(time, elevation, azimuth){
  # verschiedene elevation winkelvergleichen, besten nehmen
  
}

winkelsammlung <- c(0,5,10,15,20,25)

winkelsammlung_Rad <- winkelsammlung*pi/180

flaeche_a <- berechne_winkeleffizienz(winkelsammlung_Rad[[1]], solar_winkel$ElevationRad, solar_winkel$AzimuthRad)
flaeche_b <- berechne_winkeleffizienz(winkelsammlung_Rad[[2]], solar_winkel$ElevationRad, solar_winkel$AzimuthRad)
flaeche_c <- berechne_winkeleffizienz(winkelsammlung_Rad[[3]], solar_winkel$ElevationRad, solar_winkel$AzimuthRad)
flaeche_d <- berechne_winkeleffizienz(winkelsammlung_Rad[[4]], solar_winkel$ElevationRad, solar_winkel$AzimuthRad)
flaeche_e <- berechne_winkeleffizienz(winkelsammlung_Rad[[5]], solar_winkel$ElevationRad, solar_winkel$AzimuthRad)
flaeche_f <- berechne_winkeleffizienz(winkelsammlung_Rad[[6]], solar_winkel$ElevationRad, solar_winkel$AzimuthRad)

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
  

winkelvergleich_gathered <- gather(winkelvergleich, "winkel", "flaeche", -time)

ggplot(winkelvergleich, aes(x = time, y = flaeche, colour = winkel)) +
  geom_line()

wert_a <- berechne_kurvenflaeche(winkelvergleich$time_diff_sec, winkelvergleich$a)
wert_b <- berechne_kurvenflaeche(winkelvergleich$time_diff_sec, winkelvergleich$b)
wert_c <- berechne_kurvenflaeche(winkelvergleich$time_diff_sec, winkelvergleich$c)
wert_d <- berechne_kurvenflaeche(winkelvergleich$time_diff_sec, winkelvergleich$d)
wert_e <- berechne_kurvenflaeche(winkelvergleich$time_diff_sec, winkelvergleich$e)
wert_f <- berechne_kurvenflaeche(winkelvergleich$time_diff_sec, winkelvergleich$f)

winkelwerte <- c(wert_a, wert_b, wert_c, wert_d, wert_e, wert_f)

ggplot(mapping = aes(x = 1, y = winkelwerte)) +
  geom_point()
