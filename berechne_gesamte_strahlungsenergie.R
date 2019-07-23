# Berechne Gesamte Strahlnugsenergie

berechne_gesamte_strahlungsenergie <- function (strahlungsenergie_pro_flaeche, 
                                                relative_flaeche){
  # INPUT: relative Einstrahlflaeche, Einstrahlenergie pro Flaeche,
  #         beide Werte müssengleich lang sein
  # OUTPUT: Summe der gesamten Strahlungsenergie, 

  strahlungswerte <- (strahlungsenergie_pro_flaeche * relative_flaeche)
  
  sum(strahlungswerte[strahlungswerte>0]) # neg. Werte nicht beachten
}
