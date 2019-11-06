# Funktion zur Generierung einer gleichmäßig verteilten Reihe von Zeitstempeln

generiere_zeitreihe <- function(start_date, end_date, intervall_length) {
  # INPUT: start_date und end_date als POSIXct-Wert,
  #         intervall_length als integer, in Minuten 
  # OUTPUT: Vektor der Sequenz der Datetimes mit entsprechendem Abstand
  
  n_datetimes <- interval(start_date, end_date) / dminutes(intervall_length)
  timesequence <- round(seq(0, intervall_length * n_datetimes, length.out = n_datetimes + 1) * 60)
  return(start_date + seconds(timesequence))
}
