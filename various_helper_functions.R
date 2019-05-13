# Collection of small reusable functions

wechsel_grad_zu_rad <- function(value){
  value*pi/180
}

wechsel_rad_zu_grad <- function(x){
  x*180/pi
}

wechsel_polar_zu_kartesisch <- function(azimuth, elevation, length=1){
  # NED-System:
  # x zeigt nach Norden, also azimuth = 0, elevation = 90
  # y zeigt nach Osten, also azimuth = 90, elevation = 90
  # z zeigt nach "Unten", also azimuth egal und elevation = 0
  
  # check for correct input
  #error <- ifelse(0>azimuth | azimuth>(2*pi) | (-pi/2) > azimuth | azimuth > (pi/2), TRUE, FALSE)
  #if(error){
  #  stop("Input angle out of range")
  #}
  # --> doesn't work vectorised
  
  
  x <- length * sin(elevation) * cos(azimuth)
  y <- length * sin(elevation) * sin(azimuth)
  z <- length * cos(elevation)
  
  # Um die Geschwindigkeit und Zuverlässigkeit zu erhöhen, wird der Rundungsfehler um 0
  # minimiert, in dem ab einem Wert sehr nahe an mit 0 weitergerechnet wird.
  #x <- ifelse(x < 1e-15, 0, x)
  #y <- ifelse(y < 1e-15, 0, y)
  #z <- ifelse(z < 1e-15, 0, z)
  
  list(x, y, z)
}

berechne_skalarprodukt <- function(vector_a, vector_b){
  vector_a <- unlist(vector_a)
  vector_b <- unlist(vector_b)
  
  drop(vector_a %*% vector_b)
}

berechne_normalenvektor_zur_sonne <- function(kippwinkel=0, diffsuedwinkel=0){
  elevation <- wechsel_grad_zu_rad(90 - kippwinkel)
  azimuth <- wechsel_grad_zu_rad(180 + diffsuedwinkel)
  
  
}

gib_panelwinkel_in_skalar <- function(kippwinkel = 0){
  
  transpose((wechsel_polar_zu_kartesisch(pi, wechsel_grad_zu_rad(180-kippwinkel))))
  
  
  #transpose((wechsel_polar_zu_kartesisch(wechsel_grad_zu_rad(180 + diffsuedwinkel), wechsel_grad_zu_rad(kippwinkel-90))))
  
  #data <- ((wechsel_polar_zu_kartesisch(wechsel_grad_zu_rad(180 + diffsuedwinkel), wechsel_grad_zu_rad(kippwinkel))))
  
  #data <- lapply(data, function(x) -x)
  
  #transpose(data)
}
