berechne_kurvenflaeche <- function(x, y){
  n <- length(x)
  
  if (length(y) == 1){
    y <- rep(y, length.out = n)
  } else if (n != length(y)) {
    stop("Länge von x und y stimmen nicht überein.")
  }
  
  if (n<2){
    stop("Länge ist nicht lang genug zur Flächenberechnung")
  }

  area <- 0
  for (i in 1:(n-1)){
    part_area <- (x[[i+1]] - x[[i]]) * (y[[i]] + y[[i+1]]) / 2
    area <- area + part_area
  }
  area
}
