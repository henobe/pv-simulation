library(tidyverse)

calculateEfficiency <- function(alpha, beta){
  x <- cos(alpha)
  y <- cos(beta)
  
  x*y
}

calculateEfficiency(0.345, 0.3554)

# https://www.sunearthtools.com/dp/tools/pos_sun.php?lang=de

solarWinkel <- read_delim('SunPath.csv', delim = ";", skip = 3)

# Elevation muss höher größer null sein -> filtern

ggplot(solarWinkel, aes(x = Azimuth, y = Elevation)) +
  geom_line(alpha = 0.5) +
  geom_point(size = 0.5) +
  geom_label(data = solarWinkel[seq(1, nrow(solarWinkel), 10),], aes(label = Stunde)) +
  scale_y_continuous(limits = c(0,90)) +
  scale_x_continuous(limits = c(0,360), breaks = c(0,90,180,270,360))
