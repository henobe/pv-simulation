pivot_irridation_data <- function(sim_data) {
  sim_data %>%
    pivot_longer(cols = c("eingefangene_strahlung",
                          "sonnen_strahlung",
                          "eingefangene_strahlung_nachgefuehrt",
                          "eingefangene_strahlung_hardangle")) %>%
    select(datetime, name, value) %>%
    mutate_at("name", factor,
              levels = c("eingefangene_strahlung_nachgefuehrt",
                         "eingefangene_strahlung",
                         "eingefangene_strahlung_hardangle",
                         "sonnen_strahlung"),
              labels = c("nachgeführt",
                         "optimal\nausgerichtet",
                         "eigener Winkel",
                         "flach liegend"))
}


visualisiere_kippung <- function(elevation){
  elevation_normed <- abs(elevation)
  
  ggplot(mapping = aes(x = c(0, 0, elevation_normed),
                       y = c(0, 1, 1))) +
    xlab(NULL) +
    ylab(NULL) +
    geom_polygon(colour = "black", fill = "grey") +
    geom_text(aes(x = 270, y = 0.5, 
                  label = paste0("Optimaler Kippwinkel:\n", 
                                 round(elevation_normed, digits = 1),
                                 "\U00B0")), 
              size = 8) +
    scale_x_continuous(limits = c(0, 360), 
                       breaks = c(0, 90),
                       labels = function(x) paste0(x, "\U00B0")) +
    coord_polar(direction = -1, start = 3 * pi / 2) +
    theme(axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_text(size = 20))  
}


visualisiere_gesamtertrag <- function(comp_data) {
  sum_data <- comp_data %>%
    group_by(name) %>%
    summarise(summe = sum(value))
  
  baseline <- sum_data$summe[sum_data$name == "flach liegend"]
  
  sum_data <- sum_data %>%
    mutate(summe = (summe / baseline - 1) * 100) %>%
    filter(name != "flach liegend")
  
  ggplot(sum_data, aes(x = name, y = summe, fill = name)) +
    geom_col() +
    coord_flip() +
    scale_y_continuous(labels = function(x) paste(x, "%")) +
    labs(x = NULL,
         y = NULL,
         title = "Unterschied zu flach liegend") +
    guides(fill = FALSE) +
    theme(plot.title = element_text(size = 15),
          axis.text.y = element_text(size = 15, angle = 45),
          axis.text.x = element_text(size = 12))
}


visualisiere_kippung_steigerung <- function(elevation, comp_data) {
  multiplot(visualisiere_kippung(elevation),
            visualisiere_gesamtertrag(comp_data),
            cols = 2)
}


visualisiere_ertrag <- function(plot_data) {
  ggplot(plot_data, aes(x = datetime, y = value, colour = name)) +
    geom_line(size = 1) +
    labs(x = "Zeitpunkt",
         y = "Strahlungsstärke [W/m^2]",
         colour = "Ausrichtung") +
    theme(text = element_text(size=20))
}

mapWorld <- borders("world", colour = "grey", fill = "gray50")

visualisiere_koordinaten <- function(latitude, longitude) {
  # INPUT: Zwei Geokoordinaten
  # OUTPUT: GGPLOT-Object das auf der Weltkarte, die Koordinaten darstellt
  ggplot() +  
    mapWorld +
    geom_point(aes(x = longitude, y = latitude), color = "red", size = 3) +
    labs(x = "L\U00E4ngengrad",
         y = "Breitengrad") +
    scale_x_continuous(breaks = c(-180, -90, 0, 90, 180)) +
    scale_y_continuous(breaks = c(-90, -45, 0, 45, 90)) +
    coord_fixed(1.3)
}


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  # copied from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots / cols)),
                     ncol = cols,
                     nrow = ceiling(numPlots / cols))
  }
  
  if (numPlots == 1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
