visualisiere_steigerung <- function(proz_steigerung){
  steigerung <- proz_steigerung
  
  df <- tibble(
    lage = as_factor(c("a", "b", "c")),
    ertrag = c(100, steigerung, 150-steigerung)
  )
  
  plot <- ggplot(df, aes(x = 1, y = rev(ertrag), fill = lage)) +
    scale_fill_manual(values = c("white", "#f4c430", "grey50")) +
    geom_col() +
    geom_text(aes(x = 1, y = 10, label = paste("Prozentuale Steigerung\nvon", round(steigerung, digits = 2), "%")), size = 8, hjust = 0) +
    coord_flip() +
    theme(axis.line=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(),
          axis.ticks=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position="none",
          plot.background=element_blank(), panel.background=element_blank(),panel.border=element_blank(),
          panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
          aspect.ratio = 0.15)
  
  

  return(plot)  
}


visualisiere_ausrichtung <- function(azimuth, elevation){
  azimuth_normed <- abs(azimuth)
  elevation_normed <- abs(elevation)
  
  labeller_compass <- function(azimuth, elevation){
    if(azimuth > 0) {
      if(elevation < 0) return(c("N", "W", "S", "E"))
      else return(c("S", "E", "N", "W"))
    } else {
      if(elevation < 0) return(c("N", "E", "S", "W"))
      else return(c("S", "W", "N", "E"))
    }
  }
  
  labeller_richtung <- function(azimuth, elevation){
    if(azimuth > 0) {
      if(elevation < 0) return("W")
      else return("E")
    } else {
      if(elevation < 0) return("E")
      else return("W")
    }
  }
  
  plot_drehung <- ggplot(mapping = aes(x = c(0, 0, azimuth_normed), y = c(0, 1, 1))) +
    xlab(NULL) + ylab(NULL) +
    geom_polygon(colour = "black", fill = "grey") +
    geom_text(aes(x = c(180), y = 0.5, 
                  label = paste("Optimaler Drehwinkel:\n", 
                                round(azimuth_normed, digits = 1), "° ", 
                                labeller_richtung(azimuth, elevation),
                                sep = "")), 
              size = 8) +
    scale_x_continuous(limits = c(0, 360), 
                       breaks = c(0, 90, 180, 270), 
                       labels = labeller_compass(azimuth, elevation)) +
    coord_polar(start = ifelse(elevation < 0, 0, pi), 
                direction = ifelse(azimuth < 0, 1, -1)) +
    theme(axis.text.y=element_blank(), axis.ticks=element_blank(), axis.text=element_text(size=20))
  
  plot_kippung <- ggplot(mapping = aes(x = c(0, 0, elevation_normed), y = c(0, 1, 1))) +
    xlab(NULL) + ylab(NULL) +
    geom_polygon(colour = "black", fill = "grey") +
    geom_text(aes(x = 270, y = 0.5, 
                  label = paste("Optimaler Kippwinkel:\n", 
                                round(elevation_normed, digits = 1), "°", 
                                sep = "")), 
              size = 8) +
    scale_x_continuous(limits = c(0, 360), 
                       breaks = c(0, 90),
                       labels = function(x) paste(x, "°", sep = "")) +
    coord_polar(direction = -1, start = 3*pi/2) +
    theme(axis.text.y=element_blank(), axis.ticks=element_blank(), axis.text=element_text(size=20))  
  
  return(multiplot(plot_kippung, plot_drehung, cols = 2))
}


visualisiere_kippung <- function(elevation){
  elevation_normed <- abs(elevation)
  
  plot_kippung <- ggplot(mapping = aes(x = c(0, 0, elevation_normed), y = c(0, 1, 1))) +
    xlab(NULL) + ylab(NULL) +
    geom_polygon(colour = "black", fill = "grey") +
    geom_text(aes(x = 270, y = 0.5, 
                  label = paste("Optimaler Kippwinkel:\n", 
                                round(elevation_normed, digits = 1), "°", 
                                sep = "")), 
              size = 8) +
    scale_x_continuous(limits = c(0, 360), 
                       breaks = c(0, 90),
                       labels = function(x) paste(x, "°", sep = "")) +
    coord_polar(direction = -1, start = 3*pi/2) +
    theme(axis.text.y=element_blank(), axis.ticks=element_blank(), axis.text=element_text(size=20))  
  
  return(plot_kippung)
}


visualisiere_steigerung_text <- function(steigerung){
  plot_steigerung <- ggplot(mapping = aes(x = 1, y = 1)) +
    geom_text(aes(label = paste("Strahlungsgewinn\nvon", round(steigerung, digits = 2), "%")), size = 8) +
    scale_y_discrete() +
    scale_x_discrete() +
    theme(axis.line=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(),
          axis.ticks=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position="none",
          plot.background=element_blank(), panel.background=element_blank(),panel.border=element_blank(),
          panel.grid.major=element_blank(), panel.grid.minor=element_blank())
  
  return(plot_steigerung)
}

visualisiere_kippung_steigerung <- function(elevation, steigerung) {
  plot_kippung <- visualisiere_kippung(elevation)
  plot_steigerung <- visualisiere_steigerung_text(steigerung)
  
  return(multiplot(plot_kippung, plot_steigerung, cols = 2))
}

mapWorld <- borders("world", colour="grey", fill="gray50") # create a layer of borders

visualisiere_koordinaten <- function(latitude, longitude){
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


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  # copied from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
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
