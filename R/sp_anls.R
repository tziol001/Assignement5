# Name: Nikoula, Latifah & Nikos
# Date: 8 January 2015
# Dependencies: sp, rgdal, rgeos

# The Function perform a simple spatial analysis for two input shapefiles.
## Particularly the user specify the type and select the width to create a buffer around them, then an intersection is performed with the buffer and the shpA. The output is a a plot that shows the buffer, the points, and the name of the city such as a text with some comments about the city

# load libraries
library(sp)
library(rgdal)
library(rgeos)

sp_anls<- function(shpA, shpB, type, width) { 
  shpA <- readOGR("data/url", shpA)
  shpB <- readOGR("data/url", shpB)
  
  # transform to the selected projection system
  shpA_proj<-spTransform(shpA, prj_string_RD)
  shpB_proj<-spTransform(shpB, prj_string_RD)
  
  # make a subset according to the type that is specified by the user, input should be within "".
  subset <- shpB_proj[shpB_proj$type == type, ]
  
  # create a buffer zone accordinf to the width that is specified by the user
  buffer<- gBuffer(subset, width= width, byid= TRUE)
  
  # make intersect
  a<- gIntersection(buffer,shpA_proj, byid=TRUE)
  b<- gIntersects(buffer,shpA_proj, byid=TRUE)
 
  # select the city name
  selected<-shpA_proj@data[b]
  
  #plot the final outputs
  plot(buffer,col='lightblue' )
  points(a, col="red", pch=22)
  mtext(side = 3, line = 1, paste(selected[2]), cex = 0.8)
  text<-print(paste("The name of the city is", selected[2], "and the population is equal to",selected[4])) 
} 