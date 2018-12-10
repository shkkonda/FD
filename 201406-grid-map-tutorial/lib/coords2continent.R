library(sp)
library(rworldmap)
library(rgdal)
library(maptools)

# The single argument to this function, points, is a data.frame in which:
#   - column 1 contains the longitude in degrees
#   - column 2 contains the latitude in degrees
coords2continent = function(points)
{  
  countriesSP <- getMap(resolution='low')
  #countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail

  # converting points to a SpatialPoints object
  # setting CRS directly to that from rworldmap
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  


  # use 'over' to get indices of the Polygons object containing each point 
  indices = over(pointsSP, countriesSP)

  #indices$continent   # returns the continent (6 continent model)
  continents <- as.vector(indices$REGION)   # returns the continent (7 continent model)
  if (length(continents) == 0) {
      return(FALSE)
  } else {
      return(continents)
  }
  #indices$ADMIN  #returns country name
  #indices$ISO3 # returns the ISO3 code 
}


# The single argument to this function, points, is a data.frame in which:
#   - column 1 contains the longitude in degrees
#   - column 2 contains the latitude in degrees
coords2country = function(points)
{  
  countriesSP <- getMap(resolution='low')
  #countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail

  # converting points to a SpatialPoints object
  # setting CRS directly to that from rworldmap
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  


  # use 'over' to get indices of the Polygons object containing each point 
  indices = over(pointsSP, countriesSP)

  
  countries <- as.vector(indices$ADMIN)  #returns country name
  if (length(countries) == 0) {
      return(FALSE)
  } else {
      return(countries)
  }
}