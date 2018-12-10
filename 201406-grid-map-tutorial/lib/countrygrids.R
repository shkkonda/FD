source("lib/coords2continent.R")

library(prevR)

oceans <- readShapePoly("lib/data/ne_110m_ocean/ne_110m_ocean.shp", repair=TRUE)
lakes <- readShapePoly("lib/data/ne_10m_lakes/ne_10m_lakes.shp", repair=TRUE)

getgrid <- function(clims, mileincr, cname) {
	
	earthradius <- 3960.0
	degrees2radians <- pi/180.0
	radians2degrees <- 180/pi
	
	latincr <- mileincr/earthradius*radians2degrees
	
	latpts <- seq(clims[3], clims[4], by=latincr)
	
	lngpts <- sapply(latpts, function(lat) {
					r <- earthradius * cos(lat*degrees2radians)
					lng <- (mileincr/r)*radians2degrees
					lng
	})
	
	# Iterate south to north, calculating each set of west to east increments
	gridcells.lat <- c()
	gridcells.lng <- c()
	for (i in 1:length(latpts)) {
		currlngs <- seq(clims[1], clims[2], by=lngpts[i])
		gridcells.lng <- c(gridcells.lng, currlngs)
		gridcells.lat <- c(gridcells.lat, rep(latpts[i], length(currlngs)))
	}
	gridcells <- data.frame(lng=gridcells.lng, lat=gridcells.lat)
	gridcells$country <- coords2country(gridcells)
	
	ccells <- subset(gridcells, country==cname)
	
	ccells$isocean <- point.in.SpatialPolygons(ccells$lng, ccells$lat, oceans)
	ccells$islake <- point.in.SpatialPolygons(ccells$lng, ccells$lat, lakes)
	
	land <- subset(ccells, isocean == FALSE & islake == FALSE)[,1:2]
	return(land)
}
