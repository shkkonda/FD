####
# Grid map tutorial
# http://flowingdata.com/URL
#####

source("lib/coords2continent.R")

library(prevR)

#
# Cartesian Coordinates
#

# One symbol
x <- 1
y <- 1
symbols(1, 1, circles=1)

# Row of symbols
x1 <- c(1, 2, 3, 4, 5)
y1 <- c(1, 1, 1, 1, 1)
symbols(x1, y1, circles=c(1,1,1,1,1))
symbols(x1, y1, circles=rep(0.5, length(x1)), inches=FALSE)

# Two rows of symbols
x2 <- c(x1, x1)
y2 <- c(y1, y1 + 1)
plot(x2, y2, xlim=c(0,6), ylim=c(0,3), type="n", asp=1)
symbols(x2, y2, circles=rep(0.5, length(x2)), inches=FALSE, add=TRUE)

# Three rows
x3 <- c(x1, x1, x1)
y3 <- c(y1, y1 + 1, y1 - 1)
plot(x3, y3, xlim=c(0,6), ylim=c(0,3), type="n", asp=1)
symbols(x3, y3, circles=rep(0.5, length(x3)), inches=FALSE, add=TRUE)

# A bunch of longer rows
x1 <- 1:50
x50 <- rep(x1, 50)
y50.temp <- rep(1:50, 50)
y50 <- y50.temp[order(y50.temp)]
plot(x50, y50, xlim=c(0,50), ylim=c(0,50), type="n", asp=1)
symbols(x50, y50, circles=rep(0.5, length(x50)), inches=FALSE, add=TRUE, bg="#cccccc", fg=NA)



#
# Geographic Coordinates, naive spacing
#

# Ocean and lake shapefiles
oceans <- readShapePoly("lib/data/ne_110m_ocean/ne_110m_ocean.shp", repair=TRUE)
lakes <- readShapePoly("lib/data/ne_10m_lakes/ne_10m_lakes.shp", repair=TRUE)

# Bounding box for United Kingdom
westlim <- -10.39	# Longitude
eastlim <- 2.76
southlim <- 50.05	# Latitude
northlim <- 61.01

# Create a rectangular grid
lnggrid <- seq(westlim, eastlim, 0.2)
latgrid <- seq(southlim, northlim, 0.2)
gridcells <- expand.grid(lnggrid, latgrid)
colnames(gridcells) <- c("lng", "lat")

# Subset to areas only in given country
gridcells$country <- coords2country(gridcells)
ccells <- subset(gridcells, country=="United Kingdom")

# Subset out ocean and lakes
ccells$isocean <- point.in.SpatialPolygons(ccells$lng, ccells$lat, oceans)
ccells$islake <- point.in.SpatialPolygons(ccells$lng, ccells$lat, lakes)
land <- subset(ccells, isocean == FALSE & islake == FALSE)[,1:2]

# Plot the grid cells
lngpercell <- (range(land$lng)[2] - range(land$lng)[1]) / length(lnggrid)
latpercell <- (range(land$lat)[2] - range(land$lat)[1]) / length(latgrid)
gradius <- min(lngpercell, latpercell) / 2
plot(land, xlim=range(land$lng), ylim=range(land$lat), type="n", asp=1)
symbols(land, circles=rep(gradius, dim(land)[1]), inches=FALSE, add=TRUE, bg="#cccccc", fg=NA)

# Draw map with default projection
map("world", "uk", xlim=range(land$lng), ylim=range(land$lat))
symbols(land, circles=rep(gradius, dim(land)[1]), inches=FALSE, add=TRUE, bg="#cccccc", fg=NA)

# Change projection
map("world", "uk", proj="albers", par=c(43,62), xlim=range(land$lng), ylim=range(land$lat))
pts <- mapproject(land$lng, land$lat)
xpercell <- (range(pts$x)[2] - range(pts$x)[1]) / length(lnggrid)
ypercell <- (range(pts$y)[2] - range(pts$y)[1]) / length(latgrid)
gradius <- min(xpercell, ypercell) / 2
symbols(pts, circles=rep(gradius, dim(land)[1]), inches=FALSE, add=TRUE, bg="#cccccc", fg=NA)


#
# Geographic coordinates, even out the grid
#

clims <- c(westlim, eastlim, southlim, northlim)
mileincr <- 10
countryname <- "United Kingdom"

earthradius <- 3960.0
degrees2radians <- pi/180.0
radians2degrees <- 180/pi

# Latitude increments, uniform west to east	
latincr <- mileincr/earthradius*radians2degrees	
latpts <- seq(clims[3], clims[4], by=latincr)
	
# Longitude increments, depend on latitude	
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

# Subset points in country
gridcells$country <- coords2country(gridcells)
ccells <- subset(gridcells, country==countryname)

# Filter out lake and ocean
ccells$isocean <- point.in.SpatialPolygons(ccells$lng, ccells$lat, oceans)
ccells$islake <- point.in.SpatialPolygons(ccells$lng, ccells$lat, lakes)
land <- subset(ccells, isocean == FALSE & islake == FALSE)[,1:2]

# Map the points
map("world", "uk", proj="albers", par=c(43,62), xlim=range(land$lng), ylim=range(land$lat))
pts <- mapproject(land$lng, land$lat)
xpercell <- (range(pts$x)[2] - range(pts$x)[1]) / length(lngpts)
ypercell <- (range(pts$y)[2] - range(pts$y)[1]) / length(latpts)
gradius <- ypercell / 2
symbols(pts, circles=rep(gradius, dim(land)[1]), inches=FALSE, add=TRUE, bg="#cccccc", fg=NA)



#
# Now just regular mapping
#

source("lib/countrygrids.R")
clims <- c(-125.16, -66.05, 24.77, 49.33)	# United States of America
usgrid <- getgrid(clims, 20, "United States of America")

par(mar=c(0,0,0,0))
map("state", proj="albers", par=c(43,62), xlim=range(clims[1:2]), ylim=range(clims[3:4]))
pts <- mapproject(usgrid$lng, usgrid$lat)

randomcols <- c("purple", "cyan", "gray", "brown", "orange")
ptcols <- sapply(usgrid$lng, function(x) {
			fullrange <- max(usgrid$lng) - min(usgrid$lng)
			prop <- (-67.28516 - x) / fullrange
			index <- max(1, round(prop * length(randomcols)))
			return(randomcols[index])
	})
ptcols <- unlist(ptcols)

gradius <- 0.0015
symbols(pts, circles=rep(gradius, dim(usgrid)[1]), inches=FALSE, add=TRUE, bg=ptcols, fg=NA)


# Draw just the grid
usgrid <- getgrid(clims, 50, "United States of America")
map("state", proj="albers", par=c(43,62), xlim=range(clims[1:2]), ylim=range(clims[3:4]), col="#e0e0e0", lwd=0.5)
pts <- mapproject(usgrid$lng, usgrid$lat)
points(pts, pch=8, cex=0.5, col="#821122")