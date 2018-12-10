library(maptools)

#
# Draw a dot density map
# 


# California population data
acsClasses <- c("character", "character", "character", "numeric", "numeric")
calipop <- read.csv("data/ACS_12_5YR_B01003_with_ann.csv", stringsAsFactors=FALSE, colClasses=acsClasses)
calipop.sub <- calipop[,c(2,4)]

# California shapefile for Census tracts
catract.shp <- readShapePoly("data/tl_2012_06_tract/tl_2012_06_tract.shp", proj4string=CRS("+proj=longlat"))
capolys <- SpatialPolygonsDataFrame(catract.shp, data=as(catract.shp, "data.frame"))
head(capolys@data)

# Merge datasets
cadata <- merge(capolys@data, calipop.sub, by.x="GEOID", by.y="geoid2", sort=FALSE)
plotvar <- cadata$totalpop / 1000		# One dot per 1,000 people

# Generate random dots in polygons (e.g. census tracts)
cadots.rand <- dotsInPolys(capolys, as.integer(plotvar), f="random")

# Map boundaries
cacounties.shp <- readShapePoly("data/california/california_county_shape_file.shp", proj4string=CRS("+proj=longlat"))
cacounties <- SpatialPolygonsDataFrame(cacounties.shp, data=as(cacounties.shp, "data.frame"))

# Set zero margins in plot window, so that map fits
par(mar=c(0,0,0,0))
plot(cacounties, lwd=0.1)

# Add dots
plot(cadots.rand, add=T, pch=19, cex=0.1, col="#00880030")


#
# Try the same with a different state e.g. Nebraska
#

nepop <- read.csv("data/ACS_12_5YR_B01003-nebraska/ACS_12_5YR_B01003_with_ann.csv", stringsAsFactors=FALSE, colClasses=acsClasses)
nepop.sub <- nepop[,c(2,4)]

netract.shp <- readShapePoly("data/tl_2013_31_tract_nebraska/tl_2013_31_tract.shp",proj4string=CRS("+proj=longlat"))
nepolys <- SpatialPolygonsDataFrame(netract.shp, data=as(netract.shp, "data.frame"))
nedata <- merge(nepolys@data, nepop.sub, by.x="GEOID", by.y="geoid2", sort=FALSE)
nevar <- nedata$totalpop / 100

par(mar=c(0,0,0,0))
nedots.rand <- dotsInPolys(nepolys, as.integer(nevar), f="random")
plot(nepolys, lwd=0.1)
plot(nedots.rand, add=TRUE, pch=19, cex=0.1, col="#00880030")

# Plot a specific region
plot(nepolys, lwd=0.1, xlim=c(-96.187303,-95.847993), ylim=c(41.073844,41.388218))
plot(nedots.rand, add=TRUE, pch=19, cex=0.1, col="#008800")

# Grid of dots instead of random
nedots.reg <- dotsInPolys(nepolys, as.integer(nevar), f="regular")
plot(nepolys, lwd=0.1, xlim=c(-96.187303,-95.847993), ylim=c(41.073844,41.388218))
plot(nedots.reg, add=TRUE, pch=19, cex=0.3, col="#008800")

# Color options
randomCols <- colors()[sample(1:502, 12)]
par(mfrow=c(2,3), mar=c(0,0,1,0))
for (i in 1:6) {
	plot(nepolys, lwd=0.1, xlim=c(-96.187303,-95.847993), ylim=c(41.073844,41.388218), col=randomCols[i])
	plot(nedots.reg, add=TRUE, pch=19, cex=0.1, col=randomCols[6+i])
}



#
# Dot density by a category
#

nerace <- read.csv("data/ACS_12_5YR_B02001_race/race-nebraska-truncated.csv", stringsAsFactors=FALSE, colClasses=c("character", "numeric", "numeric", "numeric", "numeric"))
nedata <- merge(nepolys@data, nerace, by.x="GEOID", by.y="geoid2", sort=FALSE)

races <- c("white", "black", "amind", "asian")
dotCols <- c("#da6678", "#647eee", "#c29219", "#09900d")
par(mar=c(0,0,0,0))
plot(nepolys, lwd=0.1, xlim=c(-96.187303,-95.847993), ylim=c(41.073844,41.388218))
for (i in 1:length(races)) {
	nevar <- nedata[,races[i]] / 1
	nedots.race <- dotsInPolys(nepolys, as.integer(nevar), f="random")
	plot(nedots.race, add=TRUE, pch=19, cex=0.01, col="black")
}




