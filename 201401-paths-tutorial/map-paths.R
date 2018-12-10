# Load data
routes <- read.csv("runkeeper-routes-sf.csv", stringsAsFactors=FALSE)

# View data frame
routes[1:10,]

# Basic points plot
plot(routes$latitude, routes$longitude, type="n")
points(routes$latitude, routes$longitude, pch=20, cex=0.3, col="#000000")

# Basic lines plot
plot(routes$latitude, routes$longitude, type="n")
routeIds <- unique(routes$tempid)
for (i in 1:length(routeIds)) {
	currRoute <- subset(routes, tempid==routeIds[i])
	lines(currRoute$latitude, currRoute$longitude)
}

#
# Use map projections
#
library(mapproj)

# Albers projection
locProj <- mapproject(routes$latitude, routes$longitude, "albers", par=c(37, 37.5))
routes$latproj <- locProj$x
routes$lonproj <- locProj$y

# Map the projected points
plot(routes$latproj, routes$lonproj, type="n", asp=1, axes=FALSE, xlab="", ylab="")
for (i in 1:length(routeIds)) {
	currRoute <- subset(routes, tempid==routeIds[i])
	lines(currRoute$latproj, currRoute$lonproj)
}

# Azimuthal, for kicks and giggles
locProj <- mapproject(routes$latitude, routes$longitude, "azequidistant")	



#
# Aesthetics
#

# Rectangular
locProj <- mapproject(routes$latitude, routes$longitude, "rectangular", par=38)
routes$latproj <- locProj$x
routes$lonproj <- locProj$y

# With transparency and thinner lines
par(mar=c(0,0,0,0))
plot(routes$latproj, routes$lonproj, type="n", asp=1, axes=FALSE, xlab="", ylab="")
for (i in 1:length(routeIds)) {
	currRoute <- subset(routes, tempid==routeIds[i])
	lines(currRoute$latproj, currRoute$lonproj, col="#00000020", lwd=0.4)
}


# Varied line width, consistent 20 transparency
par(mfrow=c(2,3))
for (m in 0:5) {
	
	lineWidth <- 0.1 + 0.5 * m
	plot(routes$latproj, routes$lonproj, type="n", asp=1, axes=FALSE, xlab="", ylab="")
	for (i in 1:length(routeIds)) {
		currRoute <- subset(routes, tempid==routeIds[i])
		lines(currRoute$latproj, currRoute$lonproj, col="#00000020", lwd=lineWidth)
	}
}


# Consistent line width of 0.5, varied transparency
par(mfrow=c(2,3))
for (m in 1:6) {
	
	lineTrans <- round(m * 0.16 * 100)
	lineCol <- paste("#000000", lineTrans, sep="")
	plot(routes$latproj, routes$lonproj, type="n", asp=1, axes=FALSE, xlab="", ylab="")
	for (i in 1:length(routeIds)) {
		currRoute <- subset(routes, tempid==routeIds[i])
		lines(currRoute$latproj, currRoute$lonproj, col=lineCol, lwd=0.5)
	}
}


# Varied colors per map
par(mfrow=c(2,3))
mapCols <- c("#39e436", "#6e0ea9", "#d47627", "#0d5bc0", "#de0f07", "#9a5130")
for (m in 1:6) {
	lineCol <- paste(mapCols[m], 20, sep="")
	plot(routes$latproj, routes$lonproj, type="n", asp=1, axes=FALSE, xlab="", ylab="")
	for (i in 1:length(routeIds)) {
		currRoute <- subset(routes, tempid==routeIds[i])
		lines(currRoute$latproj, currRoute$lonproj, col=lineCol, lwd=0.5)
	}
}



# Varied colors per route
library(scales)		# For transparency
pickColor <- function() {
	
	# Pick a random color
	i <- sample(1:657, 1)
	theColor <- alpha(colors()[i], 0.3)
	return(theColor)
}

par(mar=c(0,0,0,0))
plot(routes$latproj, routes$lonproj, type="n", asp=1, axes=FALSE, xlab="", ylab="")
for (i in 1:length(routeIds)) {
	currRoute <- subset(routes, tempid==routeIds[i])
	lines(currRoute$latproj, currRoute$lonproj, col=pickColor(), lwd=0.5)
}




#
# Introduce base map
#

detailMap <- function(bbox, thedata) {
	basemap <- get_map(location=bbox, source='google', maptype="terrain", color="bw")
	ggmap(basemap) + geom_path(aes(x=longitude, y=latitude, group=tempid), size=0.3, color="#570864", alpha=0.3, data=thedata)
}
sanfran <- c(-122.50476977783954, 37.70528090348771, -122.3619475122155, 37.83825794027055)
detailMap(sanfran, routes)

