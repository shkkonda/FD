source("library/densityHelper.R")

library(maps)
library(mapproj)
library(maptools)
library(geosphere)
library(splancs)
library(foreign)

#
# Standard coordinates
#

# Load and format data
bball <- read.csv("data/20100413.BOSCHI.csv", stringsAsFactors=FALSE)
shots <- subset(bball, etype == "shot" & !is.na(x) & !is.na(y))
shotsMade <- subset(shots, !is.na(points))
pts <- as.points(list(x=shotsMade$x, y=shotsMade$y))

# Plot just the points
plot(pts, asp=1, xlab="", ylab="", xlim=c(0,50), ylim=c(0,40), type="n", main="Shots made at Boston vs. Celtics, 4/13/10")
segments(seq(0, 60, by=5), rep(0, 12), seq(0, 60, by=5), rep(40, 12), lwd=0.1)
segments(rep(0, 8), seq(0, 40, by=5), rep(50, 8), seq(0, 40, by=5), lwd=0.1)
points(pts, pch=21, bg="#00000020")

# Color
pal <- colorRampPalette(c("white", "black", "#e6df06"))

# Smooth
poly <- as.points( list(x = c(0,0,50,50), y=c(0,40,40,0)) )
smoo <- kernel2d(pts, poly, h0=3.3, nx=200, ny=200)

# Draw it
par(mar=c(0,0,0,0))
plot(pts, asp=1, axes=FALSE, type="n", xlab="", ylab="", xlim=c(0,50), ylim=c(0,40))
image(smoo, add=TRUE, col=pal(300))

# Outside the lane
poly.outside <- as.points( list(x = c(0,0,50,50,31,31,19,19,0), y=c(0,40,40,0,0,19,19,0,0)) )
smoo.outside <- kernel2d(pts, poly.outside, h0=3.3, nx=200, ny=200)
plot(pts, asp=1, axes=FALSE, type="n", xlab="", ylab="", xlim=c(0,50), ylim=c(0,40))
image(smoo.outside, add=TRUE, col=pal(300))
lines(poly.outside)

# Various n
par(mfrow=c(2,2), mar=c(0,0,2,0))
ns <- c(10, 50, 100, 200)
for (i in 1:length(ns)) {
	
	smoo.outside <- kernel2d(pts, poly.outside, h0=3.3, nx=ns[i], ny=ns[i])
	plot(pts, asp=1, axes=FALSE, type="n", xlab="", ylab="", xlim=c(0,50), ylim=c(0,40), main=paste("nx = ny = ", ns[i], sep=""))
	image(smoo.outside, add=TRUE, col=pal(300))
	lines(poly.outside)
	
}

# Adding an NBA court outline for kicks and giggles.
drawCourt("brown")

# Various bandwidths
bandwidths <- c(0.5, 2, 3.5, 5)
par(mfrow=c(2,2), mar=c(0,0,2,0))
for (i in 1:length(bandwidths)) {
	
	smoo.outside <- kernel2d(pts, poly.outside, h0=bandwidths[i], nx=200, ny=200)
	plot(pts, asp=1, axes=FALSE, type="n", xlab="", ylab="", xlim=c(0,50), ylim=c(0,40), main=paste("Bandwidth: ", bandwidths[i], sep=""))
	image(smoo.outside, add=TRUE, col=pal(300))
	drawCourt("brown")
	
}




#
# Map projection
#

accidents <- read.dbf("data/FARS2012/accident.dbf")
accidents <- subset(accidents, LATITUDE != NULL & LONGITUD != NULL)

# Map just the points
map("usa", col="gray", bg=NA, lwd=0.4)
points(accidents$LONGITUD, accidents$LATITUDE, pch=21, cex=0.1, col="#00000020")

# Projected points
par(mar=c(0,0,0,0))
m.proj <- map("usa", proj="albers", param=c(39, 45), col="#000000", fill=FALSE, bg=NA, lwd=0.4)
loc.proj <- mapproject(accidents$LONGITUD, accidents$LATITUDE)
pts <- as.points(list(x=loc.proj$x, y=loc.proj$y))
points(pts, pch=21, cex=0.1, col="#00000020")

# Color palette
blackWhite <- colorRampPalette(c("white", "black"))

# Hack to use US border as polygon
m.proj <- map("usa", proj="albers", param=c(39, 45), col=NA, fill=FALSE, bg=NA, lwd=0.4)
xlim <- match(NA, m.proj[["x"]]) - 1
ylim <- match(NA, m.proj[["y"]]) - 1
uspoly.proj <- as.matrix(cbind(m.proj[["x"]][1:xlim], m.proj[["y"]][1:ylim]))

# Density
smoo.proj <- kernel2d(pts, uspoly.proj, h0=0.02, nx=500, ny=500)

# Draw map
image(smoo.proj, uspoly.proj, add=TRUE, col=blackWhite(100))
map("state", proj="albers", param=c(39, 45), col="#999999", fill=FALSE, bg=NA, lwd=0.2, add=TRUE, resolution=1)


### Various colors
par(mfrow=c(2,2), mar=c(0,0,0,0))

# Lower resolution
smoo.proj <- kernel2d(pts, uspoly.proj, h0=0.02, nx=150, ny=150)

# White purple black
map("usa", proj="albers", param=c(39, 45), col=NA, fill=FALSE, bg=NA, lwd=0.4)
purple <- colorRampPalette(c("white", "purple", "black"))
image(smoo.proj, uspoly.proj, add=TRUE, col=purple(100))

# Black blue white
map("usa", proj="albers", param=c(39, 45), col=NA, fill=FALSE, bg=NA, lwd=0.4)
blue <- colorRampPalette(c("black", "light blue", "blue", "white"))
image(smoo.proj, uspoly.proj, add=TRUE, col=blue(100))

# Red orange yellow
map("usa", proj="albers", param=c(39, 45), col=NA, fill=FALSE, bg=NA, lwd=0.4)
redOrange <- colorRampPalette(c("white", "black", "red", "orange", "#e6df06"))
image(smoo.proj, uspoly.proj, add=TRUE, col=redOrange(100))

# Reverse of above
map("usa", proj="albers", param=c(39, 45), col=NA, fill=FALSE, bg=NA, lwd=0.4)
redOrange.rev <- rev(redOrange(100))
image(smoo.proj, uspoly.proj, add=TRUE, col=redOrange.rev)



#
# Introduce other elements
#

# Washington bounding box
rg <- c(-125.31,45.36,-116.29,49.35)

# Highway shapefile
washington <- readShapeSpatial("data/tl_2012_53_prisecroads/tl_2012_53_prisecroads.shp", proj4string=CRS("+proj=longlat"))
plot(washington)

# Now plot Washington, the highways, and density
par(mar=c(0,0,0,0))
m.wash <- map("state", "Washington", fill=FALSE, col=NA)
stateNum <- 53
acc.wash <- subset(accidents, STATE==stateNum)
pts.wash <- as.points(list(x=acc.wash$LONGITUD, y=acc.wash$LATITUDE))

# A hack to use the Washington state border as the polygon
na.i <- which(is.na(m.wash$x))
start.i <- na.i[6] - 1
end.i <- na.i[5] + 1
start.i2 <- na.i[4] + 1
end.i2 <- na.i[5] - 1
poly.wash.x <- c(m.wash$x[start.i:end.i], m.wash$x[start.i2:end.i2])
poly.wash.y <- c(m.wash$y[start.i:end.i], m.wash$y[start.i2:end.i2])
poly.wash <- as.points( list(x = poly.wash.x, y = poly.wash.y ) )

# Find densities
smoo.wash <- kernel2d(pts.wash, poly.wash, h0=0.1, nx=350, ny=300)

# Draw it
image(smoo.wash, add=TRUE, col=redOrange(100))
plot(washington, lwd=0.2, add=TRUE, col="#E0E0C0")
lines(poly.wash)
