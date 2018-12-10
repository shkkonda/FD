# Load animation and maps package
library(animation)
library(maps)

# Directory to store animated GIFs
ani.options(outdir = getwd())


###
# Animated GIF example
###

saveGIF({
    brownian.motion(pch = 21, cex = 5, col = "red", bg = "yellow")
}, movie.name = "brownian_motion.gif", interval = 0.1, nmax = 30, 
    ani.width = 600, ani.height = 600)
    
###
# Naive Wal-Mart growth plot
###

saveGIF({
	
	# Load data
	walmarts <- read.csv("walmarts_geocoded.csv", sep=",")
	walmarts.sub <- walmarts[1:200,]	# Subset of Wal-Mart set
	
	# A plot for each location
	for (i in 1:length(walmarts.sub[,1])) {
		plot(walmarts.sub[1:i,6], walmarts.sub[1:i,5], xlim=c(-159.37, 18.46), ylim=c(-34.07, 64.86))
	}
	
}, movie.name = "walmart-plot-naive.gif", interval = 0.1, nmax = 100, ani.width = 625, ani.height = 400)



###
# Naive Wal-Mart growth map
###

saveGIF({
	
	# Load data
	walmarts <- read.csv("walmarts_geocoded.csv", sep=",")
	walmarts.sub <- walmarts[1:100,]	# Subset of Wal-Mart set
	
	# A plot for each location
	for (i in 1:length(walmarts.sub[,1])) {
		map("state", proj="albers", param=c(39, 45), lwd=1, col="#000000")
		points(mapproject(walmarts.sub[1:i,]$lng, walmarts.sub[1:i,]$lat), col=NA, bg="red", pch=21, cex=0.5)
	}
	
}, movie.name = "walmart-map-naive.gif", interval = 0.1, nmax = 50, ani.width = 625, ani.height = 400)


###
# Timed Wal-Mart growth map, different annual windows
###

saveGIF({
	
	# Load data
	walmarts <- read.csv("walmarts_geocoded.csv", sep=",")
	
	# Year counter
	currYear <- walmarts$year[1]
	timeIncrement <- 5 	# every 5 years
	# timeIncrement <- 1 	# annual
	maxYear <- currYear + timeIncrement
	
	
	# A plot for each location
	while (currYear < 2016) {
		walmarts.sub <- walmarts[walmarts$year < maxYear,]
		
		# Draw map
		map("state", proj="albers", param=c(39, 45), lwd=1, col="#e0e0e0")
		
		# Draw locations
		points(mapproject(walmarts.sub$lng, walmarts.sub$lat), col=NA, bg="#000000", pch=21, cex=0.3)
		
		currYear <- currYear + timeIncrement
		maxYear <- currYear + timeIncrement
	}
	
}, movie.name = "walmart-map-timed-bigger.gif", interval = 0.1, nmax = 1, ani.width = 625, ani.height = 500)



###
# Timed Wal-Mart growth map, with day windows
###

# Translate dates to days since first opening
dates <- as.Date(paste(walmarts$year, walmarts$month, walmarts$day, sep="-"))
startDate <- dates[1]
days <- dates - startDate
walmarts$days <- as.numeric(days)

# A little bit about the days
summary(walmarts$days)		
daySpan <- max(walmarts$days)	# Data spans 17,489 days.

# Create animation in same way as before, but change timing.
saveGIF({
	
	# Year counter
	currDay <- walmarts$days[1]
	timeIncrement <- 14 	# every 14 days
	# timeIncrement <- 1 	# daily (Warning: this will take a long time, make a big file, and probably not work)
	maxDay <- currDay + timeIncrement
	
	
	# A plot for each location
	cutoffDay <- daySpan + timeIncrement	# Cover the span, plus a little after
	while (currDay < cutoffDay) {
		walmarts.sub <- walmarts[walmarts$days < maxDay,]
		
		# Draw map
		map("state", proj="albers", param=c(39, 45), lwd=1, col="#f0f0f0")
		
		# Draw locations
		points(mapproject(walmarts.sub$lng, walmarts.sub$lat), col=NA, bg="#000000", pch=21, cex=0.2)
		
		currDay <- currDay + timeIncrement
		maxDay <- currDay + timeIncrement
	}
	
}, movie.name = "walmart-map-days.gif", interval = 0.1, nmax = 1, ani.width = 350, ani.height = 300)




###
# Add color and labels.
###

saveGIF({
	
	# Year counter
	currDay <- walmarts$days[1]
	timeIncrement <- 28 	# every 28 days
	maxDay <- currDay + timeIncrement
	
	
	# A plot for each location
	cutoffDay <- daySpan + timeIncrement	# Cover the span, plus a little after
	while (currDay < daySpan) {
		
		# Previously mapped Wal-Marts
		walmarts.past <- walmarts[walmarts$days < currDay,]
		
		# Wal-Marts during current time frame
		walmarts.curr <- walmarts[walmarts$days >= currDay & walmarts$days < maxDay,]
		
		# Draw blank map.
		map("state", proj="albers", param=c(39, 45), lwd=1, col="#f0f0f0")
		
		# Draw previous locations (black)
		points(mapproject(walmarts.past$lng, walmarts.past$lat), col=NA, bg="gray", pch=21, cex=0.3)
		
		# Draw current locations (red)
		points(mapproject(walmarts.curr$lng, walmarts.curr$lat), col=NA, bg="red", pch=21, cex=0.6)
		
		# Label for store count
		x <- 0
		y <- -0.94
		cnt <- length(walmarts.past[,1]) + length(walmarts.curr[,1])
		text(x, y, cnt, col="red")
		
		# Done with iteration. Increment.
		currDay <- currDay + timeIncrement
		maxDay <- currDay + timeIncrement
	}
	
}, movie.name = "walmart-map-days-color.gif", interval = 0.1, nmax = 50, ani.width = 350, ani.height = 300)




