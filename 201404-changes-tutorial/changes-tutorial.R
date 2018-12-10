library(cpm)

#
# Just some random data
#

x <- c(rnorm(100,0,1), rnorm(100,1,1), rnorm(100,0,1), rnorm(100,-1,1))
result <- processStream(x, "Student", ARL0=2000, startup=20)
plot(x)
for (i in 1:length(result$changePoints)) {
	abline(v=result$changePoints[i], lty=2)
}

#
# Load Nathan's wakeup data
#

gmorning <- read.csv("data/gmorning-processed.tsv", stringsAsFactors=FALSE, sep="\t")

# Sort it by day and hour
gmorning.o <- gmorning[with(gmorning, order(dayssincestart, hour)),]
attach(gmorning.o)

#
# Find the change points
#

changes <- processStream(hour, "Kolmogorov-Smirnov", ARL0=1000, startup=15)
numChanges <- length(changes$changePoints)

#
# Plot the points and change points
#

plot(hour, xlab="", ylab="hour", main="Wake Times")
for (i in 1:length(changes$changePoints)) {
	abline(v=changes$changePoints[i], lty=2, col="red")
}

#
# Various Change Point Models
# 

par(mfrow=c(3,3), mar=c(3,3,3,3))
cpmTypes <- c("Student", "Bartlett", "GLR", "Mann-Whitney", "Mood", "Lepage", "Kolmogorov-Smirnov", "Cramer-von-Mises")
for (i in 1:length(cpmTypes)) {
	changes <- processStream(hour, cpmTypes[i], ARL0=1000, startup=15)
	numChanges <- length(changes$changePoints)

	plot(hour, xlab="", ylab="hour", main=paste(cpmTypes[i], "-", numChanges, "change pts"), col="#cccccc")
	for (j in 1:length(changes$changePoints)) {
		abline(v=changes$changePoints[j], lty=1, col="red")
	}
}


#
# Various thresholds
#

par(mfrow=c(3,3), mar=c(3,3,3,3))
thresholds <- c(370, 500, 1000, 2000, 3000, 4000, 5000, 10000, 20000)
for (i in 1:length(thresholds)) {
	changes <- processStream(hour, "Kolmogorov-Smirnov", ARL0=thresholds[i], startup=20)
	numChanges <- length(changes$changePoints)

	plot(hour, xlab="", ylab="hour", main=paste(thresholds[i], "-", numChanges, "change pts"), col="#cccccc")
	for (j in 1:length(changes$changePoints)) {
		abline(v=changes$changePoints[j], lty=1, col="purple")
	}
}


#
# Means at change intervals
#

# Base plot
plot(hour, xlab="", ylab="hour", main="Wake Times", col="gray")

# Change points
changes <- processStream(hour, "Kolmogorov-Smirnov", ARL0=1000, startup=15)
numChanges <- length(changes$changePoints)

# Get means at the beginning and end of sequence
firstChange <- changes$changePoints[1]
firstMean <- mean( hour[ 0:changes$changePoints[1] ] )
lastChange <- changes$changePoints[numChanges]
lastMean <- mean( hour[ lastChange:length(hour) ] )

# Plot the mean at each change point
x0 <- c(0, lastChange)
y0 <- c(firstMean, lastMean)
x1 <- c(firstChange, length(hour))
y1 <- c(firstMean, lastMean)
for (i in 2:length(changes$changePoints)) {
	prevChangePt <- changes$changePoints[i-1]
	currChangePt <- changes$changePoints[i]
	
	# Get the mean for the segment
	currMean <- mean(hour[prevChangePt:currChangePt])
	
	# Add segment
	x0 <- c(x0, prevChangePt)
	y0 <- c(y0, currMean)
	x1 <- c(x1, currChangePt)
	y1 <- c(y1, currMean)
}
segments(x0, y0, x1, y1, col="red")


# This time with time spacing, plot the mean at each change point
plot(dayssincestart, hour, xlab="Day", ylab="hour", main="Wake Times", col="gray")
x0 <- c(0, dayssincestart[lastChange])
y0 <- c(firstMean, lastMean)
x1 <- c(dayssincestart[firstChange], max(dayssincestart))
y1 <- c(firstMean, lastMean)
for (i in 2:length(changes$changePoints)) {
	
	prevChangePt <- changes$changePoints[i-1]
	currChangePt <- changes$changePoints[i]
	
	currMean <- mean(hour[prevChangePt:currChangePt])
	x0 <- c(x0, dayssincestart[prevChangePt])
	y0 <- c(y0, currMean)
	x1 <- c(x1, dayssincestart[currChangePt])
	y1 <- c(y1, currMean)
	
}
segments(x0, y0, x1, y1, col="red")



#
# Spruce up the plot
#

plot(dayssincestart, hour, xlab="", ylab="", main="Wake Times", type="n", axes=FALSE, ylim=c(6.5,13))


# Orient labels horizontally, with smaller font
par(las=1, cex=0.7)

# Horizontal axis
dayTicks <- c(1, 405, 770, 1135, 1501, 1957)
dayLabels <- c("Nov. 2008", "2010", "2011", "2012", "2013", "April 2014")
axis(1, at=dayTicks, labels=dayLabels, pos=6.5)

# Vertical axis
hourTicks <- 7:13
hourLabels <- c("7am", "8am", "9am", "10am", "11am", "Noon", "1pm")
axis(2, at=hourTicks, labels=hourLabels, pos=-50, col="white")

# Grid lines
for (i in 1:length(hourTicks)) {
	abline(h=hourTicks[i], lty=1, lwd=0.32, col="#cccccc")
}

# Draw points
points(dayssincestart, hour, pch=20, cex=0.5, col="#999999")

# Mean at each change point
segments(x0, y0, x1, y1, col="orange", lwd=4)



detach(gmorning.o)