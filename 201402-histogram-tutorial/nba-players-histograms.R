# Load the data.
players <- read.csv("nba-players.csv", stringsAsFactors=FALSE)

#
# Bar charts
#

warriors <- subset(players, Team=="Warriors")
warriors.o <- warriors[order(warriors$Ht_inches),]
par(mar=c(5,10,5,5))
barplot(warriors.o$Ht_inches, names.arg=warriors.o$Name, horiz=TRUE, border=NA, las=1, main="Heights of Golden State Warriors")

avgHeights <- aggregate(Ht_inches ~ POS, data=players, mean)
avgHeights.o <- avgHeights[order(avgHeights$Ht_inches, decreasing=FALSE),]
barplot(avgHeights.o$Ht_inches, names.arg=avgHeights.o$POS, border=NA, las=1)


# 
# Symbol plot
#

htrange <- range(players$Ht_inches)	# 69 to 87 inches
cnts <- rep(0, 20)
y <- c()
for (i in 1:length(players[,1])) {
	
	cntIndex <- players$Ht_inches[i] - htrange[1] + 1
	cnts[cntIndex] <- cnts[cntIndex] + 1
	y <- c(y, cnts[cntIndex])

}
plot(players$Ht_inches, y, type="n", main="Player heights", xlab="inches", ylab="count")
points(players$Ht_inches, y, pch=21, col=NA, bg="#999999")

barplot(cnts, names.arg=69:88, main="Player heights", xlab="inches", ylab="count", border=NA, las=1)


#
# Histograms with various bin sizes
#

par(mfrow=c(1,3), mar=c(3,3,3,3))
hist(players$Ht_inches, main="NBA Player Heights", xlab="inches", breaks=seq(65, 90, 1))
hist(players$Ht_inches, main="NBA Player Heights", xlab="inches", breaks=seq(65, 90, 2))
hist(players$Ht_inches, main="NBA Player Heights", xlab="inches", breaks=seq(65, 90, 5))


#
# Different bar widths
#

par(mfrow=c(1,3), mar=c(3,3,3,3))
hist(players$Ht_inches, main="NBA Player Heights", xlab="inches", breaks=c(seq(65, 75, 2), 80, 90))
hist(players$Ht_inches, main="NBA Player Heights", xlab="inches", breaks=c(65, 75, seq(80, 90, 2)))
hist(players$Ht_inches, main="NBA Player Heights", xlab="inches", breaks=c(65, seq(70, 80, 1), 90))


#
# Compare
#

par(mfrow=c(2,3), las=1, mar=c(5,5,4,1))
positions <- unique(players$POS)
for (i in 1:length(positions)) {
	currPlayers <- subset(players, POS==positions[i])
	hist(currPlayers$Ht_inches, main=positions[i], breaks=65:90, xlab="inches", border="#ffffff", col="#999999", lwd=0.4)
}


# Sorted, with a median line
par(mfrow=c(5,1), las=1, mar=c(5,5,4,1), xaxs="i", yaxs="i")
for (i in 1:length(avgHeights.o$POS)) {
	currPlayers <- subset(players, POS==avgHeights.o$POS[i])
	htMedian <- median(currPlayers$Ht_inches)
	h <- hist(currPlayers$Ht_inches, main=avgHeights.o$POS[i], breaks=65:90, xlab="inches", border=NA, col="#999999", lwd=0.4)
	maxFreq <- max(h$counts)
	segments(h$breaks, rep(0, length(h$breaks)), h$breaks, maxFreq, col="white")
	
	# Median line
	lines(c(htMedian, htMedian), c(-1, maxFreq), col="purple", lwd=2)
}





