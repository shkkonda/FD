# Load the data.
cpi <- read.csv("data/cpi-monthly-us.csv", stringsAsFactors=FALSE)
eggs <- read.csv("data/egg-prices-monthly.csv", stringsAsFactors=FALSE)
gas <- read.csv("data/gas-prices-monthly.csv", stringsAsFactors=FALSE)

# Regular time series for gas price
par(cex.axis=0.7)
gas.ts <- ts(gas$Value, start=c(1976, 1), frequency=12)
plot(gas.ts, xlab="", ylab="", main="Dollars per gallon", las=1, bty="n")


# Monthly change
curr <- gas$Value[-1]
prev <- gas$Value[1:(length(gas$Value)-1)]
monChange <- 100 * round( (curr-prev) / prev, 2 )
barCols <- sapply(monChange, 
	function(x) { 
		if (x < 0) {
			return("#2cbd25")
		} else {
			return("gray")
		}
	})
#monChange.ts <- ts(monChange, start=c(1976, 2), frequency=12)
barplot(monChange, border=NA, space=0, las=1, col=barCols, main="% change, monthly")


# Year-over-year change
curr <- gas$Value[-(1:12)]
prev <- gas$Value[1:(length(gas$Value)-12)]
annChange <- 100 * round( (curr-prev) / prev, 2 )
barCols <- sapply(annChange, 
	function(x) { 
		if (x < 0) {
			return("#2cbd25")
		} else {
			return("gray")
		}
	})
barplot(annChange, border=NA, space=0, las=1, col=barCols, main="% change, annual")


# Relative to current 2013
curr <- gas$Value[length(gas$Value)]
gasDiff <- gas$Value - curr
barCols.diff <- sapply(gasDiff,
	function(x) {
		if (x < 0) {
			return("gray")
		} else {
			return("black")
		}
	}
)
barplot(gasDiff, border=NA, space=0, las=1, col=barCols.diff, main="Dollar difference from September 2013")




# Adjust gas price for inflation
gas.cpi.merge <- merge(gas, cpi, by=c("Year", "Period"))
gas.cpi <- gas.cpi.merge[,-c(3,5)]
colnames(gas.cpi) <- c("year", "month", "gasprice.unadj", "cpi")
currCPI <- gas.cpi[dim(gas.cpi)[1], "cpi"]
gas.cpi$cpiFactor <- currCPI / gas.cpi$cpi
gas.cpi$gasprice.adj <- gas.cpi$gasprice.unadj * gas.cpi$cpiFactor

curr <- gas.cpi$gasprice.adj[dim(gas.cpi)[1]]
gasDiff.adj <- gas.cpi$gasprice.adj - curr
barCols.diff.adj <- sapply(gasDiff.adj,
	function(x) {
		if (x < 0) {
			return("gray")
		} else {
			return("black")
		}
	}
)
barplot(gasDiff.adj, border=NA, space=0, las=1, col=barCols.diff.adj, main="Adjusted dollar difference from September 2013")


# Adjusted annual change
curr <- gas.cpi$gasprice.adj[-(1:12)]
prev <- gas.cpi$gasprice.adj[1:(length(gas.cpi$gasprice.adj)-12)]
annChange.adj <- 100 * round( (curr-prev) / prev, 2 )
barCols.adj <- sapply(annChange.adj, 
	function(x) { 
		if (x < 0) {
			return("#2cbd25")
		} else {
			return("gray")
		}
	})
barplot(annChange.adj, border=NA, space=0, las=1, col=barCols.adj, main="% change, annual adjusted")




# Adjusted time series
par(mfrow=c(2,1), mar=c(4,3,2,2))
gas.ts.adj <- ts(gas.cpi$gasprice.adj, start=c(1976, 1), frequency=12)
plot(gas.ts, xlab="", ylab="", main="Dollars per gallon, unadjusted", las=1, bty="n")
plot(gas.ts.adj, xlab="", ylab="", main="Dollars per gallon, adjusted", las=1, bty="n")




# Gas versus eggs
gas.eggs.merge <- merge(gas, eggs, by=c("Year", "Period"))
gas.eggs <- gas.eggs.merge[,-c(3,5)]
colnames(gas.eggs) <- c("year", "month", "gas", "eggs")
gas.ts <- ts(gas.eggs$gas, start=c(1980, 1), frequency=12)
eggs.ts <- ts(gas.eggs$eggs, start=c(1980, 1), frequency=12)
par(bty="n", las=1)
ts.plot(gas.ts, eggs.ts, col=c("dark gray", "black"), ylim=c(0, 4), main="Price for dozen of eggs vs. gallon of regular gas, unadjusted", ylab="Dollars")
text(1980, 1.6, "Gas", pos=4, cex=0.7, col="dark gray")
text(1980, 0.5, "Eggs", pos=4, cex=0.7, col="black")


# Eggs to gas ratio
eggs.gas.ratio <- ts(gas.eggs$eggs/gas.eggs$gas, start=c(1980, 1), frequency=12)
par(cex.axis=0.7)
plot(eggs.gas.ratio, bty="n", las=1, ylab="", main="Price of eggs to gas")
lines(c(1970, 2015), c(1,1), lty=2, lwd=0.5, col="gray")
text(1979, 1.11, "Eggs cost more", cex=0.6, pos=4, offset=0)
text(1979, 0.89, "Gas costs more", cex=0.6, pos=4, offset=0)
