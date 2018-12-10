library(plotrix)

drawCourt <- function(lineColor) {
	draw.arc(25, 5.25, 9/12, angle1=0, angle2=2*pi, col=lineColor, lwd=2) # Hoop
	lines(c(22,28), c(4,4), col=lineColor, lwd=1)	# Backboard
	lines(c(2.5, 2.5), c(0, 13), col=lineColor, lwd=1) # Side 3-pt
	lines(c(47.5, 47.5), c(0, 13), col=lineColor, lwd=1)
	lines(c(19,19), c(0,19), col=lineColor, lwd=1)	# Inside lane
	lines(c(31,31), c(0,19), col=lineColor, lwd=1)
	lines(c(19,31), c(19,19), col=lineColor, lwd=1)	# Free throw
	lines(c(0,50),c(0,0), col=lineColor, lwd=1)	# Baseline
	draw.arc(25, 5.25, 23.75, angle1=pi/9.8, angle2=pi/1.113, col=lineColor, lwd=1) # 3-pt arc
	
}
