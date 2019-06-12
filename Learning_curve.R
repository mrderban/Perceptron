require(plotrix) # package plotrix is needed for function "ablineclip""
# if the following line and the line containing "dev.off()" are executed, the plot will be saved as a png file in the current working directory
# png("Presidental.png", width = 18, height = 18, units = "cm", res = 800, pointsize = 10) 
op <- par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5 , font.lab = 2, cex.axis = 1.3, bty = "n", las=1)
plot(iterationNumber, slope, col="black", pch=21, bg = "grey", cex = 1,
     xlim=c(0,70), ylim=c(-2,6), ylab="", xlab="", axes=F)
axis(1)
axis(2) 
reg1 <- lm(slope~iterationNumber)
lines(iterationNumber,slope,col="black")
par(las=0)
mtext("Number of iterations", side=1, line=2.5, cex=1.5)
mtext("Slope of linear separation", side=2, line=3.7, cex=1.5)
# dev.off()
# For comparison, consider the default plot:
#par(op) # reset to default "par" settings
#plot(height.ratio, pop.vote) #yuk!