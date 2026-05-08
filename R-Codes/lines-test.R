data = Species_Elevation
sum(is.na(data))
dim(data)

png("species-elevation.png", height = 1400, width = 1200)
par(mar=c(5,20,1,1))

 
plot(1, type='n', xlim=c(480, 1420), ylim=c(1, 97), xlab="Elevation", ylab = "" , yaxt="n", xaxt="n")
mtext("Species", side = 2, line = 18, at = 50, las = 0)
 
segments(data$x_start, data$y_values, data$x_end, data$y_values, col="blue", lwd=7)

axis(2, at=data$y_values, labels=data$y_labels, las=2)

axis(1, at=seq(480, 1420, by=20), labels=seq(480, 1420, by=20))

abline(v = c(480, 580, 700, 780, 870, 1000, 1100, 1250, 1360, 1420), col = "red", lty = 2)

dev.off()
