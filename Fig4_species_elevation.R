library(readxl)

data <- read_excel("Fig4_species_elevation.xlsx")

sum(is.na(data))
dim(data)

make_plot <- function() {
  
  par(mar = c(5, 20, 1, 1))
  par(cex = 0.5)
  
  plot(1,
       type = "n",
       xlim = c(480, 1420),
       ylim = c(1, 97),
       xlab = "Elevation",
       ylab = "",
       yaxt = "n",
       xaxt = "n")
  
  mtext("Species", side = 2, line = 18, at = 50, las = 0)
  
  segments(data$x_start, data$y_values,
           data$x_end, data$y_values,
           col = "#2A9D8F", lwd = 7)
  
  axis(2, at = data$y_values, labels = data$y_labels, las = 2)
  
  axis(1, at = seq(480, 1420, by = 20),
       labels = seq(480, 1420, by = 20))
  
  abline(v = c(480, 580, 700, 780, 870,
               1000, 1100, 1250, 1360, 1420),
         col = "#B0B0B0", lty = 2)
}

# -------------------------
# PNG export (4x scale)
# -------------------------
png("Fig4_species_elevation.png",
    width = 4800,
    height = 5600,
    res = 600)

make_plot()
dev.off()

# -------------------------
# PDF export (vector, publication-safe)
# -------------------------
pdf("Fig4_species_elevation.pdf",
    width = 12,
    height = 14)

make_plot()
dev.off()