data = Temperature_data_Pavanthi
table(data)
data1 = as.data.frame(table(data))
data1

data <- data1[data1$Freq != 0, , drop = FALSE]
data

class(data$Cloudline)

data$Cloudline = as.numeric(as.character(data$Cloudline))
data$Max = round(as.numeric(as.character(data$Max)),2)
data$Min = round(as.numeric(as.character(data$Min)),2)
data$Mean = round(as.numeric(as.character(data$Mean)),2)
class(data$Max)


library(ggplot2)

 
png("Cloudline-Month-Temp.png", height = 500, width = 700)

ggplot(data, aes(x = Sampling_Session, y = Cloudline, size = Freq, group = 1)) +
  geom_point()+ 
  geom_text(aes(label = paste(Cloudline,"(",Freq,")")), size = 4, vjust = 2.2) +
  scale_size(name = "Size", range = c(5, 12)) +
  theme(legend.position = "none")+
  labs(x = "Month", y = "Elevation of the cloudline", title = " ") +
  geom_line( aes(y = (Mean-20)/0.01),   color = "brown", size=0.1) +
  geom_text(aes(y = (Mean-20)/0.01, label= Mean),   hjust = 1.5, vjust = 0.5,  size= 3, color= "brown") +
  scale_x_discrete(breaks = 1:8, labels = c("Sep","Oct","Nov", "Dec", "Jan", "Feb", "Mar", "Apr"))+
  scale_y_continuous(name = "Cloudline Elevation", limits = c(0, 2000), breaks = seq(0, 2000, by = 500), sec.axis = sec_axis( trans=~.*0.01+20, name="Temperature"))



dev.off()

 
