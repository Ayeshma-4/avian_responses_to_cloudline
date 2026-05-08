data = Temperature_rainfall_and_cloudline_data_Pavanthi_19th_1_
table(data)
data1 = as.data.frame(table(data))
data1

data <- data1[data1$Freq != 0, , drop = FALSE]
data

class(data$Cloudline)

data$Cloudline = as.numeric(as.character(data$Cloudline))
data$rain = as.numeric(as.character(data$rain))

library(ggplot2)

png("Cloudline-Month1.png", height = 500, width = 700)

ggplot(data, aes(x = Sampling_Session, y = Cloudline, size = Freq, group = 1)) +
  geom_point()+ 
  geom_text(aes(label = paste(Cloudline,"(",Freq,")")), size = 4, vjust = 2.2) +
  scale_size(name = "Size", range = c(5, 12)) +
  theme(legend.position = "none")+
  labs(x = "Month", y = "Elevation of the cloudline", title = " ") +
  geom_line( aes(y = rain/0.035),   color = "black", size=0.1, linetype = "longdash") +
  geom_text(aes(y = rain/0.035, label= rain),   hjust = 0.5, vjust = 2,  size= 3, color= "black") +
  scale_x_discrete(breaks = 1:8, labels = c("Sep","Oct","Nov", "Dec", "Jan", "Feb", "Mar", "Apr"))+
  scale_y_continuous(name = "Cloudline Elevation", limits = c(0, 2000), breaks = seq(0, 2000, by = 500), sec.axis = sec_axis( trans=~.*0.035, name="Rainfall"))
  

dev.off()
getwd()
class(data$rain)
data$rain



png("Cloudline-Month2.png", height = 500, width = 700)

ggplot(data, aes(x = Sampling_Session, y = Cloudline, size = Freq, group = 1)) +
  geom_point()+ 
  geom_text(aes(label = paste(Cloudline,"(",Freq,")")), size = 4, vjust = 2.2) +
  scale_size(name = "Size", range = c(5, 12)) +
  theme(legend.position = "none")+
  labs(x = "Month", y = "Elevation of the cloudline", title = " ") +
  scale_x_discrete(breaks = 1:8, labels = c("Sep","Oct","Nov", "Dec", "Jan", "Feb", "Mar", "Apr"))+
  scale_y_continuous(name = "Cloudline Elevation", limits = c(0, 2000), breaks = seq(0, 2000, by = 500))


dev.off()
 


