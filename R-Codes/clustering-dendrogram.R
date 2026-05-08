library(dplyr)
data = PCA_New
colnames(data)
data_subset = data[,c("Transect_Elevation", "Average_Humidity", "Average_Light_intensity", "Average_Temperature", "Average_Wind_speed" )] 

data_standardized =  scale(data_subset)

add_var = data.frame( Species = data$Species, Habitat_type = data$Habitat_type)


data = cbind( data_standardized,   add_var)
colnames(data)

dim(data)

calcmode <- function(a) {  
  vector <- unique(a)  
  vector[which.max(tabulate(match(a, vector)))]  
} 


result <- data %>%
  group_by(Species) %>%
  summarise(
    Habitat_type =  calcmode(Habitat_type),
    Transect_Elevation = mean(Transect_Elevation),
    Average_Humidity = mean(Average_Humidity),
    Average_Light_intensity = mean(Average_Light_intensity),
    Average_Temperature = mean(Average_Temperature),
    Average_Wind_speed = mean(Average_Wind_speed)
  )

result$Habitat_type = as.numeric(result$Habitat_type)*2
result$Transect_Elevation = result$Transect_Elevation*3

 
data = result
colnames(data)
dim(data)
head(data)
 

# Hierarchical clustering
hc <- hclust(dist(data[-1]), method='average')
png("dendrogram.png", height = 700, width = 1400)
par(mar=c(1,5,5,1))
plot(hc,main = "", labels=data$Species,cex=1, xlab="", ylab="", hang = -1)
rect.hclust(hc , k = 22,  which = c(3,4,7,10,11,13,15,16,18,19))
 
dev.off()
 
