library(dplyr)
data = PCA_New_abr
sum(is.na(data))
colnames(data)

colnames(data)
data_subset = data[,c("Transect_Elevation", "Average_Humidity", "Average_Light_intensity", "Average_Temperature", "Average_Wind_speed" )]

add_var = data.frame( Species = data$Species, Habitat_type = data$Habitat_type)


data = cbind( data_subset,   add_var)
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

# result$Habitat_type = as.numeric(result$Habitat_type)
# result$Transect_Elevation = result$Transect_Elevation


data = result
colnames(data)
dim(data)
head(data)


pca_result <- prcomp(data[,c("Transect_Elevation", "Average_Humidity", "Average_Light_intensity", "Average_Temperature", "Average_Wind_speed" )] , scale. = TRUE)

pca_scores <- as.data.frame(pca_result$x[, 1:3])
summary(pca_result)
 



# Load necessary packages
library(ggplot2)
library(ggrepel)

# PC1-PC2
png("PC1-PC2-without-multiplying.png", height = 700, width = 900)
 
ggplot(data = pca_scores, aes(x = PC1, y = PC2, label = data$Species)) +
  geom_point(size = 4, color = "black") +
  geom_text_repel(size = 4, box.padding = 0.5, max.overlaps = 50) +  
  labs(title = "PC1 vs PC2", x = "PC1", y = "PC2") +
  theme_minimal()
dev.off()

# PC1-PC3
png("PC1-PC3--without-multiplying.png", height = 700, width = 900) 
ggplot(data = pca_scores, aes(x = PC1, y = PC3, label = data$Species)) +
  geom_point(size = 4, color = "black") +
  geom_text_repel(size = 4, box.padding = 0.5, max.overlaps = 50) +   
  labs(title = "PC1 vs PC3", x = "PC1", y = "PC3") +
  theme_minimal()
dev.off()

# PC2-PC3
png("PC2-PC3-without-multiplying.png", height = 700, width = 900) 
ggplot(data = pca_scores, aes(x = PC2, y = PC3, label = data$Species)) +
  geom_point(size = 4, color = "black") +
  geom_text_repel(size = 4, box.padding = 0.5, max.overlaps = 50) +  
  labs(title = "PC2 vs PC3", x = "PC2", y = "PC3") +
  theme_minimal()
dev.off()



 
