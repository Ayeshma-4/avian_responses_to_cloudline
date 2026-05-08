data = ToPawanthi_Master_data_set_new_new
library(ggplot2)
head(data)
sum(is.na(data))
############################################ Create the variable Transect & Elevation########################################
data$Extracted_Transect_Elevation=substr(data$`Transect_&_Elevation`,6,11)
data$Transect_Elevation=gsub('m','',data$Extracted_Transect_Elevation)
class(data$`Transect_Elevation`) = "Numeric"
data$Transect_Elevation = as.numeric(data$Transect_Elevation)
mode(data$Transect_Elevation)

data$Habitat_type = factor(data$Habitat_type)
summary(data)
colnames(data)
data$diff = abs(data$Cloudline-data$Transect_Elevation)
sum(data$diff==0)
mean(data$Species_richness[data$diff==0])

sum(data$Cloudline==2000)
mean(data$Species_richness[data$Cloudline==2000])
table(data$Cloudline)

dev.off()
plot(data$diff, data$Species_richness)
?plot

ggplot(data,aes(x=diff,y=Species_richness)) + geom_point()+xlab("Distance")+ylab("Species richness") +
  geom_smooth( se=FALSE , linetype = "dashed", span = 1) +
  ggtitle("Species Richness variation away from the cloudline") + theme(plot.title = element_text(hjust = 0.5))

