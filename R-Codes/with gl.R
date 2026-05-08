library(MASS)
library(AER)
library(dplyr)
#################################################### load dataset ###########################################################
data = ToPawanthi_Master_data_set_new_new
head(data)
colnames(data)
sum(is.na(data))

############################################ Create the variable Transect & Elevation########################################
data$Extracted_Transect_Elevation=substr(data$`Transect_&_Elevation`,6,11)
data$Transect_Elevation=gsub('m','',data$Extracted_Transect_Elevation)
class(data$`Transect_Elevation`) = "Numeric"
data$Transect_Elevation = as.numeric(data$Transect_Elevation)
mode(data$Transect_Elevation)

data$Habitat_type = factor(data$Habitat_type)
summary(data)

###################################### Standardize data###########################################
data_subset = data[,c("Average_Humidity", "Average_Light_intensity", "Average_Temperature", 
                      "Average_Wind_speed", "Average_Cloud_cover", "Cloudline", "Transect_Elevation")] 

data_standardized =  scale(data_subset)

add_var = data.frame(Habitat_type = data$Habitat_type, Species_richness = data$Species_richness, Abundance = data$Abundance)

data = cbind( data_standardized,   add_var)
head(data)
 #ex 
data1 = ToPawanthi_Master_data_set_new_new
head(data1)
data1$Average_Humidity[1]
(data1$Average_Humidity[1]-mean(data1$Average_Humidity))/sd(data1$Average_Humidity)

############################################### check mean & variance #########################################################
# Abundance
hist(data$Abundance)
cat("mean:", mean(data$Abundance)) 
cat("Variance: ",var(data$Abundance))

# SpeciesRichness
hist(data$Species_richness)
cat("mean:", mean(data$Species_richness)) 
cat("Variance: ",var(data$Species_richness))

############################################# Without CC Without CL ######################################################################
############################################ Poisson Regression ##################################################################### 

# Poisson - Abundance

null = glm(Abundance ~ 1, data=data, family = poisson)
full = glm(formula = Abundance ~ Average_Humidity + Average_Temperature+Habitat_type+Transect_Elevation+Average_Light_intensity+Average_Wind_speed , data = data, family = poisson)

## forward
Ab_fwd_p = stepAIC(null, scope = list(lower=null, upper=full), data=data, direction="forward")

## backward
Ab_bwd_p = stepAIC(full, scope = list(lower=null, upper=full), data=data, direction="backward")

## both
Ab_both_p = stepAIC(null, scope = list(lower=null, upper=full), data=data, direction="both")

#Poisson - SR
null = glm(Species_richness ~ 1, data=data, family = poisson)
full = glm(formula = Species_richness ~ Average_Humidity + Average_Temperature+Habitat_type+Transect_Elevation+Average_Light_intensity+Average_Wind_speed, data = data, family = poisson)


## forward
SR_fwd_p = stepAIC(null, scope = list(lower=null, upper=full), data=data, direction="forward")

## backward
SR_bwd = stepAIC(full, scope = list(lower=null, upper=full), data=data, direction="backward")

## both
SR_both = stepAIC(null, scope = list(lower=null, upper=full), data=data, direction="both")

# Final MODEL_BUILDING
poisson_sr = glm(Species_richness ~ Average_Humidity + Habitat_type + Average_Wind_speed + 
                   Transect_Elevation + Average_Light_intensity, data = data, family = poisson)
summary(poisson_sr)


dispersiontest(poisson_sr, trafo=1)

############################################### Negative Binomial Regression ####################################################

#NB - Abundance
null = glm.nb(Abundance ~ 1, data=data)
full = glm.nb(formula = Abundance ~ Average_Humidity + Average_Temperature+Habitat_type+Transect_Elevation+Average_Light_intensity+Average_Wind_speed , data = data, link = log)

## forward
Ab_fwd = stepAIC(null, scope = list(lower=null, upper=full), data=data, direction="forward")

## backward
Ab_bwd = stepAIC(full, scope = list(lower=null, upper=full), data=data, direction="backward")

## both
Ab_both = stepAIC(null, scope = list(lower=null, upper=full), data=data, direction="both")



#NB - SR
#NB
null = glm.nb(Species_richness ~ 1, data=data)
full = glm.nb(formula = Species_richness ~ Average_Humidity + Average_Temperature+Habitat_type+Transect_Elevation+Average_Light_intensity+Average_Wind_speed , data = data, link = log)


## forward
SR_fwd = stepAIC(null, scope = list(lower=null, upper=full), data=data, direction="forward")

## backward
SR_bwd = stepAIC(full, scope = list(lower=null, upper=full), data=data, direction="backward")

## both
SR_both = stepAIC(null, scope = list(lower=null, upper=full), data=data, direction="both")


# Final MODEL BUILDING
nb_sr = glm.nb(formula = Species_richness ~ Average_Humidity + Transect_Elevation, data = data, link = log)
summary(nb_sr)


############################################# With CC Without CL ######################################################################
############################################ Poisson Regression ##################################################################### 

# Poisson - Abundance

null = glm(Abundance ~ 1, data=data, family = poisson)
full = glm(formula = Abundance ~ Average_Cloud_cover+Average_Humidity + Average_Temperature+Habitat_type+Transect_Elevation+Average_Light_intensity+Average_Wind_speed , data = data, family = poisson)

## forward
Ab_fwd_p = stepAIC(null, scope = list(lower=null, upper=full), data=data, direction="forward")

## backward
Ab_bwd_p = stepAIC(full, scope = list(lower=null, upper=full), data=data, direction="backward")

## both
Ab_both_p = stepAIC(null, scope = list(lower=null, upper=full), data=data, direction="both")

#Poisson - SR
null = glm(Species_richness ~ 1, data=data, family = poisson)
full = glm(formula = Species_richness ~ Average_Cloud_cover+Average_Humidity + Average_Temperature+Habitat_type+Transect_Elevation+Average_Light_intensity+Average_Wind_speed, data = data, family = poisson)


## forward
SR_fwd_p = stepAIC(null, scope = list(lower=null, upper=full), data=data, direction="forward")

## backward
SR_bwd = stepAIC(full, scope = list(lower=null, upper=full), data=data, direction="backward")

## both
SR_both = stepAIC(null, scope = list(lower=null, upper=full), data=data, direction="both")

# Final MODEL_BUILDING
poisson_sr = glm(Species_richness ~ Average_Humidity + Habitat_type + Average_Cloud_cover + 
                   Average_Wind_speed + Average_Light_intensity + Transect_Elevation
                 , data = data, family = poisson)
summary(poisson_sr)


dispersiontest(new, trafo=1)
############################################### Negative Binomial Regression ####################################################

#NB - Abundance
null = glm.nb(Abundance ~ 1, data=data)
full = glm.nb(formula = Abundance ~ Average_Cloud_cover+Average_Humidity + Average_Temperature+Habitat_type+Transect_Elevation+Average_Light_intensity+Average_Wind_speed , data = data, link = log)

## forward
Ab_fwd = stepAIC(null, scope = list(lower=null, upper=full), data=data, direction="forward")

## backward
Ab_bwd = stepAIC(full, scope = list(lower=null, upper=full), data=data, direction="backward")

## both
Ab_both = stepAIC(null, scope = list(lower=null, upper=full), data=data, direction="both")



#NB - SR
#NB
null = glm.nb(Species_richness ~ 1, data=data)
full = glm.nb(formula = Species_richness ~ Average_Cloud_cover+Average_Humidity + Average_Temperature+Habitat_type+Transect_Elevation+Average_Light_intensity+Average_Wind_speed , data = data, link = log)


## forward
SR_fwd = stepAIC(null, scope = list(lower=null, upper=full), data=data, direction="forward")

## backward
SR_bwd = stepAIC(full, scope = list(lower=null, upper=full), data=data, direction="backward")

## both
SR_both = stepAIC(null, scope = list(lower=null, upper=full), data=data, direction="both")


# Final MODEL BUILDING
nb_sr = glm.nb(formula = Species_richness ~ Average_Humidity + Average_Cloud_cover + Transect_Elevation + 
                 Average_Light_intensity, data = data, link = log)
summary(nb_sr)


############################################# With CC With CL ######################################################################
############################################ Poisson Regression ##################################################################### 

# Poisson - Abundance

null = glm(Abundance ~ 1, data=data, family = poisson)
full = glm(formula = Abundance ~ Cloudline+Average_Cloud_cover+Average_Humidity + Average_Temperature+Habitat_type+Transect_Elevation+Average_Light_intensity+Average_Wind_speed , data = data, family = poisson)

## forward
Ab_fwd_p = stepAIC(null, scope = list(lower=null, upper=full), data=data, direction="forward")

## backward
Ab_bwd_p = stepAIC(full, scope = list(lower=null, upper=full), data=data, direction="backward")

## both
Ab_both_p = stepAIC(null, scope = list(lower=null, upper=full), data=data, direction="both")

#Poisson - SR
null = glm(Species_richness ~ 1, data=data, family = poisson)
full = glm(formula = Species_richness ~ Cloudline+Average_Cloud_cover+Average_Humidity + Average_Temperature+Habitat_type+Transect_Elevation+Average_Light_intensity+Average_Wind_speed, data = data, family = poisson)


## forward
SR_fwd_p = stepAIC(null, scope = list(lower=null, upper=full), data=data, direction="forward")

## backward
SR_bwd = stepAIC(full, scope = list(lower=null, upper=full), data=data, direction="backward")

## both
SR_both = stepAIC(null, scope = list(lower=null, upper=full), data=data, direction="both")

# Final MODEL_BUILDING
poisson_sr = glm(Species_richness ~ Cloudline + Habitat_type + Transect_Elevation + 
                   Average_Cloud_cover + Average_Light_intensity + Average_Humidity + 
                   Average_Wind_speed, data = data, family = poisson)
summary(poisson_sr)


dispersiontest(poisson_sr, trafo=1)
############################################### Negative Binomial Regression ####################################################

#NB - Abundance
null = glm.nb(Abundance ~ 1, data=data)
full = glm.nb(formula = Abundance ~ Cloudline+Average_Cloud_cover+Average_Humidity + Average_Temperature+Habitat_type+Transect_Elevation+Average_Light_intensity+Average_Wind_speed , data = data, link = log)

## forward
Ab_fwd = stepAIC(null, scope = list(lower=null, upper=full), data=data, direction="forward")

## backward
Ab_bwd = stepAIC(full, scope = list(lower=null, upper=full), data=data, direction="backward")

## both
Ab_both = stepAIC(null, scope = list(lower=null, upper=full), data=data, direction="both")



#NB - SR
#NB
null = glm.nb(Species_richness ~ 1, data=data)
full = glm.nb(formula = Species_richness ~ Cloudline+Average_Cloud_cover+Average_Humidity + Average_Temperature+Habitat_type+Transect_Elevation+Average_Light_intensity+Average_Wind_speed , data = data, link = log)


## forward
SR_fwd = stepAIC(null, scope = list(lower=null, upper=full), data=data, direction="forward")

## backward
SR_bwd = stepAIC(full, scope = list(lower=null, upper=full), data=data, direction="backward")

## both
SR_both = stepAIC(null, scope = list(lower=null, upper=full), data=data, direction="both")


# Final MODEL BUILDING
nb_sr = glm.nb(formula = Species_richness ~ Cloudline + Transect_Elevation + Average_Light_intensity + 
                 Average_Cloud_cover + Average_Humidity, data = data, link = log)
summary(nb_sr)

############################################# Without CC With CL ######################################################################
############################################ Poisson Regression ##################################################################### 

# Poisson - Abundance

null = glm(Abundance ~ 1, data=data, family = poisson)
full = glm(formula = Abundance ~ Cloudline +Average_Humidity + Average_Temperature+Habitat_type+Transect_Elevation+Average_Light_intensity+Average_Wind_speed , data = data, family = poisson)

## forward
Ab_fwd_p = stepAIC(null, scope = list(lower=null, upper=full), data=data, direction="forward")

## backward
Ab_bwd_p = stepAIC(full, scope = list(lower=null, upper=full), data=data, direction="backward")

## both
Ab_both_p = stepAIC(null, scope = list(lower=null, upper=full), data=data, direction="both")

#Poisson - SR
null = glm(Species_richness ~ 1, data=data, family = poisson)
full = glm(formula = Species_richness ~ Cloudline +Average_Humidity + Average_Temperature+Habitat_type+Transect_Elevation+Average_Light_intensity+Average_Wind_speed, data = data, family = poisson)


## forward
SR_fwd_p = stepAIC(null, scope = list(lower=null, upper=full), data=data, direction="forward")

## backward
SR_bwd = stepAIC(full, scope = list(lower=null, upper=full), data=data, direction="backward")

## both
SR_both = stepAIC(null, scope = list(lower=null, upper=full), data=data, direction="both")

# Final MODEL_BUILDING
poisson_sr = glm(Species_richness ~ Cloudline + Habitat_type + Transect_Elevation + 
                   Average_Light_intensity + Average_Wind_speed, data = data, family = poisson)
summary(poisson_sr)

pchisq(poisson_sr$deviance, df=poisson_sr$df.residual, lower.tail=FALSE)

dispersiontest(poisson_sr, trafo=1)

############################################### Negative Binomial Regression ####################################################

#NB - Abundance
null = glm.nb(Abundance ~ 1, data=data)
full = glm.nb(formula = Abundance ~ Cloudline +Average_Humidity + Average_Temperature+Habitat_type+Transect_Elevation+Average_Light_intensity+Average_Wind_speed , data = data, link = log)

## forward
Ab_fwd = stepAIC(null, scope = list(lower=null, upper=full), data=data, direction="forward")

## backward
Ab_bwd = stepAIC(full, scope = list(lower=null, upper=full), data=data, direction="backward")

## both
Ab_both = stepAIC(null, scope = list(lower=null, upper=full), data=data, direction="both")



#NB - SR
#NB
null = glm.nb(Species_richness ~ 1, data=data)
full = glm.nb(formula = Species_richness ~ Cloudline +Average_Humidity + Average_Temperature+Habitat_type+Transect_Elevation+Average_Light_intensity+Average_Wind_speed , data = data, link = log)


## forward
SR_fwd = stepAIC(null, scope = list(lower=null, upper=full), data=data, direction="forward")

## backward
SR_bwd = stepAIC(full, scope = list(lower=null, upper=full), data=data, direction="backward")

## both
SR_both = stepAIC(null, scope = list(lower=null, upper=full), data=data, direction="both")


# Final MODEL BUILDING
nb_sr = glm.nb(formula = Species_richness ~ Cloudline + Transect_Elevation + Average_Light_intensity + 
                 Habitat_type, data = data, link = log)
summary(nb_sr)
