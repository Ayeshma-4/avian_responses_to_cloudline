library(MASS)
library(AER)
#################################################### load dataset ###########################################################
data = ToPawanthi_Master_data_set_new
head(data)
colnames(data)
sum(is.na(data))

############################################ Create the variable Transect & Elevation########################################
data$Extracted_Transect_Elevation=substr(data$`Transect_&_Elevation`,6,11)
data$Transect_Elevation=gsub('m','',data$Extracted_Transect_Elevation)
class(data$`Transect_Elevation`) = "Numeric"
data$Transect_Elevation = as.numeric(data$Transect_Elevation)
mode(data$Transect_Elevation)

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
poisson_sr = glm(Species_richness ~ Average_Humidity + Transect_Elevation + Average_Light_intensity + 
                   Average_Wind_speed, data = data, family = poisson)
summary(poisson_sr)


dispersiontest(new, trafo=1)
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
nb_sr = glm.nb(formula = Species_richness ~ Average_Humidity + Transect_Elevation + Average_Light_intensity + 
                 Average_Wind_speed, data = data, link = log)
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
poisson_sr = glm(Species_richness ~ Average_Humidity + Transect_Elevation + Average_Light_intensity + 
                   Average_Wind_speed, data = data, family = poisson)
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
nb_sr = glm.nb(formula = Species_richness ~ Average_Humidity + Transect_Elevation + Average_Light_intensity + 
                 Average_Wind_speed, data = data, link = log)
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
full = glm(formula = Species_richness ~ Cloudline+Average_Cloud_cover+Average_Humidity + Average_Temperature+Habitat_type+
             Transect_Elevation+Average_Light_intensity+Average_Wind_speed+
             
             Cloudline*Average_Cloud_cover+Cloudline*Average_Humidity+Cloudline*Average_Temperature+Cloudline*Habitat_type+
             Cloudline*Transect_Elevation+Cloudline*Average_Light_intensity+Cloudline*Average_Wind_speed+
             Average_Cloud_cover*Average_Humidity+Average_Cloud_cover*Average_Temperature+Average_Cloud_cover*Habitat_type,
           Average_Cloud_cover*Transect_Elevation+Average_Cloud_cover*Average_Light_intensity+Average_Cloud_cover*Average_Wind_speed+
             Average_Humidity*Average_Temperature+Average_Humidity*Habitat_type+Average_Humidity*Transect_Elevation+
             Average_Humidity*Average_Light_intensity+Average_Humidity*Average_Wind_speed+
             Average_Temperature*Habitat_type+Average_Temperature*Transect_Elevation+Average_Temperature*Average_Light_intensity+
             Average_Temperature*Average_Wind_speed+
             Habitat_type*Transect_Elevation+Habitat_type*Average_Light_intensity+Habitat_type*Average_Wind_speed+
             Transect_Elevation*Average_Light_intensity*Transect_Elevation*Average_Wind_speed+
             Average_Light_intensity*Average_Wind_speed, data = data, family = poisson)


## forward
SR_fwd_p = stepAIC(null, scope = list(lower=null, upper=full), data=data, direction="forward")

## backward
SR_bwd = stepAIC(full, scope = list(lower=null, upper=full), data=data, direction="backward")

## both
SR_both = stepAIC(null, scope = list(lower=null, upper=full), data=data, direction="both")

# Final MODEL_BUILDING
poisson_sr = glm(Species_richness ~ Cloudline + Transect_Elevation + Average_Cloud_cover + 
                   Average_Humidity + Average_Light_intensity + Average_Wind_speed + 
                   Cloudline:Average_Cloud_cover, data = data, family = poisson)
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
nb_sr = glm.nb(formula = Species_richness ~ Cloudline + Average_Cloud_cover + Average_Humidity + 
                 Average_Light_intensity + Transect_Elevation, data = data, link = log)
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
poisson_sr = glm(Species_richness ~ Average_Humidity + Transect_Elevation + Average_Light_intensity + 
                   Average_Wind_speed, data = data, family = poisson)
summary(poisson_sr)


dispersiontest(new, trafo=1)
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
nb_sr = glm.nb(formula = Species_richness ~ Average_Humidity + Transect_Elevation + Average_Light_intensity + 
                 Average_Wind_speed, data = data, link = log)
summary(nb_sr)
