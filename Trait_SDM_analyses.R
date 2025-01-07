##----------------------------------------------------------------------------##
##
##---------------- Authors: Matthew Bennion, Orlando Lam-Gordillo and Ben Hall ##
##---------------- Project : Final Thesis project Non funded ------------------
##---------------- Start date : 01/03/2024
##---------------- End date : 01/09/2024
##----------------------------------------------------------------------------##

rm(list = ls()) # Clear entire environment

## STEPS
# 1. Load packages
# 2. Read in rasters / SDMs
# 3. Read in spreadsheet with vulnerability scores and trait linked with taxa
# 4. File and object prep
# 5. Loop to stack by taxa traits
# 6. Loop to apply vulnerability scores and stack
# 7. Save all trait applied files
# 8. The secondary method to undertake this (START HERE IF MAPPING INDIVIDUAL TRAITS)
# 9. Functional Metrics steps (not required) use to plot individual metrics 
# 10. Functional redundancy calculationm


##----------------------------------------------------------------------------##
# 1. LOAD PACKAGES, FUNCTIONS & SET SWITHCES ----------------------------------

library(raster)
library(dplyr)

install.packages("raster")
install.packages("dplyr")

vul.score <- T
trait.map <- T
funcdiv <- F

# This function is used to rescale a raster 0-1
RescaleRast <- function(x,...)
             {(x-minValue(x,...))/(maxValue(x,...)-minValue(x,...))}

##----------------------------------------------------------------------------##
# 2. READ IN RASTERS - SDMs and UNCERTAINTY ----------------------------------

## Import HSI models # The rasters for SDM are available on request from NIWA or the authors 

wd <- ""
setwd(wd)

rast_list <- list.files(getwd(), pattern = ".tif") # list files in folder
input_rasters <- lapply(rast_list, raster) # load as raster
PA <- stack(input_rasters) # convert list of rasters to stack

rast_names <- rast_list %>%
  stringr::str_remove(pattern = "_ENS_ROC.tif")

names(PA) <- rast_names


## Import HSI model Standard Deviation

setwd("")
rast_list <- list.files(getwd(), pattern = ".tif") # list files in folder
input_rasters <- lapply(rast_list, raster) # load as raster
PA_SD <- stack(input_rasters) # convert list of rasters to stack

rast_names <- rast_list %>%
  stringr::str_remove(pattern = "_ENS_SD.tif")

names(PA_SD) <- rast_names

##----------------------------------------------------------------------------##
# 3. READ IN SPREADSHEET WITH VULNERABILITY SCORES AND TRAIT LINKED INFO ------ 

setwd("")
Trait_vul <- read.csv('NZTD_Modalities_Vul_Score.csv')  #This file is available from the author 
Traits <- read.csv('NZTD_Modalities_Vul_Score.csv')

##----------------------------------------------------------------------------##
# 4. FILE AND OBJECT PREPERATION ----------------------------------------------

Taxa.list <- Trait_vul$Taxa
Taxa.vul.score <- Trait_vul$Vul_score

##----------------------------------------------------------------------------##
# 5. LOOP TO STACK BY TAXA TRAITS ---------------------------------------------

if(trait.map == T){

Attachment <- Trait_vul[Trait_vul$Degree_of_attachment_Permanent==1,]
Movement_method <- Trait_vul[Trait_vul$Movement_method_None==1,]
Structural_fragility_VH <- Trait_vul[Trait_vul$Structural_fragility_Very_fragile_Highly_breakable==1,]
Structural_fragility_H <- Trait_vul[Trait_vul$Structural_fragility_Fragile_Limited_flexibility==1,]
Bioturb <- Trait_vul[Trait_vul$Feeding_mode_Filter_suspension==1,]
Long_lived <- Trait_vul[Trait_vul$Life_span_3_10_years==1,]

Attachment_stack <- PA[[Attachment$Taxa,]]
Movement_method_stack <- PA[[Movement_method$Taxa]]
Structural_fragility_VH_stack <- PA[[Structural_fragility_VH$Taxa]]
Structural_fragility_H_stack <- PA[[Structural_fragility_H$Taxa]]
Bioturb_stack <- PA[[Bioturb$Taxa]]
Long_lived_stack <- PA[[Long_lived$Taxa]]

Attachment_trait <- sum(Attachment_stack)
Movement_method_trait <- sum(Movement_method_stack)
Structureal_fragility_VH_trait <- sum(Structural_fragility_VH_stack)
Structureal_fragility_H_trait <- sum(Structural_fragility_H_stack)
Bioturb_trait <- sum(Bioturb_stack)
Long_lived_trait <- sum(Long_lived_stack)

# Functional Richness
#.... if the above is summed it's basically just summed HSI
Functional_rich <- sum(Attachment_trait, Movement_method_trait, Structureal_fragility_H_trait, Structureal_fragility_VH_trait, Bioturb_trait, Long_lived_trait)

}

# Plot to visualize results
plot(Functional_rich)

##----------------------------------------------------------------------------##
# 6. LOOP TO APPLY VULNERABILITY SCORES AND STACK -----------------------------

if(vul.score == T){

## Habitat suitability weighted vulnerability

for (i in 1:length(Taxa.list)){
  r <- PA[[Taxa.list[i]]] # extract spatial prediction based on  taxa name
  # multiply spatial prediction by vulnerability score based on taxa name
  r <- r * Trait_vul[Trait_vul$Taxa == Taxa.list[i],2]
  if (i == 1){
    PA.Vul <- r
  } else {
    PA.Vul <- PA.Vul + r
  }
}

# Uncertainty vulnerability weighted

for (i in 1:length(Taxa.list)){
  r <- PA_SD[[Taxa.list[i]]] # extract spatial prediction based on  taxa name
  # multiply spatial prediction by vulnerability score based on taxa name
  r <- r * Trait_vul[Trait_vul$Taxa == Taxa.list[i],2]
  if (i == 1){
    PA.SD.Vul <- r
  } else {
    PA.SD.Vul <- PA.SD.Vul + r
  }
}

}

# Plot to visualize results
plot(PA.Vul)
plot(PA.SD.Vul)

##----------------------------------------------------------------------------##
# 7. SAVE ALL TRAIT APPLIED FILES ---------------------------------------------

if(vul.score == T){
  setwd("C:/Users/Student/Documents/IMEC/Modules/NES8013 Consultancy Dissertation Project/Modelling/Outputs/Vulnerability_Score")

  # Save habitat suitability trait based vulnerability
  writeRaster(PA.Vul, filename = "HSI_weighted",
              format = "GTiff", overwrite = TRUE)

  # Save vulnerabiltiy weighted uncertainty
  writeRaster(PA.SD.Vul, filename = "HSI_weighted_uncert",
              format = "GTiff", overwrite = TRUE)

}

if(trait.map == T){
  setwd("C:/Users/Student/Documents/IMEC/Modules/NES8013 Consultancy Dissertation Project/Modelling/Outputs/Vulnerability_Score")

  # Save Movement Attachment method Trait
  writeRaster(Attachment_trait, filename = "Attachment_Trait",
              format = "GTiff", overwrite = TRUE)

  # Save Movement Method Trait
  writeRaster(Movement_method_trait, filename = "Movement_Method",
              format = "GTiff", overwrite = TRUE)

  # Save SF Very High trait
  writeRaster(Structureal_fragility_VH_trait, filename = "Structural_Fragility_VH",
              format = "GTiff", overwrite = TRUE)

  # Save SF High trait
  writeRaster(Structureal_fragility_H_trait, filename = "Structural_Fragility_H",
              format = "GTiff", overwrite = TRUE)

  # Save bioturbator trait
  writeRaster(Bioturb_trait, filename = "Bioturbators",
              format = "GTiff", overwrite = TRUE)

  # Save long-lived trait
  writeRaster(Long_lived_trait, filename = "Long_lived",
              format = "GTiff", overwrite = TRUE)

  # Save functional richness
  writeRaster(Functional_rich, filename = "Functional_richness",
              format = "GTiff", overwrite = TRUE)

}


##=============================================================================##


# 8. Mapping inidividual traits (Secondary Method) -------------------------------------------------

## Read in raster stack
# PA and PA_SD
# Read in taxa list too

#Taxa.list <- Traits$Taxa
#Trait.scores <- Traits[,2:ncol(Traits)]

#Loading in individual traits
setwd("C:/Users/Student/Documents/IMEC/Modules/NES8013 Consultancy Dissertation Project/Modelling/Data")

#Additonal code to change the number of traits looked into
Trait.list <- read.csv('Traits_relevant.csv')
Trait.scores <- Trait.list

Trait.list <- Trait.list$Traits[-1]
Trait.scores <- Traits[,Trait.list]
trait.names <- colnames(Trait.scores[1:ncol(Trait.scores)])
trait.stack <- stack()


for (j in 1:ncol(Trait.scores)){

  scores <- Trait.scores[,j]

   for(i in 1:length(Taxa.list)){

    r <- PA[[Taxa.list[i]]] # extract spatial prediction based on  taxa name

    # multiply spatial prediction by vulnerability score based on taxa name
    r <- r * scores[i]

    if (i == 1){
    trait <- r

    } else {

    trait <- trait + r

    }
   }
  trait.stack <- stack(trait.stack, trait)

}

names(trait.stack) <- trait.names


setwd("C:/Users/Student/Documents/IMEC/Modules/NES8013 Consultancy Dissertation Project/Modelling/Outputs")
writeRaster(trait.stack, names(trait.stack), bylayer=TRUE, format='GTiff')


#Create functional richness
funrich <- sum(trait.stack)

# Save functional richness
writeRaster(funrich, filename = "Functional_richness",
            format = "GTiff", overwrite = TRUE)


## Now repeat for uncertainty and save all ! Also compute CV

trait_sd.stack <- stack()

for (j in 1:ncol(Trait.scores)){

  scores <- Trait.scores[,j]

  for(i in 1:length(Taxa.list)){

    r <- PA_SD[[Taxa.list[i]]] # extract spatial prediction based on  taxa name

    # multiply spatial prediction by vulnerability score based on taxa name
    r <- r * scores[i]

    if (i == 1){
      trait_sd <- r

    } else {

      trait_sd <- trait + r

    }
  }
  trait_sd.stack <- stack(trait_sd.stack, trait_sd)

}

names(trait_sd.stack) <- trait.names

setwd("")
writeRaster(trait.stack, paste(names(trait.stack), "_SD", sep = ""), bylayer=TRUE, format='GTiff')


#Compute SD of functional richness
funrich_sd <- sum(trait_sd.stack)

# Calculate CV of functional richness layer
funrich_CV <- (funrich_sd/funrich)*100


# Save functional richness
writeRaster(funrich_sd, filename = "Functional_richness_SD",
            format = "GTiff", overwrite = TRUE)

# Save functional richness
writeRaster(funrich_CV, filename = "Functional_richness_CV",
            format = "GTiff", overwrite = TRUE)


##------------------------------------------------------##

plot(trait.stack$Feeding_mode_Deposit_feeder)  #Or the example trait



##----------------------------------------------------------------------------##



# 9. Functional Metrics (Not required)  ---------------------------------------

install.packages("FD")
library(FD)

a<-read.csv("ABUNDANCE_DATA.csv",row.names=1,header=1,check.names = F)
b<-read.csv("FUZZY_DATA.csv",row.names=1,header=1)

dim(a)
dim(b)

FunctDiv<-dbFD(b,a)
FunctDiv





# 10. Functional redundancy Calculation ----

#PA_Bin <- PA
#PA_Bin[PA>0] <- 1
species_rich <- sum(PA)  # Binary/non-binary species richness
funrich <- funrich       ## From above

species_rich_rescale <- RescaleRast(species_rich)
plot(species_rich_rescale)

Funrich_rescale <- RescaleRast(funrich)
plot(Funrich_rescale)

function_redundancy <- (Funrich_rescale/species_rich_rescale)

Funredun_rescale <- RescaleRast(function_redundancy)
plot(Funredun_rescale)


# Save functional redundancy
writeRaster(function_redundancy, filename = "Function_redundancy",
            format = "GTiff", overwrite = TRUE)


sum(na.omit(value((raster))))


