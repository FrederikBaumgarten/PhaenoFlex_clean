 ##======================================================================================================
##
##  Phaenoflex - Shoot elongation
##
## Datum:           15.04.2024
## Autor:           Frederik Baumgarten
#======================================================================================================

rm(list=ls(all=TRUE))

 #------------------------------------------------------------------------------------------------------
 # Einstellungen
 #------------------------------------------------------------------------------------------------------
 # Set Working Directory
 setwd("~/switchdrive/Documents/PhenoGen/22_PhaenoFlex/dat/shoot_elongation/")
 # Libraries
 library(raster)
 library(readxl)
 library(arm)
 library(stats)
 library(data.table)
 library(nlsr)
 library(Ryacas)
 library(tidyverse)
 library(broom)
 library(writexl)
 library(dplyr)
 library(ggplot2)
 #-------------------------------------
 ## Daten einlesen
# Read the Excel file
data <- readxl::read_excel("shoot_elongation_final.xlsx", sheet = "data_raw", na = "NA") # nolint: line_length_linter.
summary(data)
str(data)
names(data)

### Clean the data
# create a new dataset with the same time window (doys) for every replicate
doy_uni<-unique(data$doy) #these are the monitoring days

# Get the unique values of "doy" and "replicate"
doy_uni <- unique(data$doy)
Tree_ID <- unique(data$TreeID)

# Create a new dataset with all combinations of "doy" and "Tree_ID"
new_data <- expand.grid(doy = doy_uni, Tree_ID = Tree_ID)

# Join the new dataset with the original dataset
new_data <- left_join(new_data, data, by = c("doy", "TreeID"))


## Select only controls
data <- data %>% 
    filter(treat == "Control")


# Create the plot
plot <- ggplot(data, aes(x = doy, y = shoot_length, color = rep)) +
    geom_point() +
    facet_wrap(~ rep)

# Print the plot
print(plot)


# Create a new dataset with the mean and standard error of "shoot_length" for each treatment
data_summary <- data %>%
    group_by(treat, doy) %>%
    summarise(mean_shoot_length = mean(shoot_length, na.rm = TRUE),
                        se_shoot_length = sd(shoot_length, na.rm = TRUE) / sqrt(n()), .groups = 'drop')

# Create the plot
plot <- ggplot(data_summary, aes(x = doy, y = mean_shoot_length, color = treat)) +
    geom_point() +
    geom_errorbar(aes(ymin = mean_shoot_length - se_shoot_length, ymax = mean_shoot_length + se_shoot_length), width = 0.2) +
    facet_wrap(~ treat)

# Print the plot
print(plot)



