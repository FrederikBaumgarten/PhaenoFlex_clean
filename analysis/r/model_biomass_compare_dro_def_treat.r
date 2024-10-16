##======================================================================================================
##  PhaenoFlex
##  Biomass
# xxx
##
## Date:             July 4th 2024
## Author:           Frederik Baumgarten
#======================================================================================================

rm(list=ls(all=TRUE))

#------------------------------------------------------------------------------------------------------
# Einstellungen
#------------------------------------------------------------------------------------------------------
# Set Working Directory
setwd("/Users/frederik/github/PhaenoFlex_clean/analysis/input/biomass")
# Libraries ------------
library(raster)
library(readxl)
library(xlsx)
library(arm)
library(stats)
library(data.table)
library(geosphere)
library("ggplot2")
library("ggpubr")
library("grid")
library("tidyr")
library(ggplot2)
library(dplyr)
library(rstanarm)
library(shinystan)
library(tidybayes)
#library()

#functions
truelength<-function(x){return(length(which(is.na(x)==FALSE)))}
#-------------------------------------
### Daten einlesen ###################################
##Monitoring data---------
dat<-read_xlsx("phaenoflex_data_12_merge_Jan_28_FB.xlsx", sheet = "dat_full", col_names = T, na = "NA")
#dat<-readxl::read_excel("phaenoflex_data_12_merge_Jan_28_FB.xlsx", sheet = "dat_full", na = "NA") # nolint: line_length_linter.

summary(dat)
str(dat)
head(dat)

#Variablen bereinigen ---------------
str(dat)
dat$experiment = as.factor(dat$experiment)
dat$spec = as.factor(dat$spec)
dat$drought_timing = as.factor(dat$drought_timing)
dat$block<-as.factor(dat$block)

##is another control. For now I will exclude this treatment
dat[dat$treatment=="drought_4" & dat$spec=="Sese", "treatment"] <- NA
dat<-dat[!is.na(dat$treatment),]

#the other drought_4 treatments were actually used as the third defoliation treatments
dat[dat$treatment=="drought_4", "treatment"] <- "defol3"
unique(dat$drought_timing)
dat$treatment<-as.factor(dat$treatment)
dat$rep<-as.factor(dat$rep)

dat$diameter_1_init<-as.numeric(dat$diameter_1_init)
dat$diameter_2_init<-as.numeric(dat$diameter_2_init)
dat$diameter_3_init<-as.numeric(dat$diameter_3_init)
dat$height_1_init<-as.numeric(dat$height_1_init)
dat$height_2_init<-as.numeric(dat$height_2_init)
dat$height_3_init<-as.numeric(dat$height_3_init)
dat$diameter_1GS<-as.numeric(dat$diameter_1GS)
dat$height_1GS<-as.numeric(dat$height_1GS)
dat$height1G2<-as.numeric(dat$height1G2)
dat$biomass_root<-as.numeric(dat$biomass_root)
dat$biomass_old_shoot<-as.numeric(dat$biomass_old_shoot)
dat$biomass_new_shoot<-as.numeric(dat$biomass_new_shoot)
dat$biomass_adventitious_shoots<-as.numeric(dat$biomass_adventitious_shoots)
dat$number_adventitious_shoots<-as.numeric(dat$number_adventitious_shoots)

##Clean data
#Remove problematic replicates based on comments, see also shoot_elongation.R

#calculate total biomass
dat$biomass_tot <- dat$biomass_root + dat$biomass_old_shoot + dat$biomass_new_shoot + dat$biomass_adventitious_shoots

# data overview
tapply(dat$biomass_tot, paste(dat$spec, dat$treatment), function (x) truelength(x))


###Data selection #########
dat<-dat[dat$spec!="Potr" & dat$spec!="Thpl",] #Remove Potr and Thpl


#change names in dat$treamtent from "drought_1" to "drought1"
dat$treatment <- gsub("drought_1", "drought1", dat$treatment)
dat$treatment <- gsub("drought_2", "drought2", dat$treatment)
dat$treatment <- gsub("drought_3", "drought3", dat$treatment)
unique(dat$treatment)

#keep only control, drought and defoliation treatments
dat<-dat[dat$treatment %in% c("control", "drought1", "drought2", "drought3", "defol1", "defol2", "defol3"),]

################################################################################################################################
#Modeling
#make treatement as a categorical variable
dat$treatment<-as.factor(dat$treatment)

#remove other treatments for now
dat<-dat[dat$treatment %in% c("control", "drought1", "drought2", "drought3", "defol1", "defol2", "defol3"),]

#order treatment levels as follows: control, control_heat, GS_extend, GS_extend_heat, drought_2, drought_1, drought_3, defol1, defol2, defol3
dat$treatment <- factor(dat$treatment, levels = c("control", "drought1", "drought2", "drought3", "defol1", "defol2", "defol3"))

#make spec as a categorical variable
dat$spec<-as.factor(dat$spec)
dat$spec<- factor(dat$spec, levels = c("Prvi", "Acma", "Quma", "Bepa", "Pico", "Sese"))
species<-levels(dat$spec)
names(dat)
dat$treatment
#################################-----------
#fit first model: 
# Define the model formula
fit_1<-stan_glm(biomass_tot ~ treatment*spec, data=dat, prior_intercept=NULL, prior=NULL, prior_aux=NULL)

print(fit_1)
print(summary(fit_1), digits=2)

## shiny stan
# Convert the fitted model to a shinystan object
#shinystan_obj <- as.shinystan(fit_1)
#launch_shinystan(shinystan_obj)

# Posterior predictive checks
pp_check(fit_1)

# Extract the posterior draws
posterior_draws <- as_draws_df(fit_1)
str(posterior_draws)

# Define a function to calculate the effect for a particular treatment and species combination
calculate_effect <- function(intercept, treatment, species, interaction) {
  intercept + treatment + species + interaction
}

# Initialize a list to store results
posterior_effects <- list()

treatments<-levels(dat$treatment)

# Calculate the effect for each treatment and species combination
for (i in seq_along(species)) {
  species_name <- species[i]
  species_col <- ifelse(species_name == "Prvi", 0, posterior_draws[[paste0("spec", species_name)]])
  
  for (j in seq_along(treatments)) {
    treatment_name <- treatments[j]
    treatment_col <- ifelse(treatment_name == "control", 0, posterior_draws[[paste0("treatment", treatment_name)]])
    
    interaction_col <- if (species_name != "Prvi" & treatment_name != "control") {
      posterior_draws[[paste0("treatment", treatment_name, ":spec", species_name)]]
    } else {
      0
    }
    
    effect_name <- paste(treatment_name, species_name, sep = "_")
    posterior_effects[[effect_name]] <- calculate_effect(
      posterior_draws$`(Intercept)`,
      treatment_col,
      species_col,
      interaction_col
    )
  }
}

# Summarize the posterior distributions for each combination
summary_effects <- lapply(posterior_effects, function(x) {
  data.frame(
    mean = mean(x),
    sd = sd(x),
    q2.5 = quantile(x, 0.025),
    q97.5 = quantile(x, 0.975)
  )
})

# Convert the summary to a data frame
summary_effects_df <- do.call(rbind, summary_effects)
summary_effects_df <- cbind(Combination = rownames(summary_effects_df), summary_effects_df)
rownames(summary_effects_df) <- NULL

# Print the summarized effects
print(summary_effects_df)

#####################################
### Plotting

### 1. graph: plot the means±SD for each treatment level
# Create a summarized data frame for each treatment level and species
summary_effects_df <- summary_effects_df %>%
  mutate(
    species = sub(".*_", "", Combination),
    treatment = sub("_.*", "", Combination)
  )

#unique(summary_effects_df$treatment)
# Convert `treatment` and `species` to factors to maintain ordering in the plot
summary_effects_df$treatment <- factor(summary_effects_df$treatment, levels = c('control', 'drought1', 'drought2', 'drought3', 'defol1', 'defol2', 'defol3'))
summary_effects_df$species <- factor(summary_effects_df$species, levels = c("Prvi", "Acma", "Quma", "Bepa", "Pico", "Sese"))


# Custom colors for treatments
treatment_colors <- c("control" = "#b7c5cf", "drought1" = "#f69f2e", "drought2" = "#ac7021", "drought3" = "#6b4614", "defol1" = "#7bff1d", "defol2" = "#5fc516", "defol3" = "#408410")

# Define custom labels for the facets
custom_labels <- c(Acma = "Acer macrophyllum", Bepa = "Betula papyrifera", Pico = "Pinus contorta", Potr = "Populus trichocarpa", Prvi = "Prunus virginiana", Quma = "Quercus garryana", Sese = "Sequoia sempervirens")

# Create a ggplot of means ± SDs for each treatment level, faceted by species in a single column
biomass_post_treat<-ggplot(summary_effects_df, aes(x = treatment, y = mean, fill = treatment)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, position = position_dodge(0.9)) +
  facet_wrap(~ species, labeller = labeller(species = custom_labels), scales = "free_y", ncol = 1) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        strip.text = element_text(face = "italic")) +
  labs(x = "Treatment", y = "Total Biomass (Mean ± SD)") +
  scale_fill_manual(values = treatment_colors) +
  theme(axis.ticks.y = element_line(),  # Add tick marks for y-axis
        strip.text.x = element_text(face = "italic", hjust = 0, size = 10),
        axis.text.y = element_text(size = 8),
        axis.title = element_text(size = 12, hjust = 0.5),
        plot.title = element_text(size = 12, hjust = 0),
        strip.background = element_rect(color = "white", fill = "white"),
        legend.position = "none",  # Remove legend
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_discrete(labels = function(x) {
    ifelse(x == "control", "Con",
           ifelse(x == "dro1", "Dro_1",
                  ifelse(x == "dro2", "Dro_2",
                         ifelse(x == "dro3", "Dro_3",
                                ifelse(x == "def1", "Def_1",
                                       ifelse(x == "def2", "Def_2",
                                              ifelse(x == "def3", "Def_3", x)))))))
  }) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

  # Export the plot as a PDF
ggsave(filename = "/Users/frederik/github/PhaenoFlex_clean/analysis/output/posteriors/biomass_post_treat.pdf", plot = biomass_post_treat, device = "pdf", path = NULL, width = 3, height = 7)





### 2. graph: plot the means±SD for each treatment level in comparison to control

#extract control
summary_effects_df[summary_effects_df$Combination == "control_Prvi", "mean"]

#identify all treatments per species and substract the mean control from all other means
#make new df
summary_effects_df_sub<-summary_effects_df
summary_effects_df_sub$mean_sub<-NA
spec<-c("Prvi", "Acma", "Quma", "Bepa", "Pico", "Sese")
for(i in 1:length(spec)){
    tmp<-summary_effects_df[grepl(spec[i], summary_effects_df$Combination), "mean"] - summary_effects_df[summary_effects_df$Combination==paste("control", spec[i], sep = "_"), "mean"]
                                                                                      
    summary_effects_df_sub[grepl(spec[i], summary_effects_df$Combination), "mean_sub"]<- tmp
}

str(summary_effects_df_sub)
#assign NA to controls
summary_effects_df_sub[grepl("control_", summary_effects_df$Combination), ]<-NA

#Remove all treatments with NAs
summary_effects_df_sub<-summary_effects_df_sub[!is.na(summary_effects_df_sub$mean_sub),]

#####################################
### Plotting

### 2. graph: plot the means±SD for each treatment level
# Create a summarized data frame for each treatment level and species
#summary_effects_df_sub <- summary_effects_df_sub %>%
#  mutate(
#    species = sub(".*_", "", Combination),
#    treatment = sub("_.*", "", Combination)
#  )

#unique(summary_effects_df$treatment)
# Convert `treatment` and `species` to factors to maintain ordering in the plot
summary_effects_df_sub$treatment <- factor(summary_effects_df_sub$treatment, levels = c("drought1", "drought2", "drought3", "defol1", "defol2", "defol3"))
summary_effects_df_sub$species <- factor(summary_effects_df_sub$species, levels = c("Prvi", "Acma", "Quma", "Bepa", "Pico", "Sese"))


# Custom colors for treatments
treatment_colors <- c("drought1" = "#b7c5cf", "drought2" = "#f69f2e", "drought3" = "#7bff1d", "defol1" = "#ea3939", "defol2" = "#91a417", "defol3" =  "#b8c46a")
species_colors<- c("Prvi" = "#b7c5cf", "Acma" = "#f69f2e", "Quma" = "#7bff1d", "Bepa" = "#ea3939", "Pico" = "#91a417", "Sese" = "#67702b")

# Define custom labels for the facets
custom_labels <- c(Acma = "Acer macrophyllum", Bepa = "Betula papyrifera", Pico = "Pinus contorta", Potr = "Populus trichocarpa", Prvi = "Prunus virginiana", Quma = "Quercus garryana", Sese = "Sequoia sempervirens")
position_dodge_width <- 0.6 # Adjust position_dodge to cluster species within a treatment

# Create a ggplot of means ± SDs for each treatment level, faceted by species in a single column
biomass_post_treat_sub <- ggplot(summary_effects_df_sub, aes(y = treatment, x = mean_sub, color = species)) +
  geom_point(position = position_dodge(width = position_dodge_width), size = 3) +  # Use points
  geom_errorbar(aes(xmin = mean_sub - sd, xmax = mean_sub + sd), width = 0, position = position_dodge(width = position_dodge_width)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +  # Dashed line at x = 0
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    strip.text = element_text(face = "italic"),
    axis.ticks.x = element_line(),  # Add tick marks for x-axis
    strip.text.x = element_text(face = "italic", hjust = 0, size = 10),
    axis.text.x = element_text(size = 8),
    axis.title = element_text(size = 12, hjust = 0.5),
    plot.title = element_text(size = 12, hjust = 0),
    strip.background = element_rect(color = "white", fill = "white"),
    #legend.position = "none",  # Remove legend
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_color_manual(values = species_colors) +  # Use species-specific colors
  scale_y_discrete(labels = function(y) {
    ifelse(y == "defol1", "Defoliation Budburst",
    ifelse(y == "defol2", "Defoliation Solstice",
    ifelse(y == "defol3", "Defoliation August",
    ifelse(y == "drought1", "Drought Budburst",
    ifelse(y == "drought2", "Drought Solstice",
    ifelse(y == "drought3", "Drought August",
    ifelse(y == "def3", "Def_3", y)))))))
  }) +
  labs(y = "Treatment", x = "Change to control (Mean ± SD g of total Biomass)") +
  theme(axis.text.y = element_text(size = 8))

# Export the plot as a PDF
ggsave(filename = "/Users/frederik/github/PhaenoFlex_clean/analysis/output/posteriors/biomass_post_treat_sub.pdf", plot = biomass_post_treat_sub, width = 4, height = 5)

