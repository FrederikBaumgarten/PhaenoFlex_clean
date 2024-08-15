 ##======================================================================================================
##
##  Phaenoflex - Shoot elongation
##
## Date:           July 4th 2024
## Author:         Frederik Baumgarten
#======================================================================================================

rm(list=ls(all=TRUE))

 #------------------------------------------------------------------------------------------------------
 # Einstellungen
 #------------------------------------------------------------------------------------------------------
 # Set Working Directory
 setwd("/Users/frederik/github/PhaenoFlex_clean/analysis/input/shoot_elongation")
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
#remove problematic replicates based on comments
#remove TreeID "Acma_con_B1_R1" and "Potr_con_B3_R12"
data <- data %>%
  filter(!(TreeID == "Acma_con_B1_R1" | TreeID == "Potr_con_B3_R12"| TreeID == "Potr_con_B3_R13"| TreeID == "Sese_con_B3_R12"))

#remove Acma_con_B1_R2 because this seems to be way to high values (probably measurement error)
data <- data %>%
  filter(!(TreeID == "Acma_con_B1_R2"))


# Get the unique values of "doy" and "treeID"
doy_uni <- unique(data$doy) #these are the monitoring days
Tree_ID <- unique(data$TreeID) #these are the plants monitored
str(data)
## Select only controls
data <- data %>%
  filter(treat == "Control")

# remove POTR in species column
data<-data[data$spec!="POTR",] #Remove Potr and Thpl


# Loop through each species
for (s in unique(data$spec)) {
  # Get the unique monitoring days for the current species
  doy_uni_species <- doy_uni$doy_uni[doy_uni$spec == s]

  # Print the species name and its unique monitoring days
  cat("Species:", s, "\n")
  cat("Unique Monitoring Days:", doy_uni_species, "\n")
}

#calculate the mean and standard error of "adjusted_length" for every spec and doy
data_summary <- data %>%
  group_by(spec, doy) %>%
  summarise(mean_adjusted_length = mean(adjusted_length, na.rm = TRUE), se_adjusted_length = sd(adjusted_length, na.rm = TRUE)/sqrt(n()), n = n())

# Existing rectangles_data dataframe
rectangles_data <- data.frame(
  spec = rep("PRVI", 3), # Example for PRVI, repeat for other species as needed
  xmin = c(138, 174, 212),
  xmax = c(163, 191, 228),
  ymin = c(0, 0, 0),
  ymax = c(40, 40, 40)
)

# New coordinates for "ACMA"
acma_coords <- data.frame(
  spec = rep("ACMA", 3),
  xmin = c(143, 174, 212),
  xmax = c(158, 191, 228),
  ymin = c(0, 0, 0), 
  ymax = c(40, 40, 40)
)

# New coordinates for "BEPA"
bepa_coords <- data.frame(
  spec = rep("BEPA", 3),
  xmin = c(138, 174, 212),
  xmax = c(154, 188, 226),
  ymin = c(0, 0, 0), 
  ymax = c(40, 40, 40)
)
# New coordinates for "PICO"
pico_coords <- data.frame(
  spec = rep("PICO", 3),
  xmin = c(151, 174, 212),
  xmax = c(163, 185, 222),
  ymin = c(0, 0, 0), 
  ymax = c(40, 40, 40)
)
# New coordinates for "QUMA"
quma_coords <- data.frame(
  spec = rep("QUMA", 3),
  xmin = c(143, 174, 212),
  xmax = c(154, 191, 227),
  ymin = c(0, 0, 0), 
  ymax = c(40, 40, 40)
)
# New coordinates for "SESE"
sese_coords <- data.frame(
  spec = rep("SESE", 3),
  xmin = c(151, 174, 212),
  xmax = c(163, 191, 228),
  ymin = c(0, 0, 0), 
  ymax = c(40, 40, 40)
)
# Append new coordinates to the rectangles_data dataframe
rectangles_data <- rbind(rectangles_data, acma_coords, bepa_coords, pico_coords, quma_coords, sese_coords)

## Define custom labels for the facets
custom_labels <- c(ACMA = "Acer macrophyllum", BEPA = "Betula papyrifera", PICO = "Pinus contorta", POTR = "Populus trichocarpa", PRVI = "Prunus virginiana", QUMA = "Quercus garryana", SESE = "Sequoia sempervirens")

#order factor levels of species
data_summary$spec <- factor(data_summary$spec, levels = c("PRVI", "SESE", "ACMA", "BEPA", "QUMA", "PICO"))

# create a plot
plot <- ggplot(data_summary[data_summary$spec == "ACMA", ], aes(x = doy, y = mean_adjusted_length, color = spec)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_adjusted_length - se_adjusted_length, ymax = mean_adjusted_length + se_adjusted_length), width = 0.2) +
  geom_rect(data = rectangles_data[rectangles_data$spec == "ACMA", ], aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = spec), inherit.aes = FALSE, alpha = 0.2) +
  facet_wrap(~ spec, labeller = labeller(spec = custom_labels), scales = "free_y") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
      strip.text = element_text(face = "italic")) +
  labs(y = "shoot elongation (cm)")


# Print the plot
print(plot)

# Export the plot as a PDF
ggsave(filename = "/Users/frederik/github/PhaenoFlex_clean/analysis/output/shoot_elongation.pdf", plot = plot, device = "pdf", path = NULL, width = 8, height = 6)



#order factor levels of species
data_summary$spec <- factor(data_summary$spec, levels = c("PRVI", "SESE", "ACMA", "BEPA", "QUMA", "PICO"))

# multispecies plot
plot <- ggplot(data_summary, aes(x = doy, y = mean_adjusted_length, color = "#1c0800")) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_adjusted_length - se_adjusted_length, ymax = mean_adjusted_length + se_adjusted_length), width = 0.2) +
  geom_rect(data = rectangles_data, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "#f3c348", inherit.aes = FALSE, alpha = 0.2) +
  facet_wrap(~ spec, labeller = labeller(spec = custom_labels), scales = "free_y", ncol = 2) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        strip.text = element_text(face = "italic")) +
  labs(y = "Shoot elongation (cm)")

# Order the facets
plot <- plot + facet_wrap(~ spec, labeller = labeller(spec = custom_labels), scales = "free_y", ncol = 2, 
                          drop = FALSE, order = c("PRVI", "SESE", "ACMA", "BEPA", "QUMA", "PICO"))


# Adjust y-axis scale for each spec
plot <- plot + scale_y_continuous(limits = c(0, NA), expand = c(0, 0))

# Print the plot
print(plot)

# Export the plot as a PDF
ggsave(filename = "/Users/frederik/github/PhaenoFlex_clean/analysis/output/shoot_elongation_all_spp.pdf", plot = plot, device = "pdf", path = NULL, width = 5.5, height = 6)
