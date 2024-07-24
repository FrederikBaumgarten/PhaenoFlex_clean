#=== === === === === === === === === === === === === === === === === === === ===
##  PhaenoFlex Soil moisture - Drought 1 and 2
##  
##
## Project:         PhaenoFlex
## Datum:           06.03.2023
## Autor:           Christophe & Frederik
#=== === === === === === === === === === === === === === === === === === === ===

# housekeeping
rm(list=ls())  
options(stringsAsFactors=FALSE)

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
# Set the path to your directory folder 
directory_path <- "/Users/christophe_rouleau-desrochers/Documents/github/PhaenoFlex/drought_treatment/R"
# Set Working Directory
setwd(directory_path)
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
#package
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(gridExtra)
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --

#=== === === === === === === === === === === === === === === === === === === ===
# Specifications:
# the "M" is for moisture, meaning it's the subset data for values of volumetric
# water content. The W is for weight, meaning the water loss
# prvi: prunus
# acma: acer
# bepa: betula 
# pico: pinus
# quma: quercus 
# sese: sequoia
#=== === === === === === === === === === === === === === === === === === === ===



#### VOLUMETRIC WATER CONTENT - DROUGHT 1 ####

# read excel file
# volumetric water content
vwc_drought_1<-read_excel("raw_data/dro1_soilmoisture.xlsx", sheet="vwc_drought_1")


###create vector of julian days that is length of the number of julian days
julianM<-colnames(vwc_drought_1)[3:length(colnames(vwc_drought_1))]

##### Prvi #####
#Subset
prviM_subset<-subset(vwc_drought_1, species=='Prvi')
# rotate the table
prviM_rot<- t(prviM_subset)
# convert to data frame
prviM_dro1 <- as.data.frame(prviM_rot, stringsAsFactors = FALSE)
# Set the first row as column names
colnames(prviM_dro1) <- prviM_dro1[1,]
# Remove the first 2 rows
prviM_dro1 <- prviM_dro1[-c(1:2),]
#add julian column to df
prviM_dro1['juliandays']<-julianM
#convert to numeric
for (i in 1:length(colnames(prviM_dro1))) {
  prviM_dro1[,i]<-as.numeric(prviM_dro1[,i])
}
#delete na rows with dates that we have no data
prviM_dro1 <- prviM_dro1[rowSums(!is.na(prviM_dro1) & prviM_dro1 != "") > 1, ]

#Create a data frame for plotting
prviM_df <- data.frame(
  x = prviM_dro1$juliandays,
  y = as.numeric(unlist(prviM_dro1[, grepl("^Prvi", colnames(prviM_dro1))])),
  column = rep(colnames(prviM_dro1)[-ncol(prviM_dro1)], each = nrow(prviM_dro1))
)

# Calculate the mean curve
mean_curve_prviM <- prviM_df %>%
  group_by(x) %>%
  summarise(mean_value = mean(y, na.rm=TRUE))

print_prviM<-ggplot()+
  geom_point(data=prviM_df, aes(x=x, y=y, color=column, show.legend=FALSE), size = 1) +
  geom_line(data=mean_curve_prviM, aes(x=x, y=mean_value), color="black", size=1)+
  labs(x="", y="")+
  ggtitle("Prvi VWC DRO1")+
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")
print_prviM<-print_prviM + theme(legend.position = "none")
print_prviM
ggsave("output/dro1/soil_moist_prviM.pdf", print_prviM, dpi = 300, width = 4, height = 4)
head(prviM_df)
#####Acma#####
#Subset
acmaM_subset<-subset(vwc_drought_1, species=='Acma')
# rotate the table
acma_rot<- t(acmaM_subset)
# convert to data frame
acmaM_dro1 <- as.data.frame(acma_rot, stringsAsFactors = FALSE)
# Set the first row as column names
colnames(acmaM_dro1) <- acmaM_dro1[1,]
# Remove the first 2 rows
acmaM_dro1 <- acmaM_dro1[-(1:2),]
#add julian column to df
acmaM_dro1['juliandays']<-julianM
#convert to numeric
for (i in 1:length(colnames(acmaM_dro1))) {
  acmaM_dro1[,i]<-as.numeric(acmaM_dro1[,i])
}
#delete na rows with dates that we have no data
acmaM_dro1 <- acmaM_dro1[rowSums(!is.na(acmaM_dro1) & acmaM_dro1 != "") > 1, ]


#Create a data frame for plotting
acmaM_df <- data.frame(
  x = acmaM_dro1$juliandays,
  y = as.numeric(unlist(acmaM_dro1[, grepl("^Acma", colnames(acmaM_dro1))])),
  column = rep(colnames(acmaM_dro1)[-ncol(acmaM_dro1)], each = nrow(acmaM_dro1))
)

# Calculate the mean curve
mean_curve_acmaM <- acmaM_df %>%
  group_by(x) %>%
  summarise(mean_value = mean(y, na.rm=TRUE))

print_acmaM<-ggplot()+
  geom_point(data=acmaM_df, aes(x=x, y=y, color=column, show.legend=FALSE), size = 1) +
  geom_line(data=mean_curve_acmaM, aes(x=x, y=mean_value), color="black", size=1)+
  labs(x="", y="")+
  ggtitle("Acma VWC DRO1")+
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")
print_acmaM<-print_acmaM + theme(legend.position = "none")
ggsave("output/dro1/soil_moist_acmaM.pdf", print_acmaM, dpi = 300, width = 4, height = 4)


#####Bepa#####
#Subset
bepaM_subset<-subset(vwc_drought_1, species=='Bepa')
# rotate the table
bepa_rot<- t(bepaM_subset)
# convert to data frame
bepaM_dro1 <- as.data.frame(bepa_rot, stringsAsFactors = FALSE)
# Set the first row as column names
colnames(bepaM_dro1) <- bepaM_dro1[1,]
# Remove the first 2 rows
bepaM_dro1 <- bepaM_dro1[-(1:2),]
#add julian column to df
bepaM_dro1['juliandays']<-julianM
#convert to numeric
for (i in 1:length(colnames(bepaM_dro1))) {
  bepaM_dro1[,i]<-as.numeric(bepaM_dro1[,i])
}

####temporaly delete na rows
bepaM_dro1 <- bepaM_dro1[rowSums(!is.na(bepaM_dro1) & bepaM_dro1 != "") > 1, ]


#Create a data frame for plotting
bepaM_df <- data.frame(
  x = bepaM_dro1$juliandays,
  y = as.numeric(unlist(bepaM_dro1[, grepl("^Bepa", colnames(bepaM_dro1))])),
  column = rep(colnames(bepaM_dro1)[-ncol(bepaM_dro1)], each = nrow(bepaM_dro1))
)

# Calculate the mean curve
mean_curve_bepaM <- bepaM_df %>%
  group_by(x) %>%
  summarise(mean_value = mean(y, na.rm=TRUE))

print_bepaM<-ggplot()+
  geom_point(data=bepaM_df, aes(x=x, y=y, color=column, show.legend=FALSE), size = 1) +
  geom_line(data=mean_curve_bepaM, aes(x=x, y=mean_value), color="black", size=1)+
  labs(x="", y="")+
  ggtitle("Bepa VWC DRO1")+
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")
print_bepaM<-print_bepaM + theme(legend.position = "none")
ggsave("output/dro1/soil_moist_bepaM.pdf", print_bepaM, dpi = 300, width = 4, height = 4)


#####Pico#####
#Subset
picoM_subset<-subset(vwc_drought_1, species=='Pico')
# rotate the table
pico_rot<- t(picoM_subset)
# convert to data frame
picoM_dro1 <- as.data.frame(pico_rot, stringsAsFactors = FALSE)
# Set the first row as column names
colnames(picoM_dro1) <- picoM_dro1[1,]
# Remove the first 2 rows
picoM_dro1 <- picoM_dro1[-(1:2),]
#add julian column to df
picoM_dro1['juliandays']<-julianM
#convert to numeric
for (i in 1:length(colnames(picoM_dro1))) {
  picoM_dro1[,i]<-as.numeric(picoM_dro1[,i])
}
#delete na rows with dates that we have no data
picoM_dro1 <- picoM_dro1[rowSums(!is.na(picoM_dro1) & picoM_dro1 != "") > 1, ]


#Create a data frame for plotting
picoM_df <- data.frame(
  x = picoM_dro1$juliandays,
  y = as.numeric(unlist(picoM_dro1[, grepl("^Pico", colnames(picoM_dro1))])),
  column = rep(colnames(picoM_dro1)[-ncol(picoM_dro1)], each = nrow(picoM_dro1))
)

# Calculate the mean curve
mean_curve_picoM <- picoM_df %>%
  group_by(x) %>%
  summarise(mean_value = mean(y, na.rm=TRUE))

print_picoM<-ggplot()+
  geom_point(data=picoM_df, aes(x=x, y=y, color=column, show.legend=FALSE), size = 1) +
  geom_line(data=mean_curve_picoM, aes(x=x, y=mean_value), color="black", size=1)+
  labs(x="", y="")+
  ggtitle("Pico VWC DRO1")+
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")
print_picoM<-print_picoM + theme(legend.position = "none")
ggsave("output/dro1/soil_moist_picoM.pdf", print_picoM, dpi = 300, width = 4, height = 4)


#####Quma#####
#Subset
qumaM_subset<-subset(vwc_drought_1, species=='Quma')
# rotate the table
quma_rot<- t(qumaM_subset)
# convert to data frame
qumaM_dro1 <- as.data.frame(quma_rot, stringsAsFactors = FALSE)
# Set the first row as column names
colnames(qumaM_dro1) <- qumaM_dro1[1,]
# Remove the first 2 rows
qumaM_dro1 <- qumaM_dro1[-(1:2),]
#add julian column to df
qumaM_dro1['juliandays']<-julianM
#convert to numeric
for (i in 1:length(colnames(qumaM_dro1))) {
  qumaM_dro1[,i]<-as.numeric(qumaM_dro1[,i])
}
#delete na rows with dates that we have no data
qumaM_dro1 <- qumaM_dro1[rowSums(!is.na(qumaM_dro1) & qumaM_dro1 != "") > 1, ]


#Create a data frame for plotting
qumaM_df <- data.frame(
  x = qumaM_dro1$juliandays,
  y = as.numeric(unlist(qumaM_dro1[, grepl("^Quma", colnames(qumaM_dro1))])),
  column = rep(colnames(qumaM_dro1)[-ncol(qumaM_dro1)], each = nrow(qumaM_dro1))
)

# Calculate the mean curve
mean_curve_qumaM <- qumaM_df %>%
  group_by(x) %>%
  summarise(mean_value = mean(y, na.rm=TRUE))

print_qumaM<-ggplot()+
  geom_point(data=qumaM_df, aes(x=x, y=y, color=column, show.legend=FALSE), size = 1) +
  geom_line(data=mean_curve_qumaM, aes(x=x, y=mean_value), color="black", size=1)+
  labs(x="", y="")+
  ggtitle("Quma VWC DRO1")+
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")
print_qumaM<-print_qumaM + theme(legend.position = "none")
print_qumaM
ggsave("output/dro1/soil_moist_qumaM.pdf", print_qumaM, dpi = 300, width = 4, height = 4)


#####Sese####
#Subset
seseM_subset<-subset(vwc_drought_1, species=='Sese')
# rotate the table
sese_rot<- t(seseM_subset)
# convert to data frame
seseM_dro1 <- as.data.frame(sese_rot, stringsAsFactors = FALSE)
# Set the first row as column names
colnames(seseM_dro1) <- seseM_dro1[1,]
# Remove the first 2 rows
seseM_dro1 <- seseM_dro1[-(1:2),]
#add julian column to df
seseM_dro1['juliandays']<-julianM
#convert to numeric
for (i in 1:length(colnames(seseM_dro1))) {
  seseM_dro1[,i]<-as.numeric(seseM_dro1[,i])
}
#delete na rows with dates that we have no data
seseM_dro1 <- seseM_dro1[rowSums(!is.na(seseM_dro1) & seseM_dro1 != "") > 1, ]


#Create a data frame for plotting
seseM_df <- data.frame(
  x = seseM_dro1$juliandays,
  y = as.numeric(unlist(seseM_dro1[, grepl("^Sese", colnames(seseM_dro1))])),
  column = rep(colnames(seseM_dro1)[-ncol(seseM_dro1)], each = nrow(seseM_dro1))
)

# Calculate the mean curve
mean_curve_seseM <- seseM_df %>%
  group_by(x) %>%
  summarise(mean_value = mean(y, na.rm=TRUE))

print_seseM<-ggplot()+
  geom_point(data=seseM_df, aes(x=x, y=y, color=column, show.legend=FALSE), size = 1) +
  geom_line(data=mean_curve_seseM, aes(x=x, y=mean_value), color="black", size=1)+
  labs(x="", y="")+
  ggtitle("Sese VWC DRO1")+
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")
print_seseM<-print_seseM + theme(legend.position = "none")
print_seseM
ggsave("output/dro1/soil_moist_seseM.pdf", print_seseM, dpi = 300, width = 4, height = 4)




#### WEIGHT - DROUGHT 1 ####

#read excel file
weight_drought_1<-read_excel("raw_data/dro1_soilmoisture.xlsx", sheet="weight_drought_1")


###create vector of julian days that is length of the number of julian days
julianW<-colnames(weight_drought_1)[3:length(colnames(weight_drought_1))]


##### Prvi #####
prviW <- subset(weight_drought_1, species=="Prvi")
str(prviW)
# rotate the table
prviW_Dro1<-t(prviW)
# convert to data frame
prviW_Dro1 <- as.data.frame(prviW_Dro1, stringsAsFactors = FALSE)
# Set the first row as column names
colnames(prviW_Dro1) <- prviW_Dro1[1, ]
# Remove the first two rows
prviW_Dro1 <- prviW_Dro1[-c(1:2),]

#add julian column to df
prviW_Dro1['juliandays']<-julianW
# Remove rows that are all empty or all NA values
# prviW_Dro1 <- prviW_Dro1[rowSums(!is.na(prviW_Dro1) & prviW_Dro1 != "") > 1, ]
head(prviW_Dro1)
str(prviW_Dro1)
######make all columns numeric
for (i in 1:length(colnames(prviW_Dro1))) {
  prviW_Dro1[,i]<-as.numeric(prviW_Dro1[,i])
}
colnames(prviW_Dro1)<- gsub("\r\n", "", colnames(prviW_Dro1))

###### Change Original values by R0 from negative exponential curve model ######
exp_prvi_weight <- prviW_Dro1[, c(1:10)]
exp_prvi_weight$daysinchambers <- c(0:(length(exp_prvi_weight$Prvi_Dro1_B1_R1)-1))

# Create an empty data frame
exp_df_prvi_weight<- tibble(id = colnames(exp_prvi_weight)[-length(colnames(exp_prvi_weight))], Asym = NA, R0 = NA, lrc = NA)

# Loop through every replicates to extract their coefficients and store them in the df
for (i in 1:length(exp_prvi_weight)) {
  tmp <- exp_prvi_weight[, i]  
  model <- nls(tmp ~ SSasymp(daysinchambers, Asym, R0, lrc), data = exp_prvi_weight)
  exp_df_prvi_weight[i, "Asym"] <- summary(model)$coefficients["Asym", "Estimate"]
  exp_df_prvi_weight[i, "R0"] <- summary(model)$coefficients["R0", "Estimate"]
  exp_df_prvi_weight[i, "lrc"] <- summary(model)$coefficients["lrc", "Estimate"]
}
exp_df_prvi_weight

# Loop through each replicate initial values
for (i in 1: length(exp_prvi_weight[,1:10])) {
  exp_prvi_weight[1, i] <- exp_df_prvi_weight[i, "R0"]
}

### calculate percentages to initial weight
tmp<-colnames(exp_prvi_weight)[1:length(colnames(exp_prvi_weight))]
for (i in 1:length(tmp)) {
  exp_prvi_weight[,paste(tmp[i],"perc", sep="_")]<-NA
  exp_prvi_weight[,paste(tmp[i],"perc", sep="_")]<-(100*exp_prvi_weight[,tmp[i]])/exp_prvi_weight[1,tmp[i]]
}

# Drop column "juliandays_perc"
exp_prvi_weight<-(exp_prvi_weight[ ,  !names(exp_prvi_weight) %in% 
                           c("daysinchambers_perc")])
# Filter columns that contain "perc"
perc_columns_prvi <- grep("perc", colnames(exp_prvi_weight), value = TRUE)


# Reshape the data frame to long format
df_long_prvi <- exp_prvi_weight %>%
  gather(column, value, all_of(perc_columns_prvi))

# Calculate the mean curve
mean_curve_prvi <- df_long_prvi %>%
  group_by(daysinchambers) %>%
  summarise(mean_value = mean(value))

# Plotting the data points and mean curve
print_prviW<-ggplot() +
  geom_point(data = df_long_prvi, aes(x = daysinchambers, y = value, color = column, show.legend = FALSE), size = 1) +
  geom_line(data = mean_curve_prvi, aes(x = daysinchambers, y = mean_value), color = "black", size = 1) +
  labs(x = "", y = "") +
  ggtitle("Prvi WL DRO1 Adjusted") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "")
print_prviW
head(df_long_prvi)
ggsave("output/dro1/soil_moist_prviW_adjusted.pdf", print_prviW, dpi = 300, width = 4, height = 4)

#####Acma#####
#Subset Acma
acmaW <- subset(weight_drought_1, species=="Acma")
# rotate the table
acmaW_Dro1<- t(acmaW)
# convert to data frame
acmaW_Dro1 <- as.data.frame(acmaW_Dro1, stringsAsFactors = FALSE)
# Set the first row as column names
colnames(acmaW_Dro1) <- acmaW_Dro1[1, ]
# Remove the first two rows
acmaW_Dro1 <- acmaW_Dro1[-c(1,2),]
#add julian column to df
acmaW_Dro1['juliandays']<-julianW
# Remove rows that are all empty or all NA values
acmaW_Dro1 <- acmaW_Dro1[rowSums(!is.na(acmaW_Dro1) & acmaW_Dro1 != "") > 1, ]

######make all columns numeric
for (i in 1:length(colnames(acmaW_Dro1))) {
  acmaW_Dro1[,i]<-as.numeric(acmaW_Dro1[,i])
}
###### Change Original values by R0 from negative exponential curve model ######
exp_acma_weight <- acmaW_Dro1[, c(1:5,11:15)]
exp_acma_weight$daysinchambers <- c(0:(length(exp_acma_weight$Acma_Dro1_B1_R1)-1))

# Create an empty data frame
exp_df_acma_weight<- tibble(id = colnames(exp_acma_weight)[-length(colnames(exp_acma_weight))], Asym = NA, R0 = NA, lrc = NA)

# Loop through every replicates to extract their coefficients and store them in the df
for (i in 1:length(exp_acma_weight)) {
  tmp <- exp_acma_weight[, i]  
  model <- nls(tmp ~ SSasymp(daysinchambers, Asym, R0, lrc), data = exp_acma_weight)
  exp_df_acma_weight[i, "Asym"] <- summary(model)$coefficients["Asym", "Estimate"]
  exp_df_acma_weight[i, "R0"] <- summary(model)$coefficients["R0", "Estimate"]
  exp_df_acma_weight[i, "lrc"] <- summary(model)$coefficients["lrc", "Estimate"]
}
exp_df_acma_weight

# Loop through each replicate initial values
for (i in 1: length(exp_acma_weight[,1:10])) {
  exp_acma_weight[1, i] <- exp_df_acma_weight[i, "R0"]
}

### calculate percentages to initial weight
tmp<-colnames(exp_acma_weight)[1:length(colnames(exp_acma_weight))]
for (i in 1:length(tmp)) {
  exp_acma_weight[,paste(tmp[i],"perc", sep="_")]<-NA
  exp_acma_weight[,paste(tmp[i],"perc", sep="_")]<-(100*exp_acma_weight[,tmp[i]])/exp_acma_weight[1,tmp[i]]
}

# Drop column "juliandays_perc"
exp_acma_weight<-(exp_acma_weight[ ,  !names(exp_acma_weight) %in% 
                                     c("daysinchambers_perc")])
# Filter columns that contain "perc"
perc_columns_acma <- grep("perc", colnames(exp_acma_weight), value = TRUE)

# Reshape the data frame to long format
df_long_acma <- exp_acma_weight %>%
  gather(column, value, all_of(perc_columns_acma))

# Calculate the mean curve
mean_curve_acma <- df_long_acma %>%
  group_by(daysinchambers) %>%
  summarise(mean_value = mean(value))

# Plotting the data points and mean curve
print_acmaW<-ggplot() +
  geom_point(data = df_long_acma, aes(x = daysinchambers, y = value, color = column, show.legend = FALSE), size = 1) +
  geom_line(data = mean_curve_acma, aes(x = daysinchambers, y = mean_value), color = "black", size = 1) +
  labs(x = "", y = "") +
  ggtitle("Acma WL DRO1 Adjusted") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "")
print_acmaW
ggsave("output/dro1/soil_moist_acmaW_adjusted.pdf", print_acmaW, dpi = 300, width = 4, height = 4)



##### Bepa #####
#Subset Bepa
bepaW <- subset(weight_drought_1, species=="Bepa")
# rotate the table
bepaW_Dro1<- t(bepaW)
# convert to data frame
bepaW_Dro1 <- as.data.frame(bepaW_Dro1, stringsAsFactors = FALSE)
# Set the first row as column names
colnames(bepaW_Dro1) <- bepaW_Dro1[1, ]
# Remove the first two rows
bepaW_Dro1 <- bepaW_Dro1[-c(1,2),]
#add julian column to df
bepaW_Dro1['juliandays']<-julianW
# Remove rows that have no values
bepaW_Dro1 <- bepaW_Dro1[c(1:19),]

######make all columns numeric
for (i in 1:length(colnames(bepaW_Dro1))) {
  bepaW_Dro1[,i]<-as.numeric(bepaW_Dro1[,i])
}
###### Change Original values by R0 from negative exponential curve model ######
exp_bepa_weight <- bepaW_Dro1[, c(1:5, 11:15)]
exp_bepa_weight$daysinchambers <- c(0:(length(exp_bepa_weight$Bepa_Dro1_B1_R1)-1))

# Create an empty data frame
exp_df_bepa_weight<- tibble(id = colnames(exp_bepa_weight)[-length(colnames(exp_bepa_weight))], Asym = NA, R0 = NA, lrc = NA)

# Loop through every replicates to extract their coefficients and store them in the df
for (i in 1:length(exp_bepa_weight)) {
  tmp <- exp_bepa_weight[, i]  
  model <- nls(tmp ~ SSasymp(daysinchambers, Asym, R0, lrc), data = exp_bepa_weight)
  exp_df_bepa_weight[i, "Asym"] <- summary(model)$coefficients["Asym", "Estimate"]
  exp_df_bepa_weight[i, "R0"] <- summary(model)$coefficients["R0", "Estimate"]
  exp_df_bepa_weight[i, "lrc"] <- summary(model)$coefficients["lrc", "Estimate"]
}
exp_df_bepa_weight

# Loop through each replicate initial values
for (i in 1: length(exp_bepa_weight[,1:10])) {
  exp_bepa_weight[1, i] <- exp_df_bepa_weight[i, "R0"]
}

### calculate percentages to initial weight
tmp<-colnames(exp_bepa_weight)[1:length(colnames(exp_bepa_weight))]
for (i in 1:length(tmp)) {
  exp_bepa_weight[,paste(tmp[i],"perc", sep="_")]<-NA
  exp_bepa_weight[,paste(tmp[i],"perc", sep="_")]<-(100*exp_bepa_weight[,tmp[i]])/exp_bepa_weight[1,tmp[i]]
}

# Drop column "juliandays_perc"
exp_bepa_weight<-(exp_bepa_weight[ ,  !names(exp_bepa_weight) %in% 
                                     c("daysinchambers_perc")])
# Filter columns that contain "perc"
perc_columns_bepa <- grep("perc", colnames(exp_bepa_weight), value = TRUE)

# Reshape the data frame to long format
df_long_bepa <- exp_bepa_weight %>%
  gather(column, value, all_of(perc_columns_bepa))

# Calculate the mean curve
mean_curve_bepa <- df_long_bepa %>%
  group_by(daysinchambers) %>%
  summarise(mean_value = mean(value))

# Plotting the data points and mean curve
print_bepaW<-ggplot() +
  geom_point(data = df_long_bepa, aes(x = daysinchambers, y = value, color = column, show.legend = FALSE), size = 1) +
  geom_line(data = mean_curve_bepa, aes(x = daysinchambers, y = mean_value), color = "black", size = 1) +
  labs(x = "", y = "") +
  ggtitle("Bepa WL DRO1 Adjusted") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "")
print_bepaW
ggsave("output/dro1/soil_moist_bepaW_adjusted.pdf", print_bepaW, dpi = 300, width = 4, height = 4)


#####Pico#####
#Subset Pico
picoW <- subset(weight_drought_1, species=="Pico")
# rotate the table
picoW_Dro1<- t(picoW)
# convert to data frame
picoW_Dro1 <- as.data.frame(picoW_Dro1, stringsAsFactors = FALSE)
# Set the first row as column names
colnames(picoW_Dro1) <- picoW_Dro1[1, ]
# Remove the first two rows
picoW_Dro1 <- picoW_Dro1[-c(1,2),]
#add julian column to df
picoW_Dro1['juliandays']<-julianW
# Remove rows that are all empty or all NA values
picoW_Dro1 <- picoW_Dro1[rowSums(!is.na(picoW_Dro1) & picoW_Dro1 != "") > 1, ]


######make all columns numeric
for (i in 1:length(colnames(picoW_Dro1))) {
  picoW_Dro1[,i]<-as.numeric(picoW_Dro1[,i])
}
###### Change Original values by R0 from negative exponential curve model ######
exp_pico_weight <- picoW_Dro1[, c(1:5, 11:15)]
exp_pico_weight$daysinchambers <- c(0:(length(exp_pico_weight$Pico_Dro1_B1_R1)-1))

# Create an empty data frame
exp_df_pico_weight<- tibble(id = colnames(exp_pico_weight)[-length(colnames(exp_pico_weight))], Asym = NA, R0 = NA, lrc = NA)

# Loop through every replicates to extract their coefficients and store them in the df
for (i in 1:length(exp_pico_weight)) {
  tmp <- exp_pico_weight[, i]  
  model <- nls(tmp ~ SSasymp(daysinchambers, Asym, R0, lrc), data = exp_pico_weight)
  exp_df_pico_weight[i, "Asym"] <- summary(model)$coefficients["Asym", "Estimate"]
  exp_df_pico_weight[i, "R0"] <- summary(model)$coefficients["R0", "Estimate"]
  exp_df_pico_weight[i, "lrc"] <- summary(model)$coefficients["lrc", "Estimate"]
}
exp_df_pico_weight

# Loop through each replicate initial values
for (i in 1: length(exp_pico_weight[,1:10])) {
  exp_pico_weight[1, i] <- exp_df_pico_weight[i, "R0"]
}

### calculate percentages to initial weight
tmp<-colnames(exp_pico_weight)[1:length(colnames(exp_pico_weight))]
for (i in 1:length(tmp)) {
  exp_pico_weight[,paste(tmp[i],"perc", sep="_")]<-NA
  exp_pico_weight[,paste(tmp[i],"perc", sep="_")]<-(100*exp_pico_weight[,tmp[i]])/exp_pico_weight[1,tmp[i]]
}

# Drop column "juliandays_perc"
exp_pico_weight<-(exp_pico_weight[ ,  !names(exp_pico_weight) %in% 
                                     c("daysinchambers_perc")])
# Filter columns that contain "perc"
perc_columns_pico <- grep("perc", colnames(exp_pico_weight), value = TRUE)

# Reshape the data frame to long format
df_long_pico <- exp_pico_weight %>%
  gather(column, value, all_of(perc_columns_pico))

# Calculate the mean curve
mean_curve_pico <- df_long_pico %>%
  group_by(daysinchambers) %>%
  summarise(mean_value = mean(value))

# Plotting the data points and mean curve
print_picoW<-ggplot() +
  geom_point(data = df_long_pico, aes(x = daysinchambers, y = value, color = column, show.legend = FALSE), size = 1) +
  geom_line(data = mean_curve_pico, aes(x = daysinchambers, y = mean_value), color = "black", size = 1) +
  labs(x = "", y = "") +
  ggtitle("Pico WL DRO1 Adjusted") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "")
print_picoW
ggsave("output/dro1/soil_moist_picoW_adjusted.pdf", print_picoW, dpi = 300, width = 4, height = 4)

#####Quma#####
#Subset Quma
qumaW <- subset(weight_drought_1, species=="Quma")
# rotate the table
qumaW_Dro1<- t(qumaW)
# convert to data frame
qumaW_Dro1 <- as.data.frame(qumaW_Dro1, stringsAsFactors = FALSE)
# Set the first row as column names
colnames(qumaW_Dro1) <- qumaW_Dro1[1, ]
# Remove the first two rows
qumaW_Dro1 <- qumaW_Dro1[-c(1,2),]
#add julian column to df
qumaW_Dro1['juliandays']<-julianW
# Remove rows that are all empty or all NA values
qumaW_Dro1 <- qumaW_Dro1[rowSums(!is.na(qumaW_Dro1) & qumaW_Dro1 != "") > 1, ]


######make all columns numeric
for (i in 1:length(colnames(qumaW_Dro1))) {
  qumaW_Dro1[,i]<-as.numeric(qumaW_Dro1[,i])
}
###### Change Original values by R0 from negative exponential curve model ######
exp_quma_weight <- qumaW_Dro1[, c(1:5,11:15)]
exp_quma_weight$daysinchambers <- c(0:(length(exp_quma_weight$Quma_Dro1_B1_R1)-1))

# Create an empty data frame
exp_df_quma_weight<- tibble(id = colnames(exp_quma_weight)[-length(colnames(exp_quma_weight))], Asym = NA, R0 = NA, lrc = NA)

# Loop through every replicates to extract their coefficients and store them in the df
for (i in 1:length(exp_quma_weight)) {
  tmp <- exp_quma_weight[, i]  
  model <- nls(tmp ~ SSasymp(daysinchambers, Asym, R0, lrc), data = exp_quma_weight)
  exp_df_quma_weight[i, "Asym"] <- summary(model)$coefficients["Asym", "Estimate"]
  exp_df_quma_weight[i, "R0"] <- summary(model)$coefficients["R0", "Estimate"]
  exp_df_quma_weight[i, "lrc"] <- summary(model)$coefficients["lrc", "Estimate"]
}
exp_df_quma_weight

# Loop through each replicate initial values
for (i in 1: length(exp_quma_weight[,1:10])) {
  exp_quma_weight[1, i] <- exp_df_quma_weight[i, "R0"]
}

### calculate percentages to initial weight
tmp<-colnames(exp_quma_weight)[1:length(colnames(exp_quma_weight))]
for (i in 1:length(tmp)) {
  exp_quma_weight[,paste(tmp[i],"perc", sep="_")]<-NA
  exp_quma_weight[,paste(tmp[i],"perc", sep="_")]<-(100*exp_quma_weight[,tmp[i]])/exp_quma_weight[1,tmp[i]]
}

# Drop column "juliandays_perc"
exp_quma_weight<-(exp_quma_weight[ ,  !names(exp_quma_weight) %in% 
                                     c("daysinchambers_perc")])
# Filter columns that contain "perc"
perc_columns_quma <- grep("perc", colnames(exp_quma_weight), value = TRUE)

# Reshape the data frame to long format
df_long_quma <- exp_quma_weight %>%
  gather(column, value, all_of(perc_columns_quma))

# Calculate the mean curve
mean_curve_quma <- df_long_quma %>%
  group_by(daysinchambers) %>%
  summarise(mean_value = mean(value, na.rm=TRUE))

# Plotting the data points and mean curve
print_qumaW<-ggplot() +
  geom_point(data = df_long_quma, aes(x = daysinchambers, y = value, color = column, show.legend = FALSE), size = 1) +
  geom_line(data = mean_curve_quma, aes(x = daysinchambers, y = mean_value), color = "black", size = 1) +
  labs(x = "", y = "") +
  ggtitle("Quma WL DRO1 Adjusted") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "")
print_qumaW
ggsave("output/dro1/soil_moist_qumaW_adjusted.pdf", print_qumaW, dpi = 300, width = 4, height = 4)


##### Sese #####
#Subset Sese
seseW <- subset(weight_drought_1, species=="Sese")
# rotate the table
seseW_Dro1<- t(seseW)
# convert to data frame
seseW_Dro1 <- as.data.frame(seseW_Dro1, stringsAsFactors = FALSE)
# Set the first row as column names
colnames(seseW_Dro1) <- seseW_Dro1[1, ]
# Remove the first two rows
seseW_Dro1 <- seseW_Dro1[-c(1,2),]
#add julian column to df
seseW_Dro1['juliandays']<-julianW
# Remove rows that are all empty or all NA values
seseW_Dro1 <- seseW_Dro1[rowSums(!is.na(seseW_Dro1) & seseW_Dro1 != "") > 1, ]


######make all columns numeric
for (i in 1:length(colnames(seseW_Dro1))) {
  seseW_Dro1[,i]<-as.numeric(seseW_Dro1[,i])
}
###### Change Original values by R0 from negative exponential curve model ######
exp_sese_weight <- seseW_Dro1[, c(1:5,11:15)]
exp_sese_weight$daysinchambers <- c(0:(length(exp_sese_weight$Sese_Dro1_B1_R1)-1))

# Create an empty data frame
exp_df_sese_weight<- tibble(id = colnames(exp_sese_weight)[-length(colnames(exp_sese_weight))], Asym = NA, R0 = NA, lrc = NA)

# Loop through every replicates to extract their coefficients and store them in the df
for (i in 1:length(exp_sese_weight)) {
  tmp <- exp_sese_weight[, i]  
  model <- nls(tmp ~ SSasymp(daysinchambers, Asym, R0, lrc), data = exp_sese_weight)
  exp_df_sese_weight[i, "Asym"] <- summary(model)$coefficients["Asym", "Estimate"]
  exp_df_sese_weight[i, "R0"] <- summary(model)$coefficients["R0", "Estimate"]
  exp_df_sese_weight[i, "lrc"] <- summary(model)$coefficients["lrc", "Estimate"]
}
exp_df_sese_weight

# Loop through each replicate initial values
for (i in 1: length(exp_sese_weight[,1:10])) {
  exp_sese_weight[1, i] <- exp_df_sese_weight[i, "R0"]
}

### calculate percentages to initial weight
tmp<-colnames(exp_sese_weight)[1:length(colnames(exp_sese_weight))]
for (i in 1:length(tmp)) {
  exp_sese_weight[,paste(tmp[i],"perc", sep="_")]<-NA
  exp_sese_weight[,paste(tmp[i],"perc", sep="_")]<-(100*exp_sese_weight[,tmp[i]])/exp_sese_weight[1,tmp[i]]
}

# Drop column "juliandays_perc"
exp_sese_weight<-(exp_sese_weight[ ,  !names(exp_sese_weight) %in% 
                                     c("daysinchambers_perc")])
# Filter columns that contain "perc"
perc_columns_sese <- grep("perc", colnames(exp_sese_weight), value = TRUE)

# Reshape the data frame to long format
df_long_sese <- exp_sese_weight %>%
  gather(column, value, all_of(perc_columns_sese))

# Calculate the mean curve
mean_curve_sese <- df_long_sese %>%
  group_by(daysinchambers) %>%
  summarise(mean_value = mean(value, na.rm=TRUE))

# Plotting the data points and mean curve
print_seseW<-ggplot() +
  geom_point(data = df_long_sese, aes(x = daysinchambers, y = value, color = column, show.legend = FALSE), size = 1) +
  geom_line(data = mean_curve_sese, aes(x = daysinchambers, y = mean_value), color = "black", size = 1) +
  labs(x = "", y = "") +
  ggtitle("Sese WL DRO1 Adjusted") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "")
print_seseW
ggsave("output/dro1/soil_moist_seseW_adjusted.pdf", print_seseW, dpi = 300, width = 4, height = 4)

#*==============================================================================

#*==============================================================================

#### VOLUMETRIC WATER CONTENT - DROUGHT 2 ####

# read excel file
# volumetric water content
vwc_drought_2 <- read_excel("raw_data/dro2_soilmoisture.xlsx", sheet = "vwc_drought_2")

#=== === === === === === === === === === === === === === === === === === === ===
# Specifications:
# the "M" is for moisture, meaning it's the subset data for values of volumetric water content
# prvi: prunus
# acma: acer
# bepa: betula 
# pico: pinus
# quma: quercus 
# sese: sequoia
#=== === === === === === === === === === === === === === === === === === === ===

### create vector of julian days that is length of the number of julian days
julianM <- colnames(vwc_drought_2)[3:length(colnames(vwc_drought_2))]

##### Prvi #####

#Subset
prviM_subset <- subset(vwc_drought_2, species == 'Prvi')
# rotate the table
prviM_rot <- t(prviM_subset)
# convert to data frame
prviM_dro2 <- as.data.frame(prviM_rot, stringsAsFactors = FALSE)
# Set the first row as column names
colnames(prviM_dro2) <- prviM_dro2[1,]
# Remove the first 2 rows
prviM_dro2 <- prviM_dro2[-c(1:2),]
# add julian column to df
prviM_dro2['juliandays'] <- julianM
# Subset only for bloc 1 and 3
# prviM_dro2cut <- prviM_dro2 [, c(1:5, 11:16)]
# convert to numeric
for (i in 1:length(colnames(prviM_dro2))) {
  prviM_dro2[, i] <- as.numeric(prviM_dro2[, i])
}
# delete na rows with dates that we have no data
prviM_dro2 <- prviM_dro2[rowSums(!is.na(prviM_dro2) & prviM_dro2 != "") > 1, ]
head(prviM_dro2)
# Create a data frame for plotting
prviM_dro_2_df <- data.frame(
  x = prviM_dro2$juliandays,
  y = as.numeric(unlist(prviM_dro2[, grepl("^Prvi", colnames(prviM_dro2))])),
  column = rep(colnames(prviM_dro2)[-ncol(prviM_dro2)], each = nrow(prviM_dro2))
)

# Calculate the mean curve
mean_curve_prviM_dro_2 <- prviM_dro_2_df %>%
  group_by(x) %>%
  summarise(mean_value = mean(y, na.rm = TRUE))

print_prviM_dro_2 <- ggplot() +
  geom_point(data = prviM_dro_2_df, aes(x = x, y = y, color = column, show.legend = FALSE), size = 1) +
  geom_line(data = mean_curve_prviM_dro_2, aes(x = x, y = mean_value), color = "black", size = 1) +
  labs(x = "", y = "") +
  ggtitle("Prvi VWC DRO2") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )
print_prviM_dro_2 <- print_prviM_dro_2 + theme(legend.position = "none")
print_prviM_dro_2
ggsave("output/dro2/soil_moist_prvi2.pdf", print_prviM_dro_2, dpi = 300, width = 4, height = 4)


##### Acma #####
# Subset
acmaM_subset <- subset(vwc_drought_2, species == 'Acma')
# Rotate the table
acmaM_rot <- t(acmaM_subset)
# Convert to data frame
acmaM_dro2 <- as.data.frame(acmaM_rot, stringsAsFactors = FALSE)
# Set the first row as column names
colnames(acmaM_dro2) <- acmaM_dro2[1,]
# Remove the first 2 rows
acmaM_dro2 <- acmaM_dro2[-c(1:2),]
# Add julian column to df
acmaM_dro2['juliandays'] <- julianM
# Convert to numeric
for (i in 1:length(colnames(acmaM_dro2))) {
  acmaM_dro2[, i] <- as.numeric(acmaM_dro2[, i])
}
# Delete na rows with dates that we have no data
acmaM_dro2 <- acmaM_dro2[rowSums(!is.na(acmaM_dro2) & acmaM_dro2 != "") > 1, ]

# Create a data frame for plotting
acmaM_df_dro_2 <- data.frame(
  x = acmaM_dro2$juliandays,
  y = as.numeric(unlist(acmaM_dro2[, grepl("^Acma", colnames(acmaM_dro2))])),
  column = rep(colnames(acmaM_dro2)[-ncol(acmaM_dro2)], each = nrow(acmaM_dro2))
)

# Calculate the mean curve
mean_curve_acmaM_dro_2 <- acmaM_df_dro_2 %>%
  group_by(x) %>%
  summarise(mean_value = mean(y, na.rm = TRUE))

print_acmaM_dro_2 <- ggplot() +
  geom_point(data = acmaM_df_dro_2, aes(x = x, y = y, color = column, show.legend = FALSE), size = 1) +
  geom_line(data = mean_curve_acmaM_dro_2, aes(x = x, y = mean_value), color = "black", size = 1) +
  labs(x = "", y = "") +
  ggtitle("Acma VWC DRO2") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )
print_acmaM_dro_2 <- print_acmaM_dro_2 + theme(legend.position = "none")
print_acmaM_dro_2
ggsave("output/dro2/soil_moist_acma2.pdf", print_acmaM_dro_2, dpi = 300, width = 4, height = 4)



##### Bepa #####
#Subset
bepaM_subset <- subset(vwc_drought_2, species == 'Bepa')
# rotate the table
bepaM_rot <- t(bepaM_subset)
# convert to data frame
bepaM_dro2 <- as.data.frame(bepaM_rot, stringsAsFactors = FALSE)
# Set the first row as column names
colnames(bepaM_dro2) <- bepaM_dro2[1,]
# Remove the first 2 rows
bepaM_dro2 <- bepaM_dro2[-c(1:2),]
# add julian column to df
bepaM_dro2['juliandays'] <- julianM
# convert to numeric
for (i in 1:length(colnames(bepaM_dro2))) {
  bepaM_dro2[, i] <- as.numeric(bepaM_dro2[, i])
}
# delete na rows with dates that we have no data
bepaM_dro2 <- bepaM_dro2[rowSums(!is.na(bepaM_dro2) & bepaM_dro2 != "") > 1, ]
head(bepaM_dro2)
colnames(bepaM_dro2)<- gsub("\r\n", "", colnames(bepaM_dro2))

# Create a data frame for plotting
bepaM_df_dro_2 <- data.frame(
  x = bepaM_dro2$juliandays,
  y = as.numeric(unlist(bepaM_dro2[, grepl("^Bepa", colnames(bepaM_dro2))])),
  column = rep(colnames(bepaM_dro2)[-ncol(bepaM_dro2)], each = nrow(bepaM_dro2))
)

# Calculate the mean curve
mean_curve_bepaM_dro_2 <- bepaM_df_dro_2 %>%
  group_by(x) %>%
  summarise(mean_value = mean(y, na.rm = TRUE))

print_bepaM_dro_2 <- ggplot() +
  geom_point(data = bepaM_df_dro_2, aes(x = x, y = y, color = column, show.legend = FALSE), size = 1) +
  geom_line(data = mean_curve_bepaM_dro_2, aes(x = x, y = mean_value), color = "black", size = 1) +
  labs(x = "", y = "") +
  ggtitle("Bepa VWC DRO2") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )
print_bepaM_dro_2 <- print_bepaM_dro_2 + theme(legend.position = "none")
print_bepaM_dro_2
ggsave("output/dro2/soil_moist_bepa2.pdf", print_bepaM_dro_2, dpi = 300, width = 4, height = 4)



##### Pico #####
#Subset
picoM_subset <- subset(vwc_drought_2, species == 'Pico')
# rotate the table
pico_rot <- t(picoM_subset)
# convert to data frame
picoM_dro2 <- as.data.frame(pico_rot, stringsAsFactors = FALSE)
# Set the first row as column names
colnames(picoM_dro2) <- picoM_dro2[1,]
# Remove the first 2 rows
picoM_dro2 <- picoM_dro2[-(1:2),]
# add julian column to df
picoM_dro2['juliandays'] <- julianM
# convert to numeric
for (i in 1:length(colnames(picoM_dro2))) {
  picoM_dro2[, i] <- as.numeric(picoM_dro2[, i])
}
# delete na rows with dates that we have no data
picoM_dro2 <- picoM_dro2[rowSums(!is.na(picoM_dro2) & picoM_dro2 != "") > 1, ]

# Create a data frame for plotting
picoM_df_dro_2 <- data.frame(
  x = picoM_dro2$juliandays,
  y = as.numeric(unlist(picoM_dro2[, grepl("^Pico", colnames(picoM_dro2))])),
  column = rep(colnames(picoM_dro2)[-ncol(picoM_dro2)], each = nrow(picoM_dro2))
)

# Calculate the mean curve
mean_curve_picoM_dro_2 <- picoM_df_dro_2 %>%
  group_by(x) %>%
  summarise(mean_value = mean(y, na.rm = TRUE))

print_picoM_dro_2 <- ggplot() +
  geom_point(data = picoM_df_dro_2, aes(x = x, y = y, color = column, show.legend = FALSE), size = 1) +
  geom_line(data = mean_curve_picoM_dro_2, aes(x = x, y = mean_value), color = "black", size = 1) +
  labs(x = "", y = "") +
  ggtitle("Pico VWC DRO2") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )
print_picoM_dro_2 <- print_picoM_dro_2 + theme(legend.position = "none")
ggsave("output/dro2/soil_moist_pico2.pdf", print_picoM_dro_2, dpi = 300, width = 4, height = 4)


##### Quma #####
#Subset
qumaM_subset <- subset(vwc_drought_2, species == 'Quma')
# rotate the table
quma_rot <- t(qumaM_subset)
# convert to data frame
qumaM_dro2 <- as.data.frame(quma_rot, stringsAsFactors = FALSE)
# Set the first row as column names
colnames(qumaM_dro2) <- qumaM_dro2[1,]
# Remove the first 2 rows
qumaM_dro2 <- qumaM_dro2[-(1:2),]
# add julian column to df
qumaM_dro2['juliandays'] <- julianM
# convert to numeric
for (i in 1:length(colnames(qumaM_dro2))) {
  qumaM_dro2[, i] <- as.numeric(qumaM_dro2[, i])
}
# delete na rows with dates that we have no data
qumaM_dro2 <- qumaM_dro2[rowSums(!is.na(qumaM_dro2) & qumaM_dro2 != "") > 1, ]

# Create a data frame for plotting
qumaM_df_dro_2 <- data.frame(
  x = qumaM_dro2$juliandays,
  y = as.numeric(unlist(qumaM_dro2[, grepl("^Quma", colnames(qumaM_dro2))])),
  column = rep(colnames(qumaM_dro2)[-ncol(qumaM_dro2)], each = nrow(qumaM_dro2))
)

# Calculate the mean curve
mean_curve_qumaM_dro_2 <- qumaM_df_dro_2 %>%
  group_by(x) %>%
  summarise(mean_value = mean(y, na.rm = TRUE))

print_qumaM_dro_2 <- ggplot() +
  geom_point(data = qumaM_df_dro_2, aes(x = x, y = y, color = column, show.legend = FALSE), size = 1) +
  geom_line(data = mean_curve_qumaM_dro_2, aes(x = x, y = mean_value), color = "black", size = 1) +
  labs(x = "", y = "") +
  ggtitle("Quma VWC DRO2") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )
print_qumaM_dro_2 <- print_qumaM_dro_2 + theme(legend.position = "none")
print_qumaM_dro_2
ggsave("output/dro2/soil_moist_quma2.pdf", print_qumaM_dro_2, dpi = 300, width = 4, height = 4)


##### Sese #####
#Subset
seseM_subset <- subset(vwc_drought_2, species == 'Sese')
# rotate the table
sese_rot <- t(seseM_subset)
# convert to data frame
seseM_dro2 <- as.data.frame(sese_rot, stringsAsFactors = FALSE)
# Set the first row as column names
colnames(seseM_dro2) <- seseM_dro2[1,]
# Remove the first 2 rows
seseM_dro2 <- seseM_dro2[-(1:2),]
# add julian column to df
seseM_dro2['juliandays'] <- julianM
# convert to numeric
for (i in 1:length(colnames(seseM_dro2))) {
  seseM_dro2[, i] <- as.numeric(seseM_dro2[, i])
}
# delete na rows with dates that we have no data
seseM_dro2 <- seseM_dro2[rowSums(!is.na(seseM_dro2) & seseM_dro2 != "") > 1, ]

# Create a data frame for plotting
seseM_df_dro_2 <- data.frame(
  x = seseM_dro2$juliandays,
  y = as.numeric(unlist(seseM_dro2[, grepl("^Sese", colnames(seseM_dro2))])),
  column = rep(colnames(seseM_dro2)[-ncol(seseM_dro2)], each = nrow(seseM_dro2))
)

# Calculate the mean curve
mean_curve_seseM_dro_2<- seseM_df_dro_2 %>%
  group_by(x) %>%
  summarise(mean_value = mean(y, na.rm = TRUE))

print_seseM_dro_2 <- ggplot() +
  geom_point(data = seseM_df_dro_2, aes(x = x, y = y, color = column, show.legend = FALSE), size = 1) +
  geom_line(data = mean_curve_seseM_dro_2, aes(x = x, y = mean_value), color = "black", size = 1) +
  labs(x = "", y = "") +
  ggtitle("Sese VWC DRO2") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )
print_seseM_dro_2 <- print_seseM_dro_2 + theme(legend.position = "none")
ggsave("output/dro2/soil_moist_sese2.pdf", print_seseM_dro_2, dpi = 300, width = 4, height = 4)




#### WEIGHT - DROUGHT 2 ####
# Read excel file
weight_drought_2 <- read_excel("raw_data/dro2_soilmoisture.xlsx", sheet = "weight_drought_2")

### Create vector of julian days that is length of the number of julian days
julianW <- colnames(weight_drought_2)[3:length(colnames(weight_drought_2))]

##### Prvi #####
prviW <- subset(weight_drought_2, species == "Prvi")
str(prviW)
# Rotate the table
prviW_Dro2 <- t(prviW)
# Convert to data frame
prviW_Dro2 <- as.data.frame(prviW_Dro2, stringsAsFactors = FALSE)
# Set the first row as column names
colnames(prviW_Dro2) <- prviW_Dro2[1, ]
# Remove the first two rows
prviW_Dro2 <- prviW_Dro2[-c(1:2), ]
# Add julian column to df
prviW_Dro2['juliandays'] <- julianW
# Remove rows that are all empty or all NA values
prviW_Dro2 <- prviW_Dro2[rowSums(!is.na(prviW_Dro2) & prviW_Dro2 != "") > 1, ]
head(prviW_Dro2)

###### Make all columns numeric
for (i in 1:length(colnames(prviW_Dro2))) {
  prviW_Dro2[, i] <- as.numeric(prviW_Dro2[, i])
}

### Calculate percentages to initial weight
tmp <- colnames(prviW_Dro2)[1:length(colnames(prviW_Dro2))]
for (i in 1:length(tmp)) {
  prviW_Dro2[, paste(tmp[i], "perc", sep = "_")] <- NA
  prviW_Dro2[, paste(tmp[i], "perc", sep = "_")] <- (100 * prviW_Dro2[, tmp[i]]) / prviW_Dro2[1, tmp[i]]
}
colnames(prviW_Dro2)<- gsub("\r\n", "", colnames(prviW_Dro2))
#### Temporary delete column julian days perc
# Drop column "juliandays_perc"
prviW_Dro2 <- prviW_Dro2[, !names(prviW_Dro2) %in% c("juliandays_perc")]
# Filter columns that contain "perc"
perc_columns_prvi_dro_2 <- grep("perc", colnames(prviW_Dro2), value = TRUE)

# Reshape the data frame to long format
df_long_prvi_dro_2 <- prviW_Dro2 %>%
  gather(column, value, all_of(perc_columns_prvi_dro_2))
head(df_long_prvi_dro_2)
# Calculate the mean curve
mean_curve_prvi_dro_2 <- df_long_prvi_dro_2 %>%
  group_by(juliandays) %>%
  summarise(mean_value = mean(value, na.rm=TRUE))

# Plotting the data points and mean curve
print_prviW_dro_2 <- ggplot() +
  geom_point(data = df_long_prvi_dro_2, aes(x = juliandays, y = value, color = column, show.legend = FALSE), size = 1) +
  geom_line(data = mean_curve_prvi_dro_2, aes(x = juliandays, y = mean_value), color = "black", size = 1) +
  labs(x = "", y = "") +
  ggtitle("Prvi WL DRO2") +
  theme_minimal()+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5))
print_prviW_dro_2<-print_prviW_dro_2 + theme(legend.position = "none") 
print_prviW_dro_2
ggsave("output/dro2/water_loss_prvi2.pdf", print_prviW_dro_2, dpi = 300, width = 4, height = 4)


##### Acma ####
acmaW <- subset(weight_drought_2, species == "Acma")
str(acmaW)
# Rotate the table
acmaW_Dro2 <- t(acmaW)
# Convert to data frame
acmaW_Dro2 <- as.data.frame(acmaW_Dro2, stringsAsFactors = FALSE)
# Set the first row as column names
colnames(acmaW_Dro2) <- acmaW_Dro2[1, ]
# Remove the first two rows
acmaW_Dro2 <- acmaW_Dro2[-c(1:2), ]
# Add julian column to df
acmaW_Dro2['juliandays'] <- julianW
# Remove rows that are all empty or all NA values
acmaW_Dro2 <- acmaW_Dro2[rowSums(!is.na(acmaW_Dro2) & acmaW_Dro2 != "") > 1, ]
head(acmaW_Dro2)

###### Make all columns numeric
for (i in 1:length(colnames(acmaW_Dro2))) {
  acmaW_Dro2[, i] <- as.numeric(acmaW_Dro2[, i])
}

### Calculate percentages to initial weight
tmp <- colnames(acmaW_Dro2)[1:length(colnames(acmaW_Dro2))]
for (i in 1:length(tmp)) {
  acmaW_Dro2[, paste(tmp[i], "perc", sep = "_")] <- NA
  acmaW_Dro2[, paste(tmp[i], "perc", sep = "_")] <- (100 * acmaW_Dro2[, tmp[i]]) / acmaW_Dro2[1, tmp[i]]
}

#### Temporary delete column julian days perc
# Drop column "juliandays_perc"
acmaW_Dro2 <- acmaW_Dro2[, !names(acmaW_Dro2) %in% c("juliandays_perc")]
# Filter columns that contain "perc"
perc_columns_acma_dro_2 <- grep("perc", colnames(acmaW_Dro2), value = TRUE)

# Reshape the data frame to long format
df_long_acma_dro_2 <- acmaW_Dro2 %>%
  gather(column, value, all_of(perc_columns_acma_dro_2))
head(df_long_acma_dro_2)
# Calculate the mean curve
mean_curve_acma_dro_2 <- df_long_acma_dro_2 %>%
  group_by(juliandays) %>%
  summarise(mean_value = mean(value, na.rm=TRUE))

# Plotting the data points and mean curve
print_acmaW_dro_2 <- ggplot() +
  geom_point(data = df_long_acma_dro_2, aes(x = juliandays, y = value, color = column, show.legend = FALSE), size = 1) +
  geom_line(data = mean_curve_acma_dro_2, aes(x = juliandays, y = mean_value), color = "black", size = 1) +
  labs(x = "", y = "") +
  ggtitle("Acma WL DRO2") +
  theme_minimal()+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5))
print_acmaW_dro_2 <- print_acmaW_dro_2 + theme(legend.position = "none")
print_acmaW_dro_2
ggsave("output/dro2/water_loss_acma2.pdf", print_acmaW_dro_2, dpi = 300, width = 4, height = 4)




##### Bepa ####
bepaW <- subset(weight_drought_2, species == "Bepa")
str(bepaW)
# Rotate the table
bepaW_Dro2 <- t(bepaW)
# Convert to data frame
bepaW_Dro2 <- as.data.frame(bepaW_Dro2, stringsAsFactors = FALSE)
# Set the first row as column names
colnames(bepaW_Dro2) <- bepaW_Dro2[1, ]
# Remove the first two rows
bepaW_Dro2 <- bepaW_Dro2[-c(1:2), ]
# Add julian column to df
bepaW_Dro2['juliandays'] <- julianW
# Remove rows that are all empty or all NA values
bepaW_Dro2 <- bepaW_Dro2[rowSums(!is.na(bepaW_Dro2) & bepaW_Dro2 != "") > 1, ]
head(bepaW_Dro2)

###### Make all columns numeric
for (i in 1:length(colnames(bepaW_Dro2))) {
  bepaW_Dro2[, i] <- as.numeric(bepaW_Dro2[, i])
}

### Calculate percentages to initial weight
tmp <- colnames(bepaW_Dro2)[1:length(colnames(bepaW_Dro2))]
for (i in 1:length(tmp)) {
  bepaW_Dro2[, paste(tmp[i], "perc", sep = "_")] <- NA
  bepaW_Dro2[, paste(tmp[i], "perc", sep = "_")] <- (100 * bepaW_Dro2[, tmp[i]]) / bepaW_Dro2[1, tmp[i]]
}

#### Temporary delete column julian days perc
# Drop column "juliandays_perc"
bepaW_Dro2 <- bepaW_Dro2[, !names(bepaW_Dro2) %in% c("juliandays_perc")]
# Filter columns that contain "perc"
perc_columns_bepa_dro_2 <- grep("perc", colnames(bepaW_Dro2), value = TRUE)

# Reshape the data frame to long format
df_long_bepa_dro_2 <- bepaW_Dro2 %>%
  gather(column, value, all_of(perc_columns_bepa_dro_2))
head(df_long_bepa_dro_2)
# Calculate the mean curve
mean_curve_bepa_dro_2 <- df_long_bepa_dro_2 %>%
  group_by(juliandays) %>%
  summarise(mean_value = mean(value, na.rm=TRUE))

# Plotting the data points and mean curve
print_bepaW_dro_2 <- ggplot() +
  geom_point(data = df_long_bepa_dro_2, aes(x = juliandays, y = value, color = column, show.legend = FALSE), size = 1) +
  geom_line(data = mean_curve_bepa_dro_2, aes(x = juliandays, y = mean_value), color = "black", size = 1) +
  labs(x = "", y = "") +
  ggtitle("Bepa WL DRO2") +
  theme_minimal()+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5))
print_bepaW_dro_2 <- print_bepaW_dro_2 + theme(legend.position = "none")
print_bepaW_dro_2
ggsave("output/dro2/water_loss_bepa2.pdf", print_bepaW_dro_2, dpi = 300, width = 4, height = 4)


##### Pico ####
picoW <- subset(weight_drought_2, species == "Pico")
str(picoW)
# Rotate the table
picoW_Dro2 <- t(picoW)
# Convert to data frame
picoW_Dro2 <- as.data.frame(picoW_Dro2, stringsAsFactors = FALSE)
# Set the first row as column names
colnames(picoW_Dro2) <- picoW_Dro2[1, ]
# Remove the first two rows
picoW_Dro2 <- picoW_Dro2[-c(1:2), ]
# Add julian column to df
picoW_Dro2['juliandays'] <- julianW
# Remove rows that are all empty or all NA values
picoW_Dro2 <- picoW_Dro2[rowSums(!is.na(picoW_Dro2) & picoW_Dro2 != "") > 1, ]
head(picoW_Dro2)

###### Make all columns numeric
for (i in 1:length(colnames(picoW_Dro2))) {
  picoW_Dro2[, i] <- as.numeric(picoW_Dro2[, i])
}

### Calculate percentages to initial weight
tmp <- colnames(picoW_Dro2)[1:length(colnames(picoW_Dro2))]
for (i in 1:length(tmp)) {
  picoW_Dro2[, paste(tmp[i], "perc", sep = "_")] <- NA
  picoW_Dro2[, paste(tmp[i], "perc", sep = "_")] <- (100 * picoW_Dro2[, tmp[i]]) / picoW_Dro2[1, tmp[i]]
}

#### Temporary delete column julian days perc
# Drop column "juliandays_perc"
picoW_Dro2 <- picoW_Dro2[, !names(picoW_Dro2) %in% c("juliandays_perc")]
# Filter columns that contain "perc"
perc_columns_pico_dro_2 <- grep("perc", colnames(picoW_Dro2), value = TRUE)

# Reshape the data frame to long format
df_long_pico_dro_2 <- picoW_Dro2 %>%
  gather(column, value, all_of(perc_columns_pico_dro_2))
head(df_long_pico_dro_2)
# Calculate the mean curve
mean_curve_pico_dro_2 <- df_long_pico_dro_2 %>%
  group_by(juliandays) %>%
  summarise(mean_value = mean(value, na.rm=TRUE))

# Plotting the data points and mean curve
print_picoW_dro_2 <- ggplot() +
  geom_point(data = df_long_pico_dro_2, aes(x = juliandays, y = value, color = column, show.legend = FALSE), size = 1) +
  geom_line(data = mean_curve_pico_dro_2, aes(x = juliandays, y = mean_value), color = "black", size = 1) +
  labs(x = "", y = "") +
  ggtitle("Pico WL DRO2") +
  theme_minimal()+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5))
print_picoW_dro_2 <- print_picoW_dro_2 + theme(legend.position = "none")
print_picoW_dro_2
ggsave("output/dro2/water_loss_pico2.pdf", print_picoW_dro_2, dpi = 300, width = 4, height = 4)



##### Quma ####
qumaW <- subset(weight_drought_2, species == "Quma")
str(qumaW)
# Rotate the table
qumaW_Dro2 <- t(qumaW)
# Convert to data frame
qumaW_Dro2 <- as.data.frame(qumaW_Dro2, stringsAsFactors = FALSE)
# Set the first row as column names
colnames(qumaW_Dro2) <- qumaW_Dro2[1, ]
# Remove the first two rows
qumaW_Dro2 <- qumaW_Dro2[-c(1:2), ]
# Add julian column to df
qumaW_Dro2['juliandays'] <- julianW
# Remove rows that are all empty or all NA values
qumaW_Dro2 <- qumaW_Dro2[rowSums(!is.na(qumaW_Dro2) & qumaW_Dro2 != "") > 1, ]
head(qumaW_Dro2)

###### Make all columns numeric
for (i in 1:length(colnames(qumaW_Dro2))) {
  qumaW_Dro2[, i] <- as.numeric(qumaW_Dro2[, i])
}

### Calculate percentages to initial weight
tmp <- colnames(qumaW_Dro2)[1:length(colnames(qumaW_Dro2))]
for (i in 1:length(tmp)) {
  qumaW_Dro2[, paste(tmp[i], "perc", sep = "_")] <- NA
  qumaW_Dro2[, paste(tmp[i], "perc", sep = "_")] <- (100 * qumaW_Dro2[, tmp[i]]) / qumaW_Dro2[1, tmp[i]]
}

#### Temporary delete column julian days perc
# Drop column "juliandays_perc"
qumaW_Dro2 <- qumaW_Dro2[, !names(qumaW_Dro2) %in% c("juliandays_perc")]
# Filter columns that contain "perc"
perc_columns_quma_dro_2 <- grep("perc", colnames(qumaW_Dro2), value = TRUE)

# Reshape the data frame to long format
df_long_quma_dro_2 <- qumaW_Dro2 %>%
  gather(column, value, all_of(perc_columns_quma_dro_2))
head(df_long_quma_dro_2)
# Calculate the mean curve
mean_curve_quma_dro_2 <- df_long_quma_dro_2 %>%
  group_by(juliandays) %>%
  summarise(mean_value = mean(value, na.rm=TRUE))

# Plotting the data points and mean curve
print_qumaW_dro_2 <- ggplot() +
  geom_point(data = df_long_quma_dro_2, aes(x = juliandays, y = value, color = column, show.legend = FALSE), size = 1) +
  geom_line(data = mean_curve_quma_dro_2, aes(x = juliandays, y = mean_value), color = "black", size = 1) +
  labs(x = "", y = "") +
  ggtitle("Quma WL DRO2") +
  theme_minimal()+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5))
print_qumaW_dro_2 <- print_qumaW_dro_2 + theme(legend.position = "none")
print_qumaW_dro_2
ggsave("output/dro2/water_loss_quma2.pdf", print_qumaW_dro_2, dpi = 300, width = 4, height = 4)



##### Sese ####
seseW <- subset(weight_drought_2, species == "Sese")
str(seseW)
# Rotate the table
seseW_Dro2 <- t(seseW)
# Convert to data frame
seseW_Dro2 <- as.data.frame(seseW_Dro2, stringsAsFactors = FALSE)
# Set the first row as column names
colnames(seseW_Dro2) <- seseW_Dro2[1, ]
# Remove the first two rows
seseW_Dro2 <- seseW_Dro2[-c(1:2), ]
# Add julian column to df
seseW_Dro2['juliandays'] <- julianW
# Remove rows that are all empty or all NA values
seseW_Dro2 <- seseW_Dro2[rowSums(!is.na(seseW_Dro2) & seseW_Dro2 != "") > 1, ]
head(seseW_Dro2)

###### Make all columns numeric
for (i in 1:length(colnames(seseW_Dro2))) {
  seseW_Dro2[, i] <- as.numeric(seseW_Dro2[, i])
}

### Calculate percentages to initial weight
tmp <- colnames(seseW_Dro2)[1:length(colnames(seseW_Dro2))]
for (i in 1:length(tmp)) {
  seseW_Dro2[, paste(tmp[i], "perc", sep = "_")] <- NA
  seseW_Dro2[, paste(tmp[i], "perc", sep = "_")] <- (100 * seseW_Dro2[, tmp[i]]) / seseW_Dro2[1, tmp[i]]
}
directory_path
#### Temporary delete column julian days perc
# Drop column "juliandays_perc"
seseW_Dro2 <- seseW_Dro2[, !names(seseW_Dro2) %in% c("juliandays_perc")]
# Filter columns that contain "perc"
perc_columns_sese_dro_2 <- grep("perc", colnames(seseW_Dro2), value = TRUE)

# Reshape the data frame to long format
df_long_sese_dro_2 <- seseW_Dro2 %>%
  gather(column, value, all_of(perc_columns_sese_dro_2))
head(df_long_sese_dro_2)
# Calculate the mean curve
mean_curve_sese_dro_2 <- df_long_sese_dro_2 %>%
  group_by(juliandays) %>%
  summarise(mean_value = mean(value, na.rm=TRUE))

# Plotting the data points and mean curve
print_seseW_dro_2 <- ggplot() +
  geom_point(data = df_long_sese_dro_2, aes(x = juliandays, y = value, color = column, show.legend = FALSE), size = 1) +
  geom_line(data = mean_curve_sese_dro_2, aes(x = juliandays, y = mean_value), color = "black", size = 1) +
  labs(x = "", y = "") +
  ggtitle("Sese WL DRO2") +
  theme_minimal()+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5))
print_seseW_dro_2 <- print_seseW_dro_2 + theme(legend.position = "none")
print_seseW_dro_2
ggsave("output/dro2/water_loss_sese2.pdf", print_seseW_dro_2, dpi = 300, width = 4, height = 4)

#*==============================================================================
#### STATS ####

##### Prvi all replicates #####
exp_perc_prvi <- exp_prvi_weight[, c(12:21)]
exp_perc_prvi$daysinchambers <- c(0:(length(exp_perc_prvi$Prvi_Dro1_B1_R1_perc)-1))
# Create empty data frame in which the coefficients will be stored
exp_df_prviW <- tibble(id = colnames(exp_perc_prvi[1:10])[-length(colnames(exp_perc_prvi))], Asym = NA, R0 = NA, lrc = NA)

# Loop through every replicates to extract their coefficients and store them in the df
for (i in 1:length(exp_perc_prvi)) {
  tmp <- exp_perc_prvi[, i]  
  model <- nls(tmp ~ SSasymp(daysinchambers, Asym, R0, lrc), data = exp_perc_prvi)
  exp_df_prviW[i, "Asym"] <- summary(model)$coefficients["Asym", "Estimate"]
  exp_df_prviW[i, "R0"] <- summary(model)$coefficients["R0", "Estimate"]
  exp_df_prviW[i, "lrc"] <- summary(model)$coefficients["lrc", "Estimate"]
}
exp_df_prviW

##### Acma all replicates #####
exp_perc_acma <- exp_acma_weight[, c(12:21)]
exp_perc_acma$daysinchambers <- c(0:(length(exp_perc_acma$Acma_Dro1_B1_R1_perc)-1))
# Create empty data frame in which the coefficients will be stored
exp_df_acmaW <- tibble(id = colnames(exp_perc_acma[1:10])[-length(colnames(exp_perc_acma))], Asym = NA, R0 = NA, lrc = NA)

# Loop through every replicates to extract their coefficients and store them in the df
for (i in 1:length(exp_perc_acma)) {
  tmp <- exp_perc_acma[, i]  
  model <- nls(tmp ~ SSasymp(daysinchambers, Asym, R0, lrc), data = exp_perc_acma)
  exp_df_acmaW[i, "Asym"] <- summary(model)$coefficients["Asym", "Estimate"]
  exp_df_acmaW[i, "R0"] <- summary(model)$coefficients["R0", "Estimate"]
  exp_df_acmaW[i, "lrc"] <- summary(model)$coefficients["lrc", "Estimate"]
}
exp_df_acmaW

##### Bepa all replicates #####
exp_perc_bepa <- exp_bepa_weight[, c(12:21)]
exp_perc_bepa$daysinchambers <- c(0:(length(exp_perc_bepa$Bepa_Dro1_B1_R1_perc)-1))
# Create empty data frame in which the coefficients will be stored
exp_df_bepaW <- tibble(id = colnames(exp_perc_bepa[1:10])[-length(colnames(exp_perc_bepa))], Asym = NA, R0 = NA, lrc = NA)

# Loop through every replicates to extract their coefficients and store them in the df
for (i in 1:length(exp_perc_bepa)) {
  tmp <- exp_perc_bepa[, i]  
  model <- nls(tmp ~ SSasymp(daysinchambers, Asym, R0, lrc), data = exp_perc_bepa)
  exp_df_bepaW[i, "Asym"] <- summary(model)$coefficients["Asym", "Estimate"]
  exp_df_bepaW[i, "R0"] <- summary(model)$coefficients["R0", "Estimate"]
  exp_df_bepaW[i, "lrc"] <- summary(model)$coefficients["lrc", "Estimate"]
}
exp_df_bepaW

##### Pico all replicates #####
exp_perc_pico <- exp_pico_weight[, c(12:21)]
exp_perc_pico$daysinchambers <- c(0:(length(exp_perc_pico$Pico_Dro1_B1_R1_perc)-1))
# Create empty data frame in which the coefficients will be stored
exp_df_picoW <- tibble(id = colnames(exp_perc_pico[1:10])[-length(colnames(exp_perc_pico))], Asym = NA, R0 = NA, lrc = NA)

# Loop through every replicates to extract their coefficients and store them in the df
for (i in 1:length(exp_perc_pico)) {
  tmp <- exp_perc_pico[, i]  
  model <- nls(tmp ~ SSasymp(daysinchambers, Asym, R0, lrc), data = exp_perc_pico)
  exp_df_picoW[i, "Asym"] <- summary(model)$coefficients["Asym", "Estimate"]
  exp_df_picoW[i, "R0"] <- summary(model)$coefficients["R0", "Estimate"]
  exp_df_picoW[i, "lrc"] <- summary(model)$coefficients["lrc", "Estimate"]
}
exp_df_picoW

##### Quma all replicates #####
exp_perc_quma <- exp_quma_weight[, c(12:21)]
exp_perc_quma$daysinchambers <- c(0:(length(exp_perc_quma$Quma_Dro1_B1_R1_perc)-1))
# Create empty data frame in which the coefficients will be stored
exp_df_qumaW <- tibble(id = colnames(exp_perc_quma[1:10])[-length(colnames(exp_perc_quma))], Asym = NA, R0 = NA, lrc = NA)

# Loop through every replicates to extract their coefficients and store them in the df
for (i in 1:length(exp_perc_quma)) {
  tmp <- exp_perc_quma[, i]  
  model <- nls(tmp ~ SSasymp(daysinchambers, Asym, R0, lrc), data = exp_perc_quma)
  exp_df_qumaW[i, "Asym"] <- summary(model)$coefficients["Asym", "Estimate"]
  exp_df_qumaW[i, "R0"] <- summary(model)$coefficients["R0", "Estimate"]
  exp_df_qumaW[i, "lrc"] <- summary(model)$coefficients["lrc", "Estimate"]
}
exp_df_qumaW

##### Sese all replicates #####
exp_perc_sese <- exp_sese_weight[, c(12:21)]
exp_perc_sese$daysinchambers <- c(0:(length(exp_perc_sese$Sese_Dro1_B1_R1_perc)-1))
# Create empty data frame in which the coefficients will be stored
exp_df_seseW <- tibble(id = colnames(exp_perc_sese[1:10])[-length(colnames(exp_perc_sese))], Asym = NA, R0 = NA, lrc = NA)

# Loop through every replicates to extract their coefficients and store them in the df
for (i in 1:length(exp_perc_sese)) {
  tmp <- exp_perc_sese[, i]  
  model <- nls(tmp ~ SSasymp(daysinchambers, Asym, R0, lrc), data = exp_perc_sese)
  exp_df_seseW[i, "Asym"] <- summary(model)$coefficients["Asym", "Estimate"]
  exp_df_seseW[i, "R0"] <- summary(model)$coefficients["R0", "Estimate"]
  exp_df_seseW[i, "lrc"] <- summary(model)$coefficients["lrc", "Estimate"]
}
exp_df_seseW
monmodele <- nls(Sese_Dro1_B1_R1_perc ~ SSasymp())

#*==============================================================================
#### ADDITION OF A THERORITICAL 7 DAY PERIOD TO BETTER ESTIMATE WILTING POINT ####
# Plots
print_prviW
print_picoW
exp_df_prviW
modelvalues <- exp_df_prviW[c(9:10),]
# "Raw" data
head(exp_perc_prvi)

##### Prvi #####
prvi_newdays <- 28:34
# get one prunus replicate for good asym
prvi_cut_h <- exp_perc_prvi[, c(10:11)]
# get parameters for that replicate
prvi_est_h <- exp_df_prviW[10,]
predicted_h <- SSasymp(prvi_newdays, prvi_est_h$Asym , prvi_est_h$R0, prvi_est_h$lrc)
prviempty_h <- tibble(Prvi_Dro1_B3_R15_perc =predicted_h, daysinchambers=prvi_newdays)
# bind
prvi_h_bind <- rbind(prvi_cut_h, prviempty_h)

# get one prunus for lower asym
prvi_cut_l <- exp_perc_prvi[, c(3,11)]
# get parameters for that replicate
prvi_est_l <- exp_df_prviW[3,]
predicted_l <- SSasymp(prvi_newdays, prvi_est_l$Asym , prvi_est_l$R0, prvi_est_l$lrc)
prviempty_l <- tibble(Prvi_Dro1_B1_R3_perc =predicted_l, daysinchambers=prvi_newdays)
# bind
prvi_l_bind <- rbind(prvi_cut_l, prviempty_l)


##### Acma #####
acma_newdays <- 16:22
# get one prunus replicate for good asym
acma_cut_h <- exp_perc_acma[, c(10:11)]
# get parameters for that replicate
acma_est_h <- exp_df_acmaW[10,]
predicted_h <- SSasymp(acma_newdays, acma_est_h$Asym , acma_est_h$R0, acma_est_h$lrc)
acmaempty_h <- tibble(Acma_Dro1_B3_R15_perc =predicted_h, daysinchambers=acma_newdays)
# bind
acma_h_bind <- rbind(acma_cut_h, acmaempty_h)

# get one prunus for lower asym
acma_cut_l <- exp_perc_acma[, c(5,11)]
# get parameters for that replicate
acma_est_l <- exp_df_acmaW[5,]
predicted_l <- SSasymp(acma_newdays, acma_est_l$Asym , acma_est_l$R0, acma_est_l$lrc)
acmaempty_l <- tibble(Acma_Dro1_B1_R5_perc =predicted_l, daysinchambers=acma_newdays)
# bind
acma_l_bind <- rbind(acma_cut_l, acmaempty_l)


##### Bepa #####
bepa_newdays <- 19:25
# get one  replicate for good asym
bepa_cut_h <- exp_perc_bepa[, c(9,11)]
# get parameters for that replicate
bepa_est_h <- exp_df_bepaW[9,]
predicted_h <- SSasymp(bepa_newdays, bepa_est_h$Asym , bepa_est_h$R0, bepa_est_h$lrc)
bepaempty_h <- tibble(Bepa_Dro1_B3_R14_perc =predicted_h, daysinchambers=bepa_newdays)
# bind
bepa_h_bind <- rbind(bepa_cut_h, bepaempty_h)

# get one  for lower asym
bepa_cut_l <- exp_perc_bepa[, c(2,11)]
# get parameters for that replicate
bepa_est_l <- exp_df_bepaW[2,]
predicted_l <- SSasymp(bepa_newdays, bepa_est_l$Asym , bepa_est_l$R0, bepa_est_l$lrc)
bepaempty_l <- tibble(Bepa_Dro1_B1_R2_perc =predicted_l, daysinchambers=bepa_newdays)
# bind
bepa_l_bind <- rbind(bepa_cut_l, bepaempty_l)

##### Pico #####
pico_newdays <- 13:19
# get one prunus replicate for good asym
pico_cut_h <- exp_perc_pico[, c(10:11)]
# get parameters for that replicate
pico_est_h <- exp_df_picoW[10,]
predicted_h <- SSasymp(pico_newdays, pico_est_h$Asym , pico_est_h$R0, pico_est_h$lrc)
picoempty_h <- tibble(Pico_Dro1_B3_R15_perc =predicted_h, daysinchambers=pico_newdays)
# bind
pico_h_bind <- rbind(pico_cut_h, picoempty_h)

# get one  for lower asym
pico_cut_l <- exp_perc_pico[, c(1,11)]
# get parameters for that replicate
pico_est_l <- exp_df_picoW[1,]
predicted_l <- SSasymp(pico_newdays, pico_est_l$Asym , pico_est_l$R0, pico_est_l$lrc)
picoempty_l <- tibble(Pico_Dro1_B1_R1_perc =predicted_l, daysinchambers=pico_newdays)
# bind
pico_l_bind <- rbind(pico_cut_l, picoempty_l)


##### Quma ##### 
quma_newdays <- 12:18
# get one prunus replicate for good asym
quma_cut_h <- exp_perc_quma[, c(10:11)]
# get parameters for that replicate
quma_est_h <- exp_df_qumaW[10,]
predicted_h <- SSasymp(quma_newdays, quma_est_h$Asym , quma_est_h$R0, quma_est_h$lrc)
qumaempty_h <- tibble(Quma_Dro1_B3_R15_perc =predicted_h, daysinchambers=quma_newdays)
# bind
quma_h_bind <- rbind(quma_cut_h, qumaempty_h)

# get one prunus for lower asym
quma_cut_l <- exp_perc_quma[, c(5,11)]
# get parameters for that replicate
quma_est_l <- exp_df_qumaW[5,]
predicted_l <- SSasymp(quma_newdays, quma_est_l$Asym , quma_est_l$R0, quma_est_l$lrc)
qumaempty_l <- tibble(Quma_Dro1_B1_R5_perc =predicted_l, daysinchambers=quma_newdays)
# bind
quma_l_bind <- rbind(quma_cut_l, qumaempty_l)

##### Sese #####
sese_newdays <- 13:19
# get one  replicate for good asym
sese_cut_h <- exp_perc_sese[, c(9,11)]
# get parameters for that replicate
sese_est_h <- exp_df_seseW[9,]
predicted_h <- SSasymp(sese_newdays, sese_est_h$Asym , sese_est_h$R0, sese_est_h$lrc)
seseempty_h <- tibble(Sese_Dro1_B3_R14_perc =predicted_h, daysinchambers=sese_newdays)
# bind
sese_h_bind <- rbind(sese_cut_h, seseempty_h)

# get one  for lower asym
sese_cut_l <- exp_perc_sese[, c(10,11)]
# get parameters for that replicate
sese_est_l <- exp_df_seseW[10,]
predicted_l <- SSasymp(sese_newdays, sese_est_l$Asym , sese_est_l$R0, sese_est_l$lrc)
seseempty_l <- tibble(Sese_Dro1_B3_R15_perc =predicted_l, daysinchambers=sese_newdays)
# bind
sese_l_bind <- rbind(sese_cut_l, seseempty_l)



new_days <- 28:34
predicted <- SSasymp(new_days, 24.8, 100, -2.90)
predicted
dput(sese_cut)
dput(modelcut)

#### Replicate references for severe symptoms ####
symptoms_list <- read.csv("output/severe_symptoms_replicates.csv")

# Vector for Prvi
symp_prvi <- symptoms_list$id[grepl("Prvi", symptoms_list$id)]

# Add _perc to the vector
symp_prvi <- paste(symp_prvi, "perc", sep = "_")

# Select columns of previous vector
select  <- exp_prvi_weight[, symp_prvi]

sel <- exp_df_prviW[exp_df_prviW$id %in% symp_prvi, ]

exp_df_prviW

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 














#*==============================================================================
#### DROUGHT 1 DROUGHT 2 COMPARAISON ####


#Convert the individual plots to grob objects
###Moisture
grob_prviM<-ggplotGrob(print_prviM)
grob_acmaM<-ggplotGrob(print_acmaM)
grob_bepaM<-ggplotGrob(print_bepaM)
grob_picoM<-ggplotGrob(print_picoM)
grob_qumaM<-ggplotGrob(print_qumaM)
grob_seseM<-ggplotGrob(print_seseM)

###Weight
grob_prviW<-ggplotGrob(print_prviW)
grob_acmaW<-ggplotGrob(print_acmaW)
grob_bepaW<-ggplotGrob(print_bepaW)
grob_picoW<-ggplotGrob(print_picoW)
grob_qumaW<-ggplotGrob(print_qumaW)
grob_seseW<-ggplotGrob(print_seseW)


### ### ###### ### ###### ### ###### ### ###### ### ###### ### ###
# DRO 2
### ### ###### ### ###### ### ###### ### ###### ### ###### ### ###
grob_prviM2<-ggplotGrob(print_prviM_dro_2)
grob_acmaM2<-ggplotGrob(print_acmaM_dro_2)
grob_bepaM2<-ggplotGrob(print_bepaM_dro_2)
grob_picoM2<-ggplotGrob(print_picoM_dro_2)
grob_qumaM2<-ggplotGrob(print_qumaM_dro_2)
grob_seseM2<-ggplotGrob(print_seseM_dro_2)

###Weight
grob_prviW2<-ggplotGrob(print_prviW_dro_2)
grob_acmaW2<-ggplotGrob(print_acmaW_dro_2)
grob_bepaW2<-ggplotGrob(print_bepaW_dro_2)
grob_picoW2<-ggplotGrob(print_picoW_dro_2)
grob_qumaW2<-ggplotGrob(print_qumaW_dro_2)
grob_seseW2<-ggplotGrob(print_seseW_dro_2)

#Panel Prvi WEIGHT
grid_plot_prvi_W1_W2 <- grid.arrange(grob_prviW, grob_prviW2, 
                               nrow = 1, ncol =2) 
ggsave("output/dro1_dro2/panel_prvi_W1_W2.pdf", grid_plot_prvi_W1_W2, dpi = 300, width = 7, height = 4)
#Panel Prvi MOISTURE
grid_plot_prvi_M1_M2 <- grid.arrange(grob_prviM, grob_prviM2, 
                               nrow = 1, ncol =2) 
ggsave("output/dro1_dro2/panel_prvi_M1_M2.pdf", grid_plot_prvi_M1_M2, dpi = 300, width = 7, height = 4)
# Panel Acma WEIGHT
grid_plot_acma_W1_W2 <- grid.arrange(grob_acmaW, grob_acmaW2, 
                                     nrow = 1, ncol =2) 
ggsave("output/dro1_dro2/panel_acma_W1_W2.pdf", grid_plot_acma_W1_W2, dpi = 300, width = 7, height = 4)
# Panel Acma MOISTURE
grid_plot_acma_M1_M2 <- grid.arrange(grob_acmaM, grob_acmaM2, 
                                     nrow = 1, ncol =2) 
ggsave("output/dro1_dro2/panel_acma_M1_M2.pdf", grid_plot_acma_M1_M2, dpi = 300, width = 7, height = 4)
#Panel Bepa WEIGHT
grid_plot_bepa_W1_W2  <- grid.arrange(grob_bepaW, grob_bepaW2, 
                                      nrow = 1, ncol =2) 
ggsave("output/dro1_dro2/panel_bepa_W1_W2.pdf", grid_plot_bepa_W1_W2, dpi = 300, width = 7, height = 4)
#Panel Bepa MOISTURE
grid_plot_bepa_M1_M2 <- grid.arrange(grob_bepaM, grob_bepaM2, 
                                     nrow = 1, ncol =2) 
ggsave("output/dro1_dro2/panel_bepa_M1_M2.pdf", grid_plot_bepa_M1_M2, dpi = 300, width = 7, height = 4)
# Panel Pico WEIGHT
grid_plot_pico_W1_W2 <- grid.arrange(grob_picoW, grob_picoW2, 
                                     nrow = 1, ncol =2) 
ggsave("output/dro1/panel_pico_W1_W2.pdf", grid_plot_pico_W1_W2, dpi = 300, width = 7, height = 4)
# Panel Pico MOISTURE
grid_plot_pico_M1_M2 <- grid.arrange(grob_picoM, grob_picoM2, 
                                     nrow = 1, ncol =2) 
ggsave("output/dro1_dro2/panel_pico_M1_M2.pdf", grid_plot_pico_M1_M2, dpi = 300, width = 7, height = 4)

#Panel Quma WEIGHT
grid_plot_quma_W1_W2 <- grid.arrange(grob_qumaW, grob_qumaW2, 
                                     nrow = 1, ncol =2) 
ggsave("output/dro1_dro2/panel_quma_W1-W2.pdf", grid_plot_quma_W1_W2, dpi = 300, width = 7, height = 4)
#Panel Quma MOISTURE
grid_plot_quma_M1_M2 <- grid.arrange(grob_qumaM, grob_qumaM2, 
                                     nrow = 1, ncol =2) 
ggsave("output/dro1_dro2/panel_quma_M1_M2_.pdf", grid_plot_quma_M1_M2, dpi = 300, width = 7, height = 4)
# Panel Sese WEIGHT
grid_plot_sese_W1_W2 <- grid.arrange(grob_seseW, grob_seseW2, 
                                     nrow = 1, ncol =2) 
ggsave("output/dro1_dro2/panel_sese_W1_W2.pdf", grid_plot_sese_W1_W2, dpi = 300, width = 7, height = 4)
# Panel Sese MOISTURE
grid_plot_sese_M1_M2 <- grid.arrange(grob_seseM, grob_seseM2, 
                                     nrow = 1, ncol =2) 
ggsave("output/dro1_dro2/panel_sese_M1_M2.pdf", grid_plot_sese_M1_M2, dpi = 300, width = 7, height = 4)



####PANEL####
### Panel

#load gridextra 

#Convert the individual plots to grob objects
###Moisture
grob_prviM<-ggplotGrob(print_prviM)
grob_acmaM<-ggplotGrob(print_acmaM)
grob_bepaM<-ggplotGrob(print_bepaM)
grob_picoM<-ggplotGrob(print_picoM)
grob_qumaM<-ggplotGrob(print_qumaM)
grob_seseM<-ggplotGrob(print_seseM)

###Weight
grob_prviW<-ggplotGrob(print_prviW)
grob_acmaW<-ggplotGrob(print_acmaW)
grob_bepaW<-ggplotGrob(print_bepaW)
grob_picoW<-ggplotGrob(print_picoW)
grob_qumaW<-ggplotGrob(print_qumaW)
grob_seseW<-ggplotGrob(print_seseW)



#Panel Prvi
grid_plot_prvi <- grid.arrange(grob_prviM, grob_prvi, 
                               nrow = 1, ncol =2) 
ggsave("output/dro1/panel_prvi.pdf", grid_plot_prvi, dpi = 300, width = 7, height = 4)
#Panel Acma
grid_plot_acma <- grid.arrange(grob_acmaM, grob_acmaM, 
                               nrow = 1, ncol =2) 
ggsave("output/dro1/panel_acma.pdf", grid_plot_acma, dpi = 300, width = 7, height = 4)
#Panel Bepa WEIGHT
grid_plot_bepa_W1_W2  <- grid.arrange(grob_bepaW, grob_bepaW2, 
                                      nrow = 1, ncol =2) 
ggsave("output/dro1/panel_bepa_W1_W2.pdf", grid_plot_bepa_W1_W2, dpi = 300, width = 7, height = 4)
#Panel Bepa MOISTURE
grid_plot_bepa_M1_M2 <- grid.arrange(grob_bepaM, grob_bepaM2, 
                                     nrow = 1, ncol =2) 
ggsave("output/dro1/panel_bepa_M1_M2.pdf", grid_plot_bepa_M1_M2, dpi = 300, width = 7, height = 4)
#Panel Pico
grid_plot_pico <- grid.arrange(grob_picoM, grob_picoW, 
                               nrow = 1, ncol =2) 
ggsave("output/dro1/panel_pico.pdf", grid_plot_pico, dpi = 300, width = 7, height = 4)
#Panel Quma WEIGHT
grid_plot_quma_W1_W2 <- grid.arrange(grob_qumaW, grob_qumaW2, 
                                     nrow = 1, ncol =2) 
ggsave("output/dro1/panel_quma_W1-W2.pdf", grid_plot_quma_W1_W2, dpi = 300, width = 7, height = 4)
#Panel Quma MOISTURE
grid_plot_quma_M1_M2 <- grid.arrange(grob_qumaM, grob_qumaM2, 
                                     nrow = 1, ncol =2) 
ggsave("output/dro1/panel_quma_M1_M2_.pdf", grid_plot_quma_M1_M2, dpi = 300, width = 7, height = 4)




#Panel Sese
grid_plot_sese <- grid.arrange(grob_seseM, grob_seseW,  
                               nrow = 1, ncol =2) 
ggsave("output/dro1/panel_sese.pdf", grid_plot_sese, dpi = 300, width = 7, height = 4)



# Combine the plots using facet_wrap
combined_plot <- ggplot() +
  geom_blank() +
  labs(x = "X-axis", y = "Y-axis") +
  ggtitle("Panel") +
  facet_wrap(~ variable, ncol = 1) +
  theme(strip.text = element_text(size = 10))

# Create a grid layout for the panel
grob_prviM <- grid.arrange(grob_prviM, top = "Volumetric Water Content")
grob_prviW <- grid.arrange(grob_prviW, top = "Water Loss (%)")
grid_plot <- grid.arrange(
  grob_prviM, grob_prviW, 
  grob_bepaM, grob_bepaW,
  grob_acmaM, grob_acmaW,
  grob_picoM, grob_picoW,
  grob_qumaM, grob_qumaW,
  grob_seseM, grob_seseW, 
  nrow = 6, ncol =2) 


ggsave("output/dro1/panel.pdf", grid_plot, dpi = 300, width = 5, height = 10)




#Panel Prvi
grid_plot_prvi <- grid.arrange(grob_prviM, grob_prvi, 
                               nrow = 1, ncol =2) 
ggsave("output/dro1/panel_prvi.pdf", grid_plot_prvi, dpi = 300, width = 7, height = 4)
#Panel Acma
grid_plot_acma <- grid.arrange(grob_acmaM, grob_acmaM, 
                               nrow = 1, ncol =2) 
ggsave("output/dro1/panel_acma.pdf", grid_plot_acma, dpi = 300, width = 7, height = 4)
#Panel Bepa
grid_plot_bepa <- grid.arrange(grob_bepaM, grob_bepaW, 
                               nrow = 1, ncol =2) 
ggsave("output/dro1/panel_bepa.pdf", grid_plot_bepa, dpi = 300, width = 7, height = 4)
#Panel Pico
grid_plot_pico <- grid.arrange(grob_picoM, grob_picoW, 
                               nrow = 1, ncol =2) 
ggsave("output/dro1/panel_pico.pdf", grid_plot_pico, dpi = 300, width = 7, height = 4)
#Panel Quma
grid_plot_quma <- grid.arrange(grob_qumaM, grob_qumaW, 
                               nrow = 1, ncol =2) 
ggsave("output/dro1/panel_quma.pdf", grid_plot_quma, dpi = 300, width = 7, height = 4)
#Panel Sese
grid_plot_sese <- grid.arrange(grob_seseM, grob_seseW,  
                               nrow = 1, ncol =2) 
ggsave("output/dro1/panel_sese.pdf", grid_plot_sese, dpi = 300, width = 7, height = 4)

