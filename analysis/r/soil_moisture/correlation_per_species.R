##======================================================================================================
## Correlation all species drought 1 - Per species
##  
##
## Project:         PhaenoFlex
## Datum:           11.07.2023
## Autor:           Christophe 
#======================================================================================================

rm(list=ls(all=TRUE))

#------------------------------------------------------------------------------------------------------
# Set the path to your directory folder called PhotoChain
directory_path <- "/Users/christophe_rouleau-desrochers/Documents/github/PhaenoFlex/drought_treatment/R/"
# Set Working Directory
setwd(directory_path)

#package
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)

#### VOlUMETRIC WATER CONTENT####

#read excel file
vwc_drought_1<-read_excel("raw_data/dro1_soilmoisture.xlsx", sheet="vwc_drought_1")
# weight
weight_drought_1<-read_excel("raw_data/dro1_soilmoisture.xlsx", sheet="weight_drought_1")

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
prvi_vwc_dro1 <- data.frame(
  julian = prviM_dro1$juliandays,
  vwc = as.numeric(unlist(prviM_dro1[, grepl("^Prvi", colnames(prviM_dro1))])),
  id = rep(colnames(prviM_dro1)[-ncol(prviM_dro1)], each = nrow(prviM_dro1))
)

# Paste cor to beginning of id
prvi_vwc_dro1$id_cor <- paste("cor", prvi_vwc_dro1$id, sep = "_")
# Create a unique id with julian day
prvi_vwc_dro1$id_julian <- paste(prvi_vwc_dro1$id_cor, prvi_vwc_dro1$julian, sep = "_")


##### Acma #####
# Subset
acmaM_subset <- subset(vwc_drought_1, species == 'Acma')
# Rotate the table
acmaM_rot <- t(acmaM_subset)
# Convert to data frame
acmaM_dro1 <- as.data.frame(acmaM_rot, stringsAsFactors = FALSE)
# Set the first row as column names
colnames(acmaM_dro1) <- acmaM_dro1[1,]
# Remove the first 2 rows
acmaM_dro1 <- acmaM_dro1[-c(1:2),]
# Add julian column to df
acmaM_dro1['juliandays'] <- julianM
# Convert to numeric
for (i in 1:length(colnames(acmaM_dro1))) {
  acmaM_dro1[,i] <- as.numeric(acmaM_dro1[,i])
}
# Delete NA rows with dates that we have no data
acmaM_dro1 <- acmaM_dro1[rowSums(!is.na(acmaM_dro1) & acmaM_dro1 != "") > 1, ]

# Create a data frame for plotting
acma_vwc_dro1 <- data.frame(
  julian = acmaM_dro1$juliandays,
  vwc = as.numeric(unlist(acmaM_dro1[, grepl("^Acma", colnames(acmaM_dro1))])),
  id = rep(colnames(acmaM_dro1)[-ncol(acmaM_dro1)], each = nrow(acmaM_dro1))
)

# Paste cor to beginning of id
acma_vwc_dro1$id_cor <- paste("cor", acma_vwc_dro1$id, sep = "_")
# Create a unique id with julian day
acma_vwc_dro1$id_julian <- paste(acma_vwc_dro1$id_cor, acma_vwc_dro1$julian, sep = "_")


##### Bepa #####
# Subset
bepaM_subset <- subset(vwc_drought_1, species == 'Bepa')
# Rotate the table
bepaM_rot <- t(bepaM_subset)
# Convert to data frame
bepaM_dro1 <- as.data.frame(bepaM_rot, stringsAsFactors = FALSE)
# Set the first row as column names
colnames(bepaM_dro1) <- bepaM_dro1[1,]
# Remove the first 2 rows
bepaM_dro1 <- bepaM_dro1[-c(1:2),]
# Add julian column to df
bepaM_dro1['juliandays'] <- julianM
# Convert to numeric
for (i in 1:length(colnames(bepaM_dro1))) {
  bepaM_dro1[,i] <- as.numeric(bepaM_dro1[,i])
}
# Delete NA rows with dates that we have no data
bepaM_dro1 <- bepaM_dro1[rowSums(!is.na(bepaM_dro1) & bepaM_dro1 != "") > 1, ]

# Create a data frame for plotting
bepa_vwc_dro1 <- data.frame(
  julian = bepaM_dro1$juliandays,
  vwc = as.numeric(unlist(bepaM_dro1[, grepl("^Bepa", colnames(bepaM_dro1))])),
  id = rep(colnames(bepaM_dro1)[-ncol(bepaM_dro1)], each = nrow(bepaM_dro1))
)

# Paste cor to beginning of id
bepa_vwc_dro1$id_cor <- paste("cor", bepa_vwc_dro1$id, sep = "_")

# Create a unique id with julian day
bepa_vwc_dro1$id_julian <- paste(bepa_vwc_dro1$id_cor, bepa_vwc_dro1$julian, sep = "_")

##### Pico #####
# Subset
picoM_subset <- subset(vwc_drought_1, species == 'Pico')
# Rotate the table
picoM_rot <- t(picoM_subset)
# Convert to data frame
picoM_dro1 <- as.data.frame(picoM_rot, stringsAsFactors = FALSE)
# Set the first row as column names
colnames(picoM_dro1) <- picoM_dro1[1,]
# Remove the first 2 rows
picoM_dro1 <- picoM_dro1[-c(1:2),]
# Add julian column to df
picoM_dro1['juliandays'] <- julianM
# Convert to numeric
for (i in 1:length(colnames(picoM_dro1))) {
  picoM_dro1[,i] <- as.numeric(picoM_dro1[,i])
}
# Delete NA rows with dates that we have no data
picoM_dro1 <- picoM_dro1[rowSums(!is.na(picoM_dro1) & picoM_dro1 != "") > 1, ]

# Create a data frame for plotting
pico_vwc_dro1 <- data.frame(
  julian = picoM_dro1$juliandays,
  vwc = as.numeric(unlist(picoM_dro1[, grepl("^Pico", colnames(picoM_dro1))])),
  id = rep(colnames(picoM_dro1)[-ncol(picoM_dro1)], each = nrow(picoM_dro1))
)

# Paste cor to beginning of id
pico_vwc_dro1$id_cor <- paste("cor", pico_vwc_dro1$id, sep = "_")
# Create a unique id with julian day
pico_vwc_dro1$id_julian <- paste(pico_vwc_dro1$id_cor, pico_vwc_dro1$julian, sep = "_")

##### Quma #####
# Subset
qumaM_subset <- subset(vwc_drought_1, species == 'Quma')
# Rotate the table
qumaM_rot <- t(qumaM_subset)
# Convert to data frame
qumaM_dro1 <- as.data.frame(qumaM_rot, stringsAsFactors = FALSE)
# Set the first row as column names
colnames(qumaM_dro1) <- qumaM_dro1[1,]
# Remove the first 2 rows
qumaM_dro1 <- qumaM_dro1[-c(1:2),]
# Add julian column to df
qumaM_dro1['juliandays'] <- julianM
# Convert to numeric
for (i in 1:length(colnames(qumaM_dro1))) {
  qumaM_dro1[,i] <- as.numeric(qumaM_dro1[,i])
}
# Delete NA rows with dates that we have no data
qumaM_dro1 <- qumaM_dro1[rowSums(!is.na(qumaM_dro1) & qumaM_dro1 != "") > 1, ]

# Create a data frame for plotting
quma_vwc_dro1 <- data.frame(
  julian = qumaM_dro1$juliandays,
  vwc = as.numeric(unlist(qumaM_dro1[, grepl("^Quma", colnames(qumaM_dro1))])),
  id = rep(colnames(qumaM_dro1)[-ncol(qumaM_dro1)], each = nrow(qumaM_dro1))
)

# Paste cor to beginning of id
quma_vwc_dro1$id_cor <- paste("cor", quma_vwc_dro1$id, sep = "_")
# Create a unique id with julian day
quma_vwc_dro1$id_julian <- paste(quma_vwc_dro1$id_cor, quma_vwc_dro1$julian, sep = "_")

##### Sese #####
# Subset
seseM_subset <- subset(vwc_drought_1, species == 'Sese')
# Rotate the table
seseM_rot <- t(seseM_subset)
# Convert to data frame
seseM_dro1 <- as.data.frame(seseM_rot, stringsAsFactors = FALSE)
# Set the first row as column names
colnames(seseM_dro1) <- seseM_dro1[1,]
# Remove the first 2 rows
seseM_dro1 <- seseM_dro1[-c(1:2),]
# Add julian column to df
seseM_dro1['juliandays'] <- julianM
# Convert to numeric
for (i in 1:length(colnames(seseM_dro1))) {
  seseM_dro1[,i] <- as.numeric(seseM_dro1[,i])
}
# Delete NA rows with dates that we have no data
seseM_dro1 <- seseM_dro1[rowSums(!is.na(seseM_dro1) & seseM_dro1 != "") > 1, ]

# Create a data frame for plotting
sese_vwc_dro1 <- data.frame(
  julian = seseM_dro1$juliandays,
  vwc = as.numeric(unlist(seseM_dro1[, grepl("^Sese", colnames(seseM_dro1))])),
  id = rep(colnames(seseM_dro1)[-ncol(seseM_dro1)], each = nrow(seseM_dro1))
)

# Paste cor to beginning of id
sese_vwc_dro1$id_cor <- paste("cor", sese_vwc_dro1$id, sep = "_")
# Create a unique id with julian day
sese_vwc_dro1$id_julian <- paste(sese_vwc_dro1$id_cor, sese_vwc_dro1$julian, sep = "_")




### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

#### WEIGHT LOSS ####
###create vector of julian days that is length of the number of julian days
julianW<-colnames(weight_drought_1)[3:length(colnames(weight_drought_1))]

##### Prvi ####
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
prviW_Dro1 <- prviW_Dro1[rowSums(!is.na(prviW_Dro1) & prviW_Dro1 != "") > 1, ]
head(prviW_Dro1)
str(prviW_Dro1)
######make all columns numeric
for (i in 1:length(colnames(prviW_Dro1))) {
  prviW_Dro1[,i]<-as.numeric(prviW_Dro1[,i])
}

###calculate percentages to initial weight
tmp<-colnames(prviW_Dro1)[1:length(colnames(prviW_Dro1))]
for (i in 1:length(tmp)) {
  prviW_Dro1[, paste("cor", sep="_", (tmp[i]))]<-NA
  prviW_Dro1[, paste("cor", sep="_", (tmp[i]))]<-(100*prviW_Dro1[,tmp[i]])/prviW_Dro1[1,tmp[i]]
}
####temporary delete column julian days cor
#Drop column "juliandays_perc"
prviW_Dro1<-(prviW_Dro1[ ,  !names(prviW_Dro1) %in% 
                           c("cor_juliandays")])
# Filter columns that contain "perc"
perc_columns_prvi <- grep("cor", colnames(prviW_Dro1), value = TRUE)

# Reshape the data frame to long format
prvi_wl_dro1 <- prviW_Dro1 %>%
  gather(id, perc, all_of(perc_columns_prvi))

prvi_wl_dro1$id <- gsub("\r\n", "", prvi_wl_dro1$id)
# Paste julian to create a unique id 
prvi_wl_dro1$id_julian <- paste(prvi_wl_dro1$id, prvi_wl_dro1$juliandays, sep = "_")


##### Acma ####
# Subset
acmaW <- subset(weight_drought_1, species == "Acma")
str(acmaW)
# Rotate the table
acmaW_Dro1 <- t(acmaW)
# Convert to data frame
acmaW_Dro1 <- as.data.frame(acmaW_Dro1, stringsAsFactors = FALSE)
# Set the first row as column names
colnames(acmaW_Dro1) <- acmaW_Dro1[1, ]
# Remove the first two rows
acmaW_Dro1 <- acmaW_Dro1[-c(1:2),]
# Add julian column to df
acmaW_Dro1['juliandays'] <- julianW
# Remove rows that are all empty or all NA values
acmaW_Dro1 <- acmaW_Dro1[rowSums(!is.na(acmaW_Dro1) & acmaW_Dro1 != "") > 1, ]
head(acmaW_Dro1)
str(acmaW_Dro1)
######make all columns numeric
for (i in 1:length(colnames(acmaW_Dro1))) {
  acmaW_Dro1[,i] <- as.numeric(acmaW_Dro1[,i])
}

###calculate percentages to initial weight
tmp <- colnames(acmaW_Dro1)[1:length(colnames(acmaW_Dro1))]
for (i in 1:length(tmp)) {
  acmaW_Dro1[, paste("cor", sep="_", (tmp[i]))] <- NA
  acmaW_Dro1[, paste("cor", sep="_", (tmp[i]))] <- (100*acmaW_Dro1[,tmp[i]])/acmaW_Dro1[1,tmp[i]]
}
####temporary delete column julian days cor
#Drop column "juliandays_perc"
acmaW_Dro1 <- acmaW_Dro1[ ,  !names(acmaW_Dro1) %in% c("cor_juliandays")]
# Filter columns that contain "perc"
perc_columns_acma <- grep("cor", colnames(acmaW_Dro1), value = TRUE)

# Reshape the data frame to long format
acma_wl_dro1 <- acmaW_Dro1 %>%
  gather(id, perc, all_of(perc_columns_acma))

acma_wl_dro1$id <- gsub("\r\n", "", acma_wl_dro1$id)
# Paste julian to create a unique id 
acma_wl_dro1$id_julian <- paste(acma_wl_dro1$id, acma_wl_dro1$juliandays, sep = "_")

##### Bepa ####
# Subset
bepaW <- subset(weight_drought_1, species == "Bepa")
str(bepaW)
# Rotate the table
bepaW_Dro1 <- t(bepaW)
# Convert to data frame
bepaW_Dro1 <- as.data.frame(bepaW_Dro1, stringsAsFactors = FALSE)
# Set the first row as column names
colnames(bepaW_Dro1) <- bepaW_Dro1[1, ]
# Remove the first two rows
bepaW_Dro1 <- bepaW_Dro1[-c(1:2),]
# Add julian column to df
bepaW_Dro1['juliandays'] <- julianW
# Remove rows that are all empty or all NA values
bepaW_Dro1 <- bepaW_Dro1[rowSums(!is.na(bepaW_Dro1) & bepaW_Dro1 != "") > 1, ]
head(bepaW_Dro1)
str(bepaW_Dro1)
######make all columns numeric
for (i in 1:length(colnames(bepaW_Dro1))) {
  bepaW_Dro1[,i] <- as.numeric(bepaW_Dro1[,i])
}

###calculate percentages to initial weight
tmp <- colnames(bepaW_Dro1)[1:length(colnames(bepaW_Dro1))]
for (i in 1:length(tmp)) {
  bepaW_Dro1[, paste("cor", sep="_", (tmp[i]))] <- NA
  bepaW_Dro1[, paste("cor", sep="_", (tmp[i]))] <- (100*bepaW_Dro1[,tmp[i]])/bepaW_Dro1[1,tmp[i]]
}
####temporary delete column julian days cor
#Drop column "juliandays_perc"
bepaW_Dro1 <- bepaW_Dro1[ ,  !names(bepaW_Dro1) %in% c("cor_juliandays")]
# Filter columns that contain "perc"
perc_columns_bepa <- grep("cor", colnames(bepaW_Dro1), value = TRUE)

# Reshape the data frame to long format
bepa_wl_dro1 <- bepaW_Dro1 %>%
  gather(id, perc, all_of(perc_columns_bepa))

bepa_wl_dro1$id <- gsub("\r\n", "", bepa_wl_dro1$id)
# Paste julian to create a unique id 
bepa_wl_dro1$id_julian <- paste(bepa_wl_dro1$id, bepa_wl_dro1$juliandays, sep = "_")

##### Pico ####
# Subset
picoW <- subset(weight_drought_1, species == "Pico")
str(picoW)
# Rotate the table
picoW_Dro1 <- t(picoW)
# Convert to data frame
picoW_Dro1 <- as.data.frame(picoW_Dro1, stringsAsFactors = FALSE)
# Set the first row as column names
colnames(picoW_Dro1) <- picoW_Dro1[1, ]
# Remove the first two rows
picoW_Dro1 <- picoW_Dro1[-c(1:2),]
# Add julian column to df
picoW_Dro1['juliandays'] <- julianW
# Remove rows that are all empty or all NA values
picoW_Dro1 <- picoW_Dro1[rowSums(!is.na(picoW_Dro1) & picoW_Dro1 != "") > 1, ]
head(picoW_Dro1)
str(picoW_Dro1)
######make all columns numeric
for (i in 1:length(colnames(picoW_Dro1))) {
  picoW_Dro1[,i] <- as.numeric(picoW_Dro1[,i])
}

###calculate percentages to initial weight
tmp <- colnames(picoW_Dro1)[1:length(colnames(picoW_Dro1))]
for (i in 1:length(tmp)) {
  picoW_Dro1[, paste("cor", sep="_", (tmp[i]))] <- NA
  picoW_Dro1[, paste("cor", sep="_", (tmp[i]))] <- (100*picoW_Dro1[,tmp[i]])/picoW_Dro1[1,tmp[i]]
}
####temporary delete column julian days cor
#Drop column "juliandays_perc"
picoW_Dro1 <- picoW_Dro1[ ,  !names(picoW_Dro1) %in% c("cor_juliandays")]
# Filter columns that contain "perc"
perc_columns_pico <- grep("cor", colnames(picoW_Dro1), value = TRUE)

# Reshape the data frame to long format
pico_wl_dro1 <- picoW_Dro1 %>%
  gather(id, perc, all_of(perc_columns_pico))

pico_wl_dro1$id <- gsub("\r\n", "", pico_wl_dro1$id)
# Paste julian to create a unique id 
pico_wl_dro1$id_julian <- paste(pico_wl_dro1$id, pico_wl_dro1$juliandays, sep = "_")

##### Quma ####
# Subset
qumaW <- subset(weight_drought_1, species == "Quma")
str(qumaW)
# Rotate the table
qumaW_Dro1 <- t(qumaW)
# Convert to data frame
qumaW_Dro1 <- as.data.frame(qumaW_Dro1, stringsAsFactors = FALSE)
# Set the first row as column names
colnames(qumaW_Dro1) <- qumaW_Dro1[1, ]
# Remove the first two rows
qumaW_Dro1 <- qumaW_Dro1[-c(1:2),]
# Add julian column to df
qumaW_Dro1['juliandays'] <- julianW
# Remove rows that are all empty or all NA values
qumaW_Dro1 <- qumaW_Dro1[rowSums(!is.na(qumaW_Dro1) & qumaW_Dro1 != "") > 1, ]
head(qumaW_Dro1)
str(qumaW_Dro1)
######make all columns numeric
for (i in 1:length(colnames(qumaW_Dro1))) {
  qumaW_Dro1[,i] <- as.numeric(qumaW_Dro1[,i])
}

###calculate percentages to initial weight
tmp <- colnames(qumaW_Dro1)[1:length(colnames(qumaW_Dro1))]
for (i in 1:length(tmp)) {
  qumaW_Dro1[, paste("cor", sep="_", (tmp[i]))] <- NA
  qumaW_Dro1[, paste("cor", sep="_", (tmp[i]))] <- (100*qumaW_Dro1[,tmp[i]])/qumaW_Dro1[1,tmp[i]]
}
####temporary delete column julian days cor
#Drop column "juliandays_perc"
qumaW_Dro1 <- qumaW_Dro1[ ,  !names(qumaW_Dro1) %in% c("cor_juliandays")]
# Filter columns that contain "perc"
perc_columns_quma <- grep("cor", colnames(qumaW_Dro1), value = TRUE)

# Reshape the data frame to long format
quma_wl_dro1 <- qumaW_Dro1 %>%
  gather(id, perc, all_of(perc_columns_quma))

quma_wl_dro1$id <- gsub("\r\n", "", quma_wl_dro1$id)
# Paste julian to create a unique id 
quma_wl_dro1$id_julian <- paste(quma_wl_dro1$id, quma_wl_dro1$juliandays, sep = "_")

##### Sese ####
# Subset
seseW <- subset(weight_drought_1, species == "Sese")
str(seseW)
# Rotate the table
seseW_Dro1 <- t(seseW)
# Convert to data frame
seseW_Dro1 <- as.data.frame(seseW_Dro1, stringsAsFactors = FALSE)
# Set the first row as column names
colnames(seseW_Dro1) <- seseW_Dro1[1, ]
# Remove the first two rows
seseW_Dro1 <- seseW_Dro1[-c(1:2),]
# Add julian column to df
seseW_Dro1['juliandays'] <- julianW
# Remove rows that are all empty or all NA values
seseW_Dro1 <- seseW_Dro1[rowSums(!is.na(seseW_Dro1) & seseW_Dro1 != "") > 1, ]
head(seseW_Dro1)
str(seseW_Dro1)
######make all columns numeric
for (i in 1:length(colnames(seseW_Dro1))) {
  seseW_Dro1[,i] <- as.numeric(seseW_Dro1[,i])
}

###calculate percentages to initial weight
tmp <- colnames(seseW_Dro1)[1:length(colnames(seseW_Dro1))]
for (i in 1:length(tmp)) {
  seseW_Dro1[, paste("cor", sep="_", (tmp[i]))] <- NA
  seseW_Dro1[, paste("cor", sep="_", (tmp[i]))] <- (100*seseW_Dro1[,tmp[i]])/seseW_Dro1[1,tmp[i]]
}
####temporary delete column julian days cor
#Drop column "juliandays_perc"
seseW_Dro1 <- seseW_Dro1[ ,  !names(seseW_Dro1) %in% c("cor_juliandays")]
# Filter columns that contain "perc"
perc_columns_sese <- grep("cor", colnames(seseW_Dro1), value = TRUE)

# Reshape the data frame to long format
sese_wl_dro1 <- seseW_Dro1 %>%
  gather(id, perc, all_of(perc_columns_sese))

sese_wl_dro1$id <- gsub("\r\n", "", sese_wl_dro1$id)
# Paste julian to create a unique id 
sese_wl_dro1$id_julian <- paste(sese_wl_dro1$id, sese_wl_dro1$juliandays, sep = "_")

#### Correlations ####

##### Prvi #####
merged_prvi <- merge(prvi_wl_dro1[, c("id_julian", "perc")], prvi_vwc_dro1[, c("id_julian", "vwc")], by = "id_julian", all = TRUE)
head(merged_prvi)
regression_prvi <- lm(vwc ~ perc, data=merged_prvi) 
regression_prvi
summary(regression_prvi)

##### Acma #####
merged_acma <- merge(acma_wl_dro1[, c("id_julian", "perc")], acma_vwc_dro1[, c("id_julian", "vwc")], by = "id_julian", all = TRUE)
head(merged_prvi)
regression_acma <- lm(vwc ~ perc, data=merged_acma) 
regression_acma
summary(regression_acma)

##### Bepa #####
merged_bepa <- merge(bepa_wl_dro1[, c("id_julian", "perc")], bepa_vwc_dro1[, c("id_julian", "vwc")], by = "id_julian", all = TRUE)
head(merged_bepa)
regression_bepa <- lm(vwc ~ perc, data=merged_bepa)
regression_bepa
summary(regression_bepa)

##### Pico #####
merged_pico <- merge(pico_wl_dro1[, c("id_julian", "perc")], pico_vwc_dro1[, c("id_julian", "vwc")], by = "id_julian", all = TRUE)
head(merged_pico)
regression_pico <- lm(vwc ~ perc, data=merged_pico)
regression_pico
summary(regression_pico)

##### Quma #####
merged_quma <- merge(quma_wl_dro1[, c("id_julian", "perc")], quma_vwc_dro1[, c("id_julian", "vwc")], by = "id_julian", all = TRUE)
head(merged_quma)
regression_quma <- lm(vwc ~ perc, data=merged_quma)
regression_quma
summary(regression_quma)

##### Sese #####
merged_sese <- merge(sese_wl_dro1[, c("id_julian", "perc")], sese_vwc_dro1[, c("id_julian", "vwc")], by = "id_julian", all = TRUE)
head(merged_sese)
regression_sese <- lm(vwc ~ perc, data=merged_sese)
regression_sese
summary(regression_sese)

#### Plots ####

##### ALL SPECIES #####
allspecies <- ggplot() +
  geom_point(data=merged_prvi, aes(x=perc, y=vwc, colour = "Prvi"), size=1) +
  geom_point(data=merged_acma, aes(x=perc, y=vwc, colour = "Acma"), size=1) +
  geom_point(data=merged_bepa, aes(x=perc, y=vwc, colour = "Bepa"), size=1) +
  geom_point(data=merged_pico, aes(x=perc, y=vwc, colour = "Pico"), size=1) +
  geom_point(data=merged_quma, aes(x=perc, y=vwc, colour = "Quma"), size=1) +
  geom_point(data=merged_sese, aes(x=perc, y=vwc, colour = "Sese"), size=1) +
  geom_smooth(data = merged_prvi, aes(x = perc, y = vwc, colour = "Prvi"), method = "lm", se = FALSE) +
  geom_smooth(data = merged_acma, aes(x = perc, y = vwc, colour = "Acma"), method = "lm", se = FALSE) +
  geom_smooth(data = merged_bepa, aes(x = perc, y = vwc, colour = "Bepa"), method = "lm", se = FALSE) +
  geom_smooth(data = merged_pico, aes(x = perc, y = vwc, colour = "Pico"), method = "lm", se = FALSE) +
  geom_smooth(data = merged_quma, aes(x = perc, y = vwc, colour = "Quma"), method = "lm", se = FALSE) +
  geom_smooth(data = merged_sese, aes(x = perc, y = vwc, colour = "Sese"), method = "lm", se = FALSE) +
  scale_color_manual(name = "Species", values = c("Prvi" = "blue",
                                                     "Acma" = "chocolate",
                                                     "Bepa" = "coral",
                                                     "Pico" = "pink",
                                                     "Quma" = "black",
                                                     "Sese" = "green")) +
  labs(x = "Water content (%)", y = "Volumetric Water content (VWC)") +
  ggtitle("Correlation VWC by Water content")+
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5),
        legend.position = c(0.932, 0.78),
        legend.background = element_rect(fill="white", 
                                         linewidth =0.5, linetype="solid")) +
  xlim(100, 35) +
  ylim(0,50)
allspecies
ggsave("output/correlation/correlation_allspecies.pdf", allspecies, dpi = 300, width = 7, height = 5)

##### Prvi #####
print_prvi <- ggplot()+
  geom_point(data=merged_prvi, aes(x=perc, y=vwc, show.legend= FALSE), size=1)+
  geom_smooth(data = merged_prvi, aes(x = perc, y = vwc), method = "lm", se = FALSE, color="black") +
  labs(x = "Water content %", y = "Volumetric Water content") +
  ggtitle("Prvi - Drought 1")+
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5))
print_prvi<-print_prvi + theme(legend.position = "none")
print_prvi
ggsave("output/correlation/correlation_prvi.pdf", print_prvi, dpi = 300, width = 4, height = 4)

##### Acma #####
print_acma <- ggplot()+
  geom_point(data=merged_acma, aes(x=perc, y=vwc, show.legend=FALSE), size=1)+
  geom_smooth(data=merged_acma, aes(x=perc, y=vwc), method="lm", se=FALSE, color="black") +
  labs(x="Water content %", y="Volumetric Water content") +
  ggtitle("Acma - Drought 1") +
  theme_minimal() +
  theme(panel.grid=element_blank(),
        panel.background=element_rect(fill="white"),
        plot.title=element_text(hjust=0.5))
print_acma <- print_acma + theme(legend.position="none")
print_acma
ggsave("output/correlation/correlation_acma.pdf", print_acma, dpi=300, width=4, height=4)

##### Bepa #####
print_bepa <- ggplot() +
  geom_point(data=merged_bepa, aes(x=perc, y=vwc, show.legend=FALSE), size=1) +
  geom_smooth(data=merged_bepa, aes(x=perc, y=vwc), method="lm", se=FALSE, color="black") +
  labs(x="Water content %", y="Volumetric Water content") +
  ggtitle("Bepa - Drought 1") +
  theme_minimal() +
  theme(panel.grid=element_blank(),
        panel.background=element_rect(fill="white"),
        plot.title=element_text(hjust=0.5))
print_bepa <- print_bepa + theme(legend.position="none")
print_bepa
ggsave("output/correlation/correlation_bepa.pdf", print_bepa, dpi=300, width=4, height=4)

##### Pico #####
print_pico <- ggplot() +
  geom_point(data=merged_pico, aes(x=perc, y=vwc, show.legend=FALSE), size=1) +
  geom_smooth(data=merged_pico, aes(x=perc, y=vwc), method="lm", se=FALSE, color="black") +
  labs(x="Water content %", y="Volumetric Water content") +
  ggtitle("Pico - Drought 1") +
  theme_minimal() +
  theme(panel.grid=element_blank(),
        panel.background=element_rect(fill="white"),
        plot.title=element_text(hjust=0.5))
print_pico <- print_pico + theme(legend.position="none")
print_pico
ggsave("output/correlation/correlation_pico.pdf", print_pico, dpi=300, width=4, height=4)

##### Quma #####
print_quma <- ggplot() +
  geom_point(data=merged_quma, aes(x=perc, y=vwc, show.legend=FALSE), size=1) +
  geom_smooth(data=merged_quma, aes(x=perc, y=vwc), method="lm", se=FALSE, color="black") +
  labs(x="Water content %", y="Volumetric Water content") +
  ggtitle("Quma - Drought 1") +
  theme_minimal() +
  theme(panel.grid=element_blank(),
        panel.background=element_rect(fill="white"),
        plot.title=element_text(hjust=0.5))
print_quma <- print_quma + theme(legend.position="none")
print_quma
ggsave("output/correlation/correlation_quma.pdf", print_quma, dpi=300, width=4, height=4)

##### Sese #####
print_sese <- ggplot() +
  geom_point(data=merged_sese, aes(x=perc, y=vwc, show.legend=FALSE), size=1) +
  geom_smooth(data=merged_sese, aes(x=perc, y=vwc), method="lm", se=FALSE, color="black") +
  labs(x="Water content %", y="Volumetric Water content") +
  ggtitle("Sese - Drought 1") +
  theme_minimal() +
  theme(panel.grid=element_blank(),
        panel.background=element_rect(fill="white"),
        plot.title=element_text(hjust=0.5))
print_sese <- print_sese + theme(legend.position="none")
print_sese
ggsave("output/correlation/correlation_sese.pdf", print_sese, dpi=300, width=4, height=4)
