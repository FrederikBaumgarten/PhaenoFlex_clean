#--------------------------------------------------------------------
# Senescence data 
# Author: Britany Wu
# Date: July 8th
#--------------------------------------------------------------------
# Housekeeping
rm(list = ls())

if(length(grep("britanywuuu", getwd()) > 0)) {
  setwd("/Users/britanywuuu/UBC/tel/PhaenoFlex_clean/analysis")
} else if(length(grep("frederik", getwd()) > 0)) {
  setwd("/Users/frederik/github/PhaenoFlex_clean/analysis")
}
library(readxl)
library(ggplot2)

# Load----------------------------------------------------------------
d.a <- read_excel("input/senescence/senescence_Amax.xlsx")
d.other <- read_excel("input/senescence/senescence_5_Sept_FB.xlsx", sheet = "percentage")

# Format data 
rep.info <- d.other[, 1:4]
d.index <- cbind(rep.info, d.other[, grep("INDEX", colnames(d.other))])
d.percent <- cbind(rep.info, d.other[, grep("PERC", colnames(d.other))])
d.cci <- cbind(rep.info, d.other[, grep("CCI", colnames(d.other))])

# Function to subset data based on species
subset_data <- function(species, a, index, percen, CCI) {
  list(
    raw_A = subset(a, spec == species),
    index = subset(index, spec == species),
    percen = subset(percen, spec == species),
    CCI = subset(CCI, spec == species)
  )
}

species_list <- c("Prvi", "Acma", "Bepa", "Quma")
data_list <- lapply(species_list, subset_data, a = d.a, index = d.index, percen = d.percent, CCI = d.cci) #subset the full data into lists according to species and type of measurment

##cleaning ----------------------------------------------------------
#create a data frame with 6 columns: id, species, treatment, doy, value, and type
d.cleaned <- data.frame(matrix(data = NA, ncol = 5, nrow = 0)) #create a empty dataframe to store the cleaned data 
for (i in 1:length(data_list)){ #a loop to extract doy and values for tyep "A"
  list <- data_list[[i]]
  for (n in 1:nrow(list[["raw_A"]])){
    data <- list[["raw_A"]]
    A <- t(data[n, c(7:19)]) #select value columns and transpose them so that value will be in a single column 
    print(A)
    doy <- colnames(data)[7:19] #select doy values from column names of the xlsx file
    d.temp <- data.frame((matrix(data = NA, ncol = 0, nrow = length(doy)))) #create a temp dataframe
    d.temp$id <- data$tree_ID[n]
    d.temp$species <- data$spec[n]
    d.temp$treatment <- data$drought_timing[n]
    d.temp$doy <- as.numeric(doy)
    d.temp$value <- A[1:13]
    d.temp$type <- "A"
    d.cleaned <- rbind(d.cleaned, d.temp) #append temp to d.cleaned dataframe
  }
  for (n in 1:nrow(list[["percen"]])){ #a loop to extract doy and values for tyep "PERC" (same structure as above)
    data <- list[["percen"]]
    P <- t(data[n, c(5:13)]) #transpose
    doy <- sub("_PERC", "", colnames(data)[grepl("PERC", colnames(data))])
    d.temp <- data.frame((matrix(data = NA, ncol = 0, nrow = length(doy))))
    d.temp$id <- data$ID[n]
    d.temp$species <- data$spec[n]
    d.temp$treatment <- data$drought_timing[n]
    d.temp$doy <- as.numeric(doy)
    d.temp$value <- P[1:9]
    d.temp$type <- "Percentage"
    for (o in 1:nrow(d.temp)){ # add 100 (before monitoring) or 0 (after senescence) to empty cells 
      if (o > 1 && !is.na(d.temp$value[o - 1]) && (is.na(d.temp$value[o]))){
        d.temp$value[o] <- 0
      } else if (is.na(d.temp$value[o]) && !(is.na(d.temp$value[o + 1]))) {
        d.temp$value[o] <- 100
      }
      }
    d.cleaned <- rbind(d.cleaned, d.temp)
  }
    for (n in 1:nrow(list[["CCI"]])){ #a loop to extract doy and values for tyep "CCI" (same structure as above)
    data <- list[["CCI"]]
    CCI <- t(data[n, c(5:13)]) #transpose
    doy <- sub("_CCI", "", colnames(data)[grepl("CCI", colnames(data))])
    d.temp <- data.frame((matrix(data = NA, ncol = 0, nrow = length(doy))))
    d.temp$id <- data$ID[n]
    d.temp$species <- data$spec[n]
    d.temp$treatment <- data$drought_timing[n]
    d.temp$doy <- as.numeric(doy)
    d.temp$value <- CCI[1:9]
    d.temp$type <- "CCI"
    for (o in 1:nrow(d.temp)) {
      match <- which(d.cleaned$id == d.temp$id[o] & d.cleaned$doy == d.temp$doy[o] & d.cleaned$type == "Percentage")
      if (o > 1 && length(match) > 0 && !is.na(d.cleaned$value[match]) && is.na(d.temp$value[o])) {
        d.temp$value[o] <- d.temp$value[o - 1]
      }
      else if (o == nrow(d.temp) && is.na(d.temp$value[o])){
        d.temp$value[o]<- 0
      }
    }
    d.cleaned <- rbind(d.cleaned, d.temp)
  }
}
d.cleaned$value <- as.numeric(d.cleaned$value)
head(d.cleaned)
tail(d.cleaned)
print(d.cleaned$id[which(d.cleaned$type == "Percentage" & is.na(d.cleaned$value))])
print(d.cleaned$id[which(d.cleaned$type == "Percentage" & is.na(d.cleaned$value))])
cci <- subset(d.cleaned, d.cleaned$type == "CCI")
percent <- subset(d.cleaned, d.cleaned$type == "Percentage")
print(percent)
unique(percent$value)
print(cci)
#preliminary plots--------------------------------------------------
#plotting to see the original data
colors <- rainbow(length(unique(d.cleaned$type))) 
names(colors) <- unique(d.cleaned$type)

for (spec in species_list){
  file <-file.path(paste0("output/", spec, "Rawplot", ".pdf"))
  pdf(file, width = 25, height = 10)
  par(mfrow = c(2, 5), mar = c(5, 2, 2, 4), oma = c(2, 2, 4, 2))
  data <- subset(d.cleaned, d.cleaned$species == spec)
  for (trmt in unique(data$treatment)){
    trmt_data <- subset(data, treatment == trmt)
    primary <- subset(trmt_data, type == "Percentage")
    secondary <- subset(trmt_data, type != "Percentage")
    plot(primary$doy, primary$value,
         pch = 16, xlab = "DOY", ylab = "",
         xlim = c(216, 304), ylim = c(0, 100),
         main = paste(trmt),
         xaxt = 'n', yaxt = 'n', cex.axis = 0.7)
    mtext("Percentage", side = 2, line = 2, col = "black", cex = 0.7)
    
    axis(1, at = seq(216, 304, by = 10), cex.axis = 0.7)
    axis(2, at = seq(0, 100, by = 10), cex.axis = 0.7)
    par(new = TRUE)
    plot(secondary$doy, secondary$value, col = colors[secondary$type], #plotting secondary axis (A and CCI)
         pch = 16, axes = FALSE, xlab = "", ylab = "",
         xlim = c(216, 304), ylim = c(0, 10))
    axis(4, at = seq(0, 10, by = 1), col = "red", col.axis = "red")
    mtext("Amax value", side = 4, line = 2, col = "red", cex = 0.7)
    legend("topright", legend = names(colors), col = colors, pch = 16, cex = 0.7)
  }
  mtext(paste(spec), side = 3, outer = TRUE, line = 1, cex = 1.5)
  dev.off()
} 

## Logistic function for senescence data-----------------------------
# code from Fredi back in November 2023
senes_sim <- function(t, a, b, c) {
  y <- a / (1 + exp((t - b) / c)) + rnorm(length(t), 0, 0.5) # logistic function with noise
  return(y)
}

# Parameters
a <- 25     # Asymptote or maximum cholorophyll concentration
b <- 180    # the day when the cholorophyll concentration decrease is the fastest
c <- 5      # controls the rate of decrease

# Generate x values
t <- seq(150, 310) # time: day of the year from a selected start and end date #note 2024 July 25th: the upper limit should be ~310

# Simulate senescence process using a logistic function
y <- senes_sim(t, a, b, c)
# Plot the simulated data senescence process
png("output/simulatedSenescence.png")
plot(t, y, type = "l", xlab = "Day of the year", ylab = "A",
     main = "Senescence process of deciduous trees in autumn")
dev.off()
#Fit data to SSlogis using simulated data 
dat<-data.frame(values= y, time= t) #create data frame with simulated data
fit <- nls(values ~ SSlogis(time, a, b, c), data = dat)
png("output/simulatedSenescenceFit.png")
plot(t,y, type = "l", xlab = "Day of the year", ylab = "A",
  main = "Senescence process of deciduous trees in autumn")
curve(predict(fit, newdata = data.frame(time = x), se = FALSE), 
        add = TRUE, col = "red")
dev.off()
summary(fit)
#plot the fit on the simulated data

### Fit data to SSlogis----------------------------------------------
###Amax
d.all <- data.frame(matrix(data = NA, ncol = 4, nrow = 0 )) # create a dataframe for all the slopes #2024 July 25th: extract also a and c into d.all
fits.list.A <- list() # create a list to store fit for each rep
d.A <- subset(d.cleaned, type == "A")
d.A$value <- as.numeric(d.A$value)
d.A$doy <- as.numeric(d.A$doy)
for (i in 1:length(unique(d.A$id))){
  rep <- unique(d.A$id)[i]
  d <- subset(d.A, d.A$id == rep)
  fit.dat <- data.frame(value = d$value, time = d$doy)
  fit <- tryCatch({ #fit the nls to each rep, and writes out data id if there's error
    nls(value ~ SSlogis(time, a,b,c), data = fit.dat)
  }, error = function (e){
    cat("Error in fitting for id:", rep, "\nError message:", e$message, "Skipping. \n")
    return(NULL)
  })
  if (!is.null(fit)) { #if the data fit successfully store the coefficients to the data frame
    a_value <- coef(fit)['a']
    b_value <- coef(fit)['b']
    c_value <- coef(fit)['c']
    fits.list.A[[paste0(rep, "_fit_A")]] <- fit #save fit into the list 
    temp <- data.frame(matrix(data = NA, ncol = 0, nrow = 1))
    temp$species <- unique(d$species[which(d$id == rep)])
    temp$treatment <- unique(d$treatment[which(d$id == rep)])
    temp$id <- rep
    temp$a <- a_value
    temp$b <- b_value
    temp$c <- c_value
    temp$type <- "A"
    d.all <- rbind(d.all, temp) #append b to the big dataframe
  }
}
##CCI
d.C <- subset(d.cleaned, type == "CCI")
fits.list.C <- list()
d.C$value <- as.numeric(d.C$value)
d.C$doy <- as.numeric(d.C$doy)
for (i in 1:length(unique(d.C$id))){
  rep <- unique(d.C$id)[i]
  d <- subset(d.C, d.C$id == rep)
  fit.dat <- data.frame(value = d$value, time = d$doy)
  fit <- tryCatch({
    nls(value ~ SSlogis(time, a,b,c), data = fit.dat)
  }, error = function (e){
    cat("Error in fitting for id:", rep, "\nError message:", e$message, "Skipping. \n")
    return(NULL)
  })
  if (!is.null(fit)) {
    a_value <- coef(fit)['a']
    b_value <- coef(fit)['b']
    c_value <- coef(fit)['c']
    fits.list.C[[paste0(rep, "_fit_C")]] <- fit #save fit into the list 
    temp <- data.frame(matrix(data = NA, ncol = 0, nrow = 1))
    temp$species <- unique(d$species[which(d$id == rep)])
    temp$treatment <- unique(d$treatment[which(d$id == rep)])
    temp$id <- rep
    temp$a <- a_value
    temp$b <- b_value
    temp$c <- c_value
    temp$type <- "CCI"
    d.all <- rbind(d.all, temp) #append b to the big dataframe
  }
}
## Percentage
d.P <- subset(d.cleaned, type == "Percentage")
fits.list.P <- list()
d.P$value <- as.numeric(d.P$value)
d.P$doy <- as.numeric(d.P$doy)
for (i in 1:length(unique(d.P$id))){
  rep <- unique(d.P$id)[i]
  d <- subset(d.P, d.P$id == rep)
  fit.dat <- data.frame(value = d$value, time = d$doy)
  fit <- tryCatch({
    nls(value ~ SSlogis(time, a,b,c), data = fit.dat)
  }, error = function (e){
    cat("Error in fitting for id:", rep, "\nError message:", e$message, "Skipping. \n")
    return(NULL)
  })
  if (!is.null(fit)) {
    a_value <- coef(fit)['a']
    b_value <- coef(fit)['b']
    c_value <- coef(fit)['c']
    fits.list.P[[paste0(rep, "_fit_P")]] <- fit
    temp <- data.frame(matrix(data = NA, ncol = 0, nrow = 1))
    temp$species <- unique(d$species[which(d$id == rep)])
    temp$treatment <- unique(d$treatment[which(d$id == rep)])
    temp$id <- rep
    temp$a <- a_value
    temp$b <- b_value
    temp$c <- c_value
    temp$type <- "Percentage"
    d.all <- rbind(d.all, temp)
  }
}
#
# Write a function to extract the doy for the perce
standardization <- function(params, percent) {
  a <- params["a"]
  b <- params["b"]
  c <- params["c"]
  value <- percent*a
  doy <- b - c * log((a / value) - 1)
  return(doy)
}
# extrac_paramegters from a fit 
extract_params <- function(fit) {
  coef(fit)
}
d.all$per10 <- NA
d.all$per50 <- NA
d.all$per90 <- NA

# extract doy for 10%, 50% and 90% senescence
for (i in 1:nrow(d.all)){
  rep <- d.all$id[i]
  if (d.all$type[i] == "A") {
    fit_index <- grep(rep, names(fits.list.A))
    if (length(fit_index) > 0) {
      fit <- fits.list.A[[fit_index[1]]]  # Use the first match
      # Continue with your processing using the fit
      params <- extract_params(fit)
      d.all$per10[which(d.all$id == rep & d.all$type == "A")] <- standardization(params, 0.1)
      d.all$per50[which(d.all$id == rep & d.all$type == "A")] <- standardization(params, 0.5)
      d.all$per90[which(d.all$id == rep & d.all$type == "A")] <- standardization(params, 0.9)
    } else {
      cat("Fit not found for id:", rep, "in fits.list.A\n")
    }
  }
  else if (d.all$type[i] == "CCI") {
    fit_index <- grep(rep, names(fits.list.C))
    if (length(fit_index) > 0) {
      fit <- fits.list.C[[fit_index[1]]]  # Use the first match
      params <- extract_params(fit)
      d.all$per10[which(d.all$id == rep & d.all$type == "CCI")] <- standardization(params, 0.1)
      d.all$per50[which(d.all$id == rep & d.all$type == "CCI")] <- standardization(params, 0.5)
      d.all$per90[which(d.all$id == rep & d.all$type == "CCI")] <- standardization(params, 0.9)
      
    } else {
      cat("Fit not found for id:", rep, "in fits.list.C\n")
    }
  }
  else if (d.all$type[i] == "Percentage") {
    fit_index <- grep(rep, names(fits.list.P))
    if (length(fit_index) > 0) {
      fit <- fits.list.P[[fit_index[1]]]  # Use the first match
      params <- extract_params(fit)
      d.all$per10[which(d.all$id == rep & d.all$type == "Percentage")] <- standardization(params, 0.1)
      d.all$per50[which(d.all$id == rep & d.all$type == "Percentage")] <- standardization(params, 0.5)
      d.all$per90[which(d.all$id == rep & d.all$type == "Percentage")] <- standardization(params, 0.9)
    } else {
      cat("Fit not found for id:", rep, "in fits.list.P\n")
    }
  } else {
    cat("ID:", rep, "NA for type in d.all" )
  }
}

## export the summary table for each individual
write.csv(d.all, "output/repsummary.csv")
head(d.all)
#summary data (Q10, Q50, Q90, and b) for each treatment in each species 
#should I separate by each metrics? Maybe...
#try tapply() function 
treatment_group <- split(d.all, d.all$species) #The end format isn't pleasing
# trmt <- d.all$treatment
# type <- d.all$type
# species <- d.all$species
# per10 <- d.all$per10
# per10_mean <- tapply(per10, list(species, trmt, type), mean)
treatment_group <- lapply(treatment_group, function(sub_df) {
  treatment_group <- split(sub_df, sub_df$treatment)
  lapply(treatment_group, function(type_df){
    split(type_df, type_df$type) 
    })
  }) # split d.all by species and treatment under each species
standard_error <- function(x) {
  sd(x) / sqrt(length(x))
}
d.summary <- data.frame(matrix(data = NA, nrow = 0, ncol = 5)) #create a data frame for mean and sd for each species + treatment combination 
for (i in 1:length(names(treatment_group))){
  d.spec <- treatment_group[[i]]
  for (n in 1:length(names(d.spec))){
    d.trmt <- d.spec[[n]]
    for (o in 1:length(names(d.trmt))){
      data <- d.trmt[[o]]
      temp <- data.frame(matrix(data = NA, ncol = 0, nrow = 1))
      temp$species <- unique(data$species)
      temp$trmt <- unique(data$treatment)
      temp$a <- mean(data$a)
      temp$ase <- standard_error(data$a)
      temp$b <- mean(data$b)
      temp$bse <- standard_error(data$b)
      temp$c <- mean(data$c)
      temp$cse <- standard_error(data$c)
      temp$per10mean <- mean(data$per10)
      temp$per10se <- standard_error(data$per10)
      temp$per50mean <- mean(data$per50)
      temp$per50se <- standard_error(data$per50)
      temp$per90mean <- mean(data$per90)
      temp$per90se <- standard_error(data$per90)
      temp$type <- unique(data$type)
      d.summary <- rbind(d.summary, temp)
    }
  }
}
# export summary table for each treatmenrt + species
write.csv(d.summary, "output/treatmentFitSummary.csv")

##plotting-------------------------------------------------
#treatment as x-axis
#A
for (i in 1: length(species_list)){
  spec <- species_list[i]
  df <- subset(d.summary, d.summary$species == spec & d.summary$type == "A")
  filenameA <- paste0("output/", spec, "PlotA.png")

  png(filenameA)
  plot(1, type = "n", xlim = c(1, length(unique(df$trmt))), 
    ylim = c(min(df$per90mean) - 20 , max(df$per10mean) + 20),
    xaxt = "n", xlab = "Treatment", ylab = "doy", main = paste("Doy of the % decrese in Amax value for each treatment of", spec))
  axis(1, at = 1:length(unique(df$trmt)), labels = unique(df$trmt))
  points(as.factor(df$trmt), df$per10mean, pch = 19, col = "red")
  points(as.factor(df$trmt), df$per50mean, pch = 19, col = "blue")
  points(as.factor(df$trmt), df$per90mean, pch = 19, col = "green")
  arrows(as.numeric((as.factor(df$trmt))), df$per10mean - df$per10se, as.numeric((as.factor(df$trmt))), df$per10mean + df$per10se, angle = 90, code = 3, length = 0.1)
  arrows(as.numeric((as.factor(df$trmt))), df$per50mean - df$per50se, as.numeric((as.factor(df$trmt))), df$per50mean + df$per50se, angle = 90, code = 3, length = 0.1)
  arrows(as.numeric((as.factor(df$trmt))), df$per90mean - df$per90se, as.numeric((as.factor(df$trmt))), df$per90mean + df$per90se, angle = 90, code = 3, length = 0.1)
  dev.off()
}
#Perc
for (i in 1: length(species_list)){
  spec <- species_list[i]
  df <- subset(d.summary, d.summary$species == spec & d.summary$type == "Percentage")
  filenameA <- paste0("output/", spec, "PlotPerc.png")

  png(filenameA)
  plot(1, type = "n", xlim = c(1, length(unique(df$trmt))), 
    ylim = c(min(df$per90mean) - 20 , max(df$per10mean) + 20),
    xaxt = "n", xlab = "Treatment", ylab = "doy", main = paste("Doy of the % decrese in Amax value for each treatment of", spec))
  axis(1, at = 1:length(unique(df$trmt)), labels = unique(df$trmt))
  points(as.factor(df$trmt), df$per10mean, pch = 19, col = "red")
  points(as.factor(df$trmt), df$per50mean, pch = 19, col = "blue")
  points(as.factor(df$trmt), df$per90mean, pch = 19, col = "green")
  arrows(as.numeric((as.factor(df$trmt))), df$per10mean - df$per10se, as.numeric((as.factor(df$trmt))), df$per10mean + df$per10se, angle = 90, code = 3, length = 0.1)
  arrows(as.numeric((as.factor(df$trmt))), df$per50mean - df$per50se, as.numeric((as.factor(df$trmt))), df$per50mean + df$per50se, angle = 90, code = 3, length = 0.1)
  arrows(as.numeric((as.factor(df$trmt))), df$per90mean - df$per90se, as.numeric((as.factor(df$trmt))), df$per90mean + df$per90se, angle = 90, code = 3, length = 0.1)
  dev.off()
}
#CCI
for (i in 1: length(species_list)){
  spec <- species_list[i]
  df <- subset(d.summary, d.summary$species == spec & d.summary$type == "CCI")
  filenameA <- paste0("output/", spec, "PlotCCI.png")

  png(filenameA)
  plot(1, type = "n", xlim = c(1, length(unique(df$trmt))), 
    ylim = c(min(df$per90mean) - 20 , max(df$per10mean) + 20),
    xaxt = "n", xlab = "Treatment", ylab = "doy", main = paste("Doy of the % decrese in Amax value for each treatment of", spec))
  axis(1, at = 1:length(unique(df$trmt)), labels = unique(df$trmt))
  points(as.factor(df$trmt), df$per10mean, pch = 19, col = "red")
  points(as.factor(df$trmt), df$per50mean, pch = 19, col = "blue")
  points(as.factor(df$trmt), df$per90mean, pch = 19, col = "green")
  arrows(as.numeric((as.factor(df$trmt))), df$per10mean - df$per10se, as.numeric((as.factor(df$trmt))), df$per10mean + df$per10se, angle = 90, code = 3, length = 0.1)
  arrows(as.numeric((as.factor(df$trmt))), df$per50mean - df$per50se, as.numeric((as.factor(df$trmt))), df$per50mean + df$per50se, angle = 90, code = 3, length = 0.1)
  arrows(as.numeric((as.factor(df$trmt))), df$per90mean - df$per90se, as.numeric((as.factor(df$trmt))), df$per90mean + df$per90se, angle = 90, code = 3, length = 0.1)
  dev.off()
}
