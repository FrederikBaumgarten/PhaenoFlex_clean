#--------------------------------------------------------------------
# Senescence data 
# Author: Britany Wu
# Date: July 8th
#--------------------------------------------------------------------
# Housekeeping
rm(list = ls())

path <- "~/Documents/ubc/year5/TemporalEcologyLab/PhaenoFlex_clean/analysis/"
path <- "/Users/frederik/github/PhaenoFlex_clean/analysis"
setwd(path)

library(ggplot2)
library(gridExtra)
library(readxl)
library(dplyr)
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
data_list <- lapply(species_list, subset_data, a = d.a, index = d.index, percen = d.percent, CCI = d.cci)


##cleaning ----------------------------------------------------------
#create a data frame with 6 columns: id, species, treatment, doy, value, and type
###prvi--------------------------------------------------------------
d.cleaned <- data.frame(matrix(data = NA, ncol = 5, nrow = 0))
for (i in 1:length(data_list)){
  list <- data_list[[i]]
  for (n in 1:nrow(list[["raw_A"]])){
    data <- list[["raw_A"]]
    A <- t(data[n, c(7:19)]) #transpose
    print(A)
    doy <- colnames(data)[7:19]
    d.temp <- data.frame((matrix(data = NA, ncol = 0, nrow = length(doy))))
    d.temp$id <- data$tree_ID[n]
    d.temp$species <- data$spec[n]
    d.temp$treatment <- data$drought_timing[n]
    d.temp$doy <- as.numeric(doy)
    d.temp$value <- A[1:13]
    d.temp$type <- "A"
    d.cleaned <- rbind(d.cleaned, d.temp)
  }
  for (n in 1:nrow(list[["CCI"]])){
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
    d.cleaned <- rbind(d.cleaned, d.temp)
  }
  for (n in 1:nrow(list[["percen"]])){
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
    d.cleaned <- rbind(d.cleaned, d.temp)
  }
}
d.cleaned$value <- as.numeric(d.cleaned$value)
#preliminary plots--------------------------------------------------
colors <- rainbow(length(unique(d.cleaned$type)))
names(colors) <- unique(d.cleaned$type)

for (spec in species_list){
  file <-file.path(paste0("output/senescence_plots/", spec, "_rawplot", ".pdf"))
  pdf(file, width = 25, height = 10)
  par(mfrow = c(2, 5), mar = c(5, 2, 2, 4), oma = c(2, 2, 4, 2))
  data <- subset(d.cleaned, d.cleaned$species == spec)
  for (trmt in unique(data$treatment)){
    trmt_data <- subset(data, treatment == trmt)
    primary <- subset(trmt_data, type != "A")
    secondary <- subset(trmt_data, type == "A")
    plot(primary$doy, primary$value, col = colors[primary$type],
         pch = 16, xlab = "DOY", ylab = "",
         xlim = c(216, 304), ylim = c(0, 100),
         main = paste(trmt),
         xaxt = 'n', yaxt = 'n', cex.axis = 0.7)
    mtext("Percentage", side = 2, line = 2, col = "black", cex = 0.7)
    
    axis(1, at = seq(216, 304, by = 10), cex.axis = 0.7)
    axis(2, at = seq(0, 100, by = 10), cex.axis = 0.7)
    par(new = TRUE)
    plot(secondary$doy, secondary$value, col = "red",
         pch = 16, axes = FALSE, xlab = "", ylab = "",
         xlim = c(216, 304), ylim = c(0, 10))
    axis(4, at = seq(0, 10, by = 1), col = "red", col.axis = "red")
    mtext("Amax value", side = 4, line = 2, col = "red", cex = 0.7)
    legend("topright", legend = names(colors), col = colors, pch = 16, cex = 0.7)
  }
  mtext(paste(spec), side = 3, outer = TRUE, line = 1, cex = 1.5)
  dev.off()
} 
par(mfrow = c(1, 1))

## Logistic function for senescence data-----------------------------
senes_sim <- function(t, a, b, c) {
  y <- a / (1 + exp((t - b) / c)) + rnorm(length(t), 0, 0.5) # logistic function with noise
  return(y)
}

# Parameters
a <- 25     # Asymptote or maximum cholorophyll concentration
b <- 180    # the day when the cholorophyll concentration decrease is the fastest
c <- 5      # controls the rate of decrease

# Generate x values
t <- seq(150, 210) # time: day of the year from a selected start and end date

# Simulate senescence process using a logistic function
y <- senes_sim(t, a, b, c)
# Plot the simulated data senescence process
png("output/senescence_plots/simulated.png")
plot(t, y, type = "l", xlab = "Day of the year", ylab = "A",
     main = "Senescence process of deciduous trees in autumn")
dev.off()
#Fit data to SSlogis using simulated data 
dat<-data.frame(values= y, time= t) #create data frame with simulated data
fit <- nls(values ~ SSlogis(time, a, b, c), data = dat)
summary(fit)

### Fit data to SSlogis----------------------------------------------
###Amax
d.all <- data.frame(matrix(data = NA, ncol = 4, nrow = 0 )) # create a dataframe for all the slopes
fits.list.A <- list() # create a list to store fit for each rep
d.A <- subset(d.cleaned, type == "A")
d.A$value <- as.numeric(d.A$value)
d.A$doy <- as.numeric(d.A$doy)
for (i in 1:length(unique(d.A$id))){
  rep <- unique(d.A$id)[i]
  d <- subset(d.A, d.A$id == rep)
  fit.dat <- data.frame(value = d$value, time = d$doy)
  fit <- tryCatch({
    nls(value ~ SSlogis(time, a,b,c), data = fit.dat)
  }, error = function (e){
    cat("Error in fitting for id:", rep, "\nError message:", e$message, "Skipping. \n")
    return(NULL)
  })
  if (!is.null(fit)) {
    b_value <- coef(fit)['b']
    fits.list.A[[paste0(rep, "_fit_A")]] <- fit
    temp <- data.frame(matrix(data = NA, ncol = 0, nrow = 1))
    temp$species <- unique(d$species[which(d$id == rep)])
    temp$treatment <- unique(d$treatment[which(d$id == rep)])
    temp$id <- rep
    temp$b <- b_value
    temp$type <- "A"
    d.all <- rbind(d.all, temp)
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
    b_value <- coef(fit)['b'] # extract slope from the fitted curve
    fits.list.C[[paste0(rep, "_fit_C")]] <- fit #save fit into the list 
    temp <- data.frame(matrix(data = NA, ncol = 0, nrow = 1))
    temp$species <- unique(d$species[which(d$id == rep)])
    temp$treatment <- unique(d$treatment[which(d$id == rep)])
    temp$id <- rep
    temp$b <- b_value
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
    b_value <- coef(fit)['b']
    fits.list.P[[paste0(rep, "_fit_P")]] <- fit
    temp <- data.frame(matrix(data = NA, ncol = 0, nrow = 1))
    temp$species <- unique(d$species[which(d$id == rep)])
    temp$treatment <- unique(d$treatment[which(d$id == rep)])
    temp$id <- rep
    temp$b <- b_value
    temp$type <- "Percentage"
    d.all <- rbind(d.all, temp)
  }
}
#
# Write a function to extract quantiles
logistic_quantile <- function(params, percent) {
  a <- params["a"]
  b <- params["b"]
  c <- params["c"]
  value <- percent*a
  doy <- b - c * log((a / value) - 1)
  return(doy)
}
# extrac_params
extract_params <- function(fit) {
  coef(fit)
}
d.all$ten <- NA
d.all$fifty <- NA
d.all$ninty <- NA

# extract doy for 10%, 50% and 90% senescence
for (i in 1:nrow(d.all)){
  rep <- d.all$id[i]
  if (d.all$type[i] == "A") {
    fit_index <- grep(rep, names(fits.list.A))
    if (length(fit_index) > 0) {
      fit <- fits.list.A[[fit_index[1]]]  # Use the first match
      # Continue with your processing using the fit
      params <- extract_params(fit)
      d.all$doy10[which(d.all$id == rep & d.all$type == "A")] <- logistic_quantile(params, 0.1)
      d.all$doy50[which(d.all$id == rep & d.all$type == "A")] <- logistic_quantile(params, 0.5)
      d.all$doy90[which(d.all$id == rep & d.all$type == "A")] <- logistic_quantile(params, 0.9)
    } else {
      cat("Fit not found for id:", rep, "in fits.list.A\n")
    }
  }
  else if (d.all$type[i] == "CCI") {
    fit_index <- grep(rep, names(fits.list.C))
    if (length(fit_index) > 0) {
      fit <- fits.list.C[[fit_index[1]]]  # Use the first match
      params <- extract_params(fit)
      d.all$doy10[which(d.all$id == rep & d.all$type == "CCI")] <- logistic_quantile(params, 0.1)
      d.all$doy50[which(d.all$id == rep & d.all$type == "CCI")] <- logistic_quantile(params, 0.5)
      d.all$doy90[which(d.all$id == rep & d.all$type == "CCI")] <- logistic_quantile(params, 0.9)
      
    } else {
      cat("Fit not found for id:", rep, "in fits.list.C\n")
    }
  }
  else if (d.all$type[i] == "Percentage") {
    fit_index <- grep(rep, names(fits.list.P))
    if (length(fit_index) > 0) {
      fit <- fits.list.P[[fit_index[1]]]  # Use the first match
      params <- extract_params(fit)
      d.all$doy10[which(d.all$id == rep & d.all$type == "Percentage")] <- logistic_quantile(params, 0.1)
      d.all$doy50[which(d.all$id == rep & d.all$type == "Percentage")] <- logistic_quantile(params, 0.5)
      d.all$doy90[which(d.all$id == rep & d.all$type == "Percentage")] <- logistic_quantile(params, 0.9)
    } else {
      cat("Fit not found for id:", rep, "in fits.list.P\n")
    }
  } else {
    cat("ID:", rep, "NA for type in d.all" )
  }
}

#summary data (Q10, Q50, Q90, and b) for each treatment in each species 
#should I separate by each metrics? Maybe...
treatment_group <- split(d.all, d.all$species)
treatment_group <- lapply(treatment_group, function(sub_df) {
  treatment_group <- split(sub_df, sub_df$treatment)
  lapply(treatment_group, function(type_df){
    split(type_df, type_df$type) 
    })
  }) # split d.all by species and treatment under each species
standard_error <- function(x) {
  sd(x) / sqrt(length(x))
}
trmt_spec_summary <- data.frame(matrix(data = NA, nrow = 0, ncol = 5))
for (i in 1:length(names(treatment_group))){
  d.spec <- treatment_group[[i]]
  for (n in 1:length(names(d.spec))){
    d.trmt <- d.spec[[n]]
    for (o in 1:length(names(d.trmt))){
      data <- d.trmt[[o]]
      temp <- data.frame(matrix(data = NA, ncol = 0, nrow = 1))
      temp$species <- unique(data$species)
      temp$trmt <- unique(data$treatment)
      temp$b <- mean(data$b)
      temp$Q10 <- mean(data$doy10)
      temp$Q10se <- standard_error(data$doy10)
      temp$Q50 <- mean(data$doy50)
      temp$Q50se <- standard_error(data$doy50)
      temp$Q90 <- mean(data$doy90)
      temp$Q90se <- standard_error(data$doy90)
      temp$type <- unique(data$type)
      trmt_spec_summary <- rbind(trmt_spec_summary, temp)
    }
  }
}
# general fit for each species
spec_list <- split(d.cleaned, d.cleaned$species)
d.specwise <- data.frame(matrix(data = NA, ncol = 3, nrow = 0 ))
fits.list.spec <- list()
for (i in 1:length(unique(d.cleaned$species))){
  name <- unique(d.cleaned$species)[i]
  d.species <- subset(d.cleaned, d.cleaned$species == name)
  for (n in 1:length(unique(d.species$type))){
    metric <- unique(d.species$type)[n]
    d <- subset(d.species, d.species$type == metric)
    fit.dat <- data.frame(value = d$value, time = d$doy)
    fit <- tryCatch({
      nls(value ~ SSlogis(time, a,b,c), data = fit.dat)
    }, error = function (e){
      cat("Error in fitting for", name, metric, "\nError message:", e$message, "Skipping. \n")
      return(NULL)
    })
    if (!is.null(fit)) {
      b_value <- coef(fit)['b']
      fits.list.spec[[paste0(name, "_fit_", metric)]] <- fit
      temp <- data.frame(matrix(data = NA, ncol = 0, nrow = 1))
      temp$species <- name
      temp$b <- b_value
      temp$type <- metric
      d.specwise <- rbind(d.specwise, temp)
    }
  }
}

for (i in 1:nrow(d.specwise)){
  rep <- d.aspecwise$id[i]
  fit <- fits.list.spec[[i]]
  params <- extract_params(fit)
  d.specwise$quantile10[which(d.all$id == rep & d.all$type == "A")] <- logistic_quantile(params, 0.1)
  d.specwise$quantile50[which(d.all$id == rep & d.all$type == "A")] <- logistic_quantile(params, 0.5)
  d.specwise$quantile90[which(d.all$id == rep & d.all$type == "A")] <- logistic_quantile(params, 0.9)

}

#plotting one curve for each species
for (i in 1: length(species_list)){
  spec <- species_list[i]
  filenameA <- paste0("output/senescence_plots/", spec, "_A_plot.png")
  png(filenameA)
  plot(d.cleaned$doy[which(d.cleaned$species == spec & d.cleaned$type == "A")], 
       d.cleaned$value[which(d.cleaned$species == spec & d.cleaned$type == "A")], 
       xlab = "Time (doy)", ylab = "Value (A)", 
       pch = 16, col = "blue")
  fitnameA <- paste0(spec, "_fit_A")
  curve(predict(fits.list.spec[[fitnameA]], newdata = data.frame(time = x), se = FALSE), 
        add = TRUE, col = "blue4")
  title(spec)
  dev.off()
}
png("output/senescence_plots/A_plot.png")
plot(d.cleaned$doy[which(d.cleaned$species == "Prvi" & d.cleaned$type == "A")], 
     d.cleaned$value[which(d.cleaned$species == "Prvi" & d.cleaned$type == "A")], 
     xlab = "Time (doy)", ylab = "Value (A)", 
     pch = 16, col = "blue")
plot(d.cleaned$doy[which(d.cleaned$species == "Prvi" & d.cleaned$type == "Percentage")],
     d.cleaned$value[which(d.cleaned$species == "Prvi" & d.cleaned$type == "Percentage")],
     xlab = "Time (doy)", ylab = "Value (A)",
     pch = 16, col = "red")
curve(predict(fits.list.spec[["Prvi_fit_A"]], newdata = data.frame(time = x), se = FALSE), 
      add = TRUE, col = "blue4")
curve(predict(fits.list.spec[["Prvi_fit_Percentage"]], newdata = data.frame(time = x), se = FALSE),
      add = TRUE, col = "red4")
title("Prvi")
dev.off()