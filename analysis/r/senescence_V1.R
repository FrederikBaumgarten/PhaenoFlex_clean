#--------------------------------------------------------------------
# Senescence data 
# Author: Britany Wu
# Date: October 24th
#--------------------------------------------------------------------
# Housekeeping
rm(list = ls(all = TRUE))

path <- "~/Documents/ubc/year5/TemporalEcologyLab/Phaenoflex/senescence"
setwd(path)

library(ggplot2)
library(gridExtra)
library(readxl)
library(dplyr)
#Load----------------------------------------------------------------
raw_a.dat <- read_excel("data/senescence_full.xlsx")
meta.dat <- read_excel("data/senescence_5_Sept_FB.xlsx", sheet = "percentage")
rep.info <- meta.dat[, 1:4]
index.dat <- meta.dat[, grep("INDEX", colnames(meta.dat))]
percen.dat <- meta.dat[, grep("PERC", colnames(meta.dat))]
CCI.dat <- meta.dat[, grep("CCI", colnames(meta.dat))]
index.dat <- cbind(rep.info, index.dat)
percen.dat <- cbind(rep.info, percen.dat)
CCI.dat <- cbind(rep.info, CCI.dat)
prvi_Adat <- subset(raw_a.dat, spec == "Prvi")
prvi_Cdat <- subset(CCI.dat, spec == "Prvi")
prvi_Pdat <- subset(percen.dat, spec == "Prvi")
acma_Adat <- subset(raw_a.dat, spec == "Acma")
acma_Cdat <- subset(CCI.dat, spec == "Acma")
acma_Pdat <- subset(percen.dat, spec == "Acma")
bepa_Adat <- subset(raw_a.dat, spec == "Bepa")
bepa_Cdat <- subset(CCI.dat, spec == "Bepa")
bepa_Pdat <- subset(percen.dat, spec == "Bepa")
quma_Adat <- subset(raw_a.dat, spec == "Quma")
quma_Cdat <- subset(CCI.dat, spec == "Quma")
quma_Pdat <- subset(percen.dat, spec == "Quma")
doy <- colnames(prvi_Adat)
doy <- doy[7:19]

##cleaning ----------------------------------------------------------
#create a data frame with 3 columns, doy, A, and id 
###prvi--------------------------------------------------------------
praw_dat <- data.frame(matrix(data = NA, ncol = 3, nrow = 0))
for (i in 1:nrow(prvi_Adat)){
  A <- t(prvi_Adat[i, c(7:19)]) #transpose
  loop <- data.frame(matrix(data = NA, ncol = 0, nrow = length(doy)))
  loop$id <- prvi_Adat$tree_ID[i]
  loop$value <- A[1:13]
  loop$doy <- doy
  loop$doy <- as.numeric(doy)
  loop$type <- "A"
  praw_dat <- rbind(praw_dat, loop)
}
for (i in 1:nrow(prvi_Cdat)){
  colnames <- colnames(prvi_Cdat)
  colnames <- sub("_CCI", "", colnames[grepl("CCI", colnames)])
  C <- t(prvi_Cdat[i, c(5:13)])
  loop <- data.frame(matrix(data = NA, ncol = 0, nrow = length(colnames)))
  loop$id <- prvi_Cdat$ID[i]
  loop$value <- C[1:9]
  loop$doy <- colnames
  loop$doy <- as.numeric(colnames)
  loop$type <- "CCI"
  praw_dat <- rbind(praw_dat,loop)
}
for (i in 1:nrow(prvi_Pdat)){
  colnames <- colnames(prvi_Pdat)
  colnames <- sub("_PERC", "", colnames[grepl("_PERC", colnames)])
  P <- t(prvi_Pdat[i, c(5:13)])
  loop <- data.frame(matrix(data = NA, ncol = 0, nrow = length(colnames)))
  loop$id <- prvi_Pdat$ID[i]
  loop$value <- P[1:9]
  loop$value <- as.numeric(loop$value)/10
  loop$doy <- colnames
  loop$doy <- as.numeric(colnames)
  loop$type <- "P"
  praw_dat <- rbind(praw_dat,loop)
}
praw_dat$value <- as.numeric(praw_dat$value)
# praw_Adat <- na.omit(praw_dat)
###acma--------------------------------------------------------------
araw_dat <- data.frame(matrix(data = NA, ncol = 3, nrow = 0))
for (i in 1:nrow(acma_Adat)){
  A <- t(acma_Adat[i, c(7:19)]) #transpose
  loop <- data.frame(matrix(data = NA, ncol = 0, nrow = length(doy)))
  loop$id <- acma_Adat$tree_ID[i]
  loop$value <- A[1:13]
  loop$doy <- doy
  loop$doy <- as.numeric(doy)
  loop$type <- "A"
  araw_dat <- rbind(araw_dat, loop)
}
for (i in 1:nrow(acma_Cdat)){
  colnames <- colnames(acma_Cdat)
  colnames <- sub("_CCI", "", colnames[grepl("CCI", colnames)])
  C <- t(acma_Cdat[i, c(5:13)])
  loop <- data.frame(matrix(data = NA, ncol = 0, nrow = length(colnames)))
  loop$id <- acma_Cdat$ID[i]
  loop$value <- C[1:9]
  loop$doy <- colnames
  loop$doy <- as.numeric(colnames)
  loop$type <- "CCI"
  araw_dat <- rbind(araw_dat,loop)
}
for (i in 1:nrow(acma_Pdat)){
  colnames <- colnames(acma_Pdat)
  colnames <- sub("_PERC", "", colnames[grepl("_PERC", colnames)])
  P <- t(acma_Pdat[i, c(5:13)])
  loop <- data.frame(matrix(data = NA, ncol = 0, nrow = length(colnames)))
  loop$id <- acma_Pdat$ID[i]
  loop$value <- P[1:9]
  loop$value <- as.numeric(loop$value)/10
  loop$doy <- colnames
  loop$doy <- as.numeric(colnames)
  loop$type <- "P"
  araw_dat <- rbind(araw_dat,loop)
}
araw_dat$value <- as.numeric(araw_dat$value)
###bepa--------------------------------------------------------------
braw_dat <- data.frame(matrix(data = NA, ncol = 3, nrow = 0))
for (i in 1:nrow(bepa_Adat)){
  A <- t(bepa_Adat[i, c(7:19)]) #transpose
  loop <- data.frame(matrix(data = NA, ncol = 0, nrow = length(doy)))
  loop$id <- bepa_Adat$tree_ID[i]
  loop$value <- A[1:13]
  loop$doy <- doy
  loop$doy <- as.numeric(doy)
  loop$type <- "A"
  braw_dat <- rbind(braw_dat, loop)
}
for (i in 1:nrow(bepa_Cdat)){
  colnames <- colnames(bepa_Cdat)
  colnames <- sub("_CCI", "", colnames[grepl("CCI", colnames)])
  C <- t(bepa_Cdat[i, c(5:13)])
  loop <- data.frame(matrix(data = NA, ncol = 0, nrow = length(colnames)))
  loop$id <- bepa_Cdat$ID[i]
  loop$value <- C[1:9]
  loop$doy <- colnames
  loop$doy <- as.numeric(colnames)
  loop$type <- "CCI"
  braw_dat <- rbind(braw_dat,loop)
}
for (i in 1:nrow(bepa_Pdat)){
  colnames <- colnames(bepa_Pdat)
  colnames <- sub("_PERC", "", colnames[grepl("_PERC", colnames)])
  P <- t(bepa_Pdat[i, c(5:13)])
  loop <- data.frame(matrix(data = NA, ncol = 0, nrow = length(colnames)))
  loop$id <- bepa_Pdat$ID[i]
  loop$value <- P[1:9]
  loop$value <- as.numeric(loop$value)/10
  loop$doy <- colnames
  loop$doy <- as.numeric(colnames)
  loop$type <- "P"
  braw_dat <- rbind(braw_dat,loop)
}
braw_dat$value <- as.numeric(braw_dat$value)
###quma--------------------------------------------------------------
qraw_dat <- data.frame(matrix(data = NA, ncol = 3, nrow = 0))
for (i in 1:nrow(quma_Adat)){
  A <- t(quma_Adat[i, c(7:19)]) #transpose
  loop <- data.frame(matrix(data = NA, ncol = 0, nrow = length(doy)))
  loop$id <- quma_Adat$tree_ID[i]
  loop$value <- A[1:13]
  loop$doy <- doy
  loop$doy <- as.numeric(doy)
  loop$type <- "A"
  qraw_dat <- rbind(qraw_dat, loop)
}
for (i in 1:nrow(quma_Cdat)){
  colnames <- colnames(quma_Cdat)
  colnames <- sub("_CCI", "", colnames[grepl("CCI", colnames)])
  C <- t(quma_Cdat[i, c(5:13)])
  loop <- data.frame(matrix(data = NA, ncol = 0, nrow = length(colnames)))
  loop$id <- quma_Cdat$ID[i]
  loop$value <- C[1:9]
  loop$doy <- colnames
  loop$doy <- as.numeric(colnames)
  loop$type <- "CCI"
  qraw_dat <- rbind(qraw_dat,loop)
}
for (i in 1:nrow(quma_Pdat)){
  colnames <- colnames(quma_Pdat)
  colnames <- sub("_PERC", "", colnames[grepl("_PERC", colnames)])
  P <- t(quma_Pdat[i, c(5:13)])
  loop <- data.frame(matrix(data = NA, ncol = 0, nrow = length(colnames)))
  loop$id <- quma_Pdat$ID[i]
  loop$value <- P[1:9]
  loop$value <- as.numeric(loop$value)/10
  loop$doy <- colnames
  loop$doy <- as.numeric(colnames)
  loop$type <- "P"
  qraw_dat <- rbind(qraw_dat,loop)
}
qraw_dat$value <- as.numeric(qraw_dat$value)
#preliminary plots--------------------------------------------------
##prvi--------------------------------------------------------------
pcon_plot <- ggplot(data = praw_dat[grepl("Con", praw_dat$id), ])+
  geom_point(aes(x = doy, y = value, group = type, color = type), size = 1.5) +
  geom_line(aes(x = doy, y = value, group = type, color = type)) +
  scale_x_continuous(breaks = seq(216,304, by = 10)) +
  scale_y_continuous(breaks = seq(0,100, by = 10)) +
  facet_wrap(vars(id)) +
  labs( x = "Day of Year", y = "Assimilation Rate (A) [µmol m-2 s-2]")
pcon_plot
ggsave("res/pcon_plot.png", pcon_plot, dpi = 300)
pgs_plot <- ggplot(data = praw_dat[grepl("GS_ext", praw_dat$id), ])+
  geom_point(aes(x = doy, y = value, group = type, color = type), size = 1.5) +
  geom_line(aes(x = doy, y = value, group = type, color = type)) +
  scale_x_continuous(breaks = seq(216,304, by = 10)) +
  scale_y_continuous(breaks = seq(0,100, by = 10)) +
  facet_wrap(vars(id)) +
  labs( x = "Day of Year", y = "Assimilation Rate (A) [µmol m-2 s-2]")
pgs_plot

##acma--------------------------------------------------------------
acon_plot <- ggplot(data = araw_dat[grepl("Con", araw_dat$id), ])+
  geom_point(aes(x = doy, y = value, group = type, color = type), size = 1.5) +
  geom_line(aes(x = doy, y = value, group = type, color = type)) +
  scale_x_continuous(breaks = seq(216,304, by = 10)) +
  scale_y_continuous(breaks = seq(0,100, by = 10)) +
  facet_wrap(vars(id)) +
  labs( x = "Day of Year", y = "Assimilation Rate (A) [µmol m-2 s-2]")
acon_plot
ggsave("res/acon_plot.png", acon_plot, dpi = 300)
##bepa--------------------------------------------------------------
bcon_plot <- ggplot(data = braw_dat[grepl("Con", braw_dat$id), ])+
  geom_point(aes(x = doy, y = value, group = type, color = type), size = 1.5) +
  geom_line(aes(x = doy, y = value, group = type, color = type)) +
  scale_x_continuous(breaks = seq(216,304, by = 10)) +
  scale_y_continuous(breaks = seq(0,100, by = 10)) +
  facet_wrap(vars(id)) +
  labs( x = "Day of Year", y = "Assimilation Rate (A) [µmol m-2 s-2]")
bcon_plot
ggsave("res/bcon_plot.png", bcon_plot, dpi = 300)
##quma--------------------------------------------------------------
qcon_plot <- ggplot(data = qraw_dat[grepl("Con", qraw_dat$id), ])+
  geom_point(aes(x = doy, y = value, group = type, color = type), size = 1.5) +
  geom_line(aes(x = doy, y = value, group = type, color = type)) +
  scale_x_continuous(breaks = seq(216,304, by = 10)) +
  scale_y_continuous(breaks = seq(0,100, by = 10)) +
  facet_wrap(vars(id)) +
  labs( x = "Day of Year", y = "Assimilation Rate (A) [µmol m-2 s-2]")
qcon_plot
ggsave("res/qcon_plot.png", qcon_plot, dpi = 300)


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
png("res/simulated.png")
plot(t, y, type = "l", xlab = "Day of the year", ylab = "A",
     main = "Senescence process of deciduous trees in autumn")
dev.off()
#Fit data to SSlogis using simulated data 
dat<-data.frame(values= y, time= t) #create data frame with simulated data
fit <- nls(values ~ SSlogis(time, a, b, c), data = dat)
summary(fit)

### Fit data to SSlogis----------------------------------------------
###Amax
##prvi
#con
p_adat <- subset(praw_dat, type == "A")
rep.id.p <- unique(p_adat$id)
con.id.p <- subset(rep.id.p, grepl("Con", rep.id.p))
b_prvi <- data.frame(matrix(data = NA, ncol = 3, nrow = 0))
for (i in 1:length(con.id.p)){
  id.p <- con.id.p[i]
  data.p <- subset(p_adat, id == id.p)
  fit.dat <- data.frame(value = data.p$value, time = data.p$doy)
  fit <- tryCatch({
    nls(value ~ SSlogis(time, a, b, c), data = fit.dat)
  }, error = function(e) {
    cat("Error in fitting for id =", id.p, "Skipping.\n")
    return(NULL)  # Return NULL to indicate an error
  })
  
  # Check if the fitting was successful
  if (!is.null(fit)) {
    b_value <- coef(fit)['b']
    
    temp <- data.frame(matrix(data = NA, ncol = 0, nrow = 1))
    temp$trt <- "Con"
    temp$id <- id.p
    temp$b <- b_value
    b_prvi <- rbind(b_prvi, temp)
    
    plot.name <- paste0("res/", id.p, "plot.png")
    png(plot.name)
    plot(data.p$doy, data.p$value, xlab = "Time (doy)", ylab = "Value (A)",
         pch = 16, col = "blue")
    curve(predict(fit, newdata = data.frame(time = x), se = FALSE), 
          add = TRUE, col = "red")
    title(id.p)
    dev.off()
  }
}
#dro3
dro3.id.p <- subset(rep.id.p, grepl("Dro3", rep.id.p))
for (i in 1:length(dro3.id.p)){
  id.p <- dro3.id.p[i]
  data.p <- subset(p_adat, id == id.p)
  fit.dat <- data.frame(value = data.p$value, time = data.p$doy)
  fit <- tryCatch({
nls(value ~ SSlogis(time, a, b, c), data = fit.dat)
}, error = function(e) {
  cat("Error in fitting for id =", id.p, "Skipping.\n")
  return(NULL)  # Return NULL to indicate an error
})

# Check if the fitting was successful
if (!is.null(fit)) {
  b_value <- coef(fit)['b']
  
  temp <- data.frame(matrix(data = NA, ncol = 0, nrow = 1))
  temp$trt <- "Dro3"
  temp$id <- id.p
  temp$b <- b_value
  b_prvi <- rbind(b_prvi, temp)
  
  plot.name <- paste0("res/", id.p, "plot.png")
  png(plot.name)
  plot(data.p$doy, data.p$value, xlab = "Time (doy)", ylab = "Value (A)",
       pch = 16, col = "blue")
  curve(predict(fit, newdata = data.frame(time = x), se = FALSE), 
        add = TRUE, col = "red")
  title(id.p)
  dev.off()
}
}
#gs_ext
gs.id.p <- subset(rep.id.p, grepl("GS", rep.id.p))
for (i in 1:length(gs.id.p)){
  id.p <- gs.id.p[i]
  data.p <- subset(p_adat, id == id.p)
  fit.dat <- data.frame(value = data.p$value, time = data.p$doy)
  fit <- tryCatch({
    nls(value ~ SSlogis(time, a, b, c), data = fit.dat)
  }, error = function(e) {
    cat("Error in fitting for id =", id.p, "Skipping.\n")
    return(NULL)  # Return NULL to indicate an error
  })
  
  # Check if the fitting was successful
  if (!is.null(fit)) {
    b_value <- coef(fit)['b']
    
    temp <- data.frame(matrix(data = NA, ncol = 0, nrow = 1))
    temp$trt <- "GS_ext"
    temp$id <- id.p
    temp$b <- b_value
    b_prvi <- rbind(b_prvi, temp)
    
    plot.name <- paste0("res/", id.p, "plot.png")
    png(plot.name)
    plot(data.p$doy, data.p$value, xlab = "Time (doy)", ylab = "Value (A)",
         pch = 16, col = "blue")
    curve(predict(fit, newdata = data.frame(time = x), se = FALSE), 
          add = TRUE, col = "red")
    title(id.p)
    dev.off()
  }
}
png("res/prvi_box.png")
boxplot(b ~ trt, data = b_prvi, xlab = "treatment", ylab = "DOY for 50% decrease in A rate")
title("Predicted Prvi A50 DOY")
dev.off()
##acma 
#con
a_adat <- subset(araw_dat, type == "A") #only want A data 
rep.id.a <- unique(a_adat$id)
con.id.a <- subset(rep.id.a, grepl("Con", rep.id.a))
b_acma <- data.frame(matrix(data = NA, ncol = 3, nrow = 0))
for (i in 1:length(con.id.a)){ 
  id.a <- con.id.a[i]
  data.a <- subset(a_adat, id == id.a)
  fit.dat <- data.frame(value = data.a$value, time = data.a$doy)
  fit <- tryCatch({
    nls(value ~ SSlogis(time, a, b, c), data = fit.dat)
  }, error = function(e) {
    cat("Error in fitting for id =", id.a, "Skipping.\n")
    return(NULL)  # Return NULL to indicate an error
  })
  
  # Check if the fitting was successful
  if (!is.null(fit)) {
    b_value <- coef(fit)['b']
    
    temp <- data.frame(matrix(data = NA, ncol = 0, nrow = 1))
    temp$trt <- "Con"
    temp$id <- id.a
    temp$b <- b_value
    b_acma <- rbind(b_acma, temp)
    
    plot.name <- paste0("res/", id.a, "plot.png")
    png(plot.name)
    plot(data.a$doy, data.a$value, xlab = "Time (doy)", ylab = "Value (A)",
         pch = 16, col = "blue")
    curve(predict(fit, newdata = data.frame(time = x), se = FALSE), 
          add = TRUE, col = "red")
    title(id.a)
    dev.off()
  }
}
#dro3
dro3.id.a <- subset(rep.id.a, grepl("Dro3", rep.id.a))
for (i in 1:length(dro3.id.a)){
  id.a <- dro3.id.a[i]
  data.a <- subset(a_adat, id == id.a)
  fit.dat <- data.frame(value = data.a$value, time = data.a$doy)
  fit <- tryCatch({
    nls(value ~ SSlogis(time, a, b, c), data = fit.dat)
  }, error = function(e) {
    cat("Error in fitting for id =", id.a, "Skipping.\n")
    return(NULL)  # Return NULL to indicate an error
  })
  
  # Check if the fitting was successful
  if (!is.null(fit)) {
    b_value <- coef(fit)['b']
    
    temp <- data.frame(matrix(data = NA, ncol = 0, nrow = 1))
    temp$trt <- "Dro3"
    temp$id <- id.a
    temp$b <- b_value
    b_acma <- rbind(b_acma, temp)
    
    plot.name <- paste0("res/", id.a, "plot.png")
    png(plot.name)
    plot(data.a$doy, data.a$value, xlab = "Time (doy)", ylab = "Value (A)",
         pch = 16, col = "blue")
    curve(predict(fit, newdata = data.frame(time = x), se = FALSE), 
          add = TRUE, col = "red")
    title(id.a)
    dev.off()
  }
}
#gs_ext
gs.id.a <- subset(rep.id.a, grepl("GS", rep.id.a))
for (i in 1:length(gs.id.a)){
  id.a <- gs.id.a[i]
  data.a <- subset(a_adat, id == id.a)
  fit.dat <- data.frame(value = data.a$value, time = data.a$doy)
  fit <- tryCatch({
    nls(value ~ SSlogis(time, a, b, c), data = fit.dat)
  }, error = function(e) {
    cat("Error in fitting for id =", id.a, "Skipping.\n")
    return(NULL)  # Return NULL to indicate an error
  })
  
  # Check if the fitting was successful
  if (!is.null(fit)) {
    b_value <- coef(fit)['b']
    
    temp <- data.frame(matrix(data = NA, ncol = 0, nrow = 1))
    temp$trt <- "GSn"
    temp$id <- id.a
    temp$b <- b_value
    b_acma <- rbind(b_acma, temp)
    
    plot.name <- paste0("res/", id.a, "plot.png")
    png(plot.name)
    plot(data.a$doy, data.a$value, xlab = "Time (doy)", ylab = "Value (A)",
         pch = 16, col = "blue")
    curve(predict(fit, newdata = data.frame(time = x), se = FALSE), 
          add = TRUE, col = "red")
    title(id.a)
    dev.off()
  }
}
png("res/acma_box.png")
boxplot(b ~ trt, data = b_acma, xlab = "treatment", ylab = "DOY for 50% decrease in A rate")
title("Predicted Acma A50 DOY")
dev.off()
##Bepa
#con
b_adat <- subset(braw_dat, type == "A")
rep.id.b <- unique(b_adat$id)
con.id.b <- subset(rep.id.b, grepl("Con", rep.id.b))
b_bepa <- data.frame(matrix(data = NA, ncol = 3, nrow = 0))
for (i in 1:length(con.id.b)){
  id.b <- con.id.b[i]
  data.b <- subset(b_adat, id == id.b)
  fit.dat <- data.frame(value = data.b$value, time = data.b$doy)
  fit <- tryCatch({
    nls(value ~ SSlogis(time, a, b, c), data = fit.dat)
  }, error = function(e) {
    cat("Error in fitting for id =", id.b, "Skipping.\n")
    return(NULL)  # Return NULL to indicate an error
  })
  
  # Check if the fitting was successful
  if (!is.null(fit)) {
    b_value <- coef(fit)['b']
    
    temp <- data.frame(matrix(data = NA, ncol = 0, nrow = 1))
    temp$trt <- "Con"
    temp$id <- id.b
    temp$b <- b_value
    b_bepa <- rbind(b_bepa, temp)
    
    plot.name <- paste0("res/", id.b, "plot.png")
    png(plot.name)
    plot(data.b$doy, data.b$value, xlab = "Time (doy)", ylab = "Value (A)",
         pch = 16, col = "blue")
    curve(predict(fit, newdata = data.frame(time = x), se = FALSE), 
          add = TRUE, col = "red")
    title(id.b)
    dev.off()
  }
}
#dro3
dro3.id.b <- subset(rep.id.b, grepl("Dro3", rep.id.b))
for (i in 1:length(dro3.id.b)){
  id.b <- dro3.id.b[i]
  data.b <- subset(b_adat, id == id.b)
  fit.dat <- data.frame(value = data.b$value, time = data.b$doy)
  fit <- tryCatch({
    nls(value ~ SSlogis(time, a, b, c), data = fit.dat)
  }, error = function(e) {
    cat("Error in fitting for id =", id.b, "Skipping.\n")
    return(NULL)  # Return NULL to indicate an error
  })
  
  # Check if the fitting was successful
  if (!is.null(fit)) {
    b_value <- coef(fit)['b']
    
    temp <- data.frame(matrix(data = NA, ncol = 0, nrow = 1))
    temp$trt <- "Dro3"
    temp$id <- id.b
    temp$b <- b_value
    b_bepa <- rbind(b_bepa, temp)
    
    plot.name <- paste0("res/", id.b, "plot.png")
    png(plot.name)
    plot(data.b$doy, data.b$value, xlab = "Time (doy)", ylab = "Value (A)",
         pch = 16, col = "blue")
    curve(predict(fit, newdata = data.frame(time = x), se = FALSE), 
          add = TRUE, col = "red")
    title(id.b)
    dev.off()
  }
}
#gs_ext
gs.id.b <- subset(rep.id.b, grepl("GS", rep.id.b))
for (i in 1:length(gs.id.b)){
  id.b <- gs.id.b[i]
  data.b <- subset(b_adat, id == id.b)
  fit.dat <- data.frame(value = data.b$value, time = data.b$doy)
  fit <- tryCatch({ # skip the rep that regression cant fit
    nls(value ~ SSlogis(time, a, b, c), data = fit.dat)
  }, error = function(e) {
    cat("Error in fitting for id =", id.b, "Skipping.\n")
    return(NULL)  # Return NULL to indicate an error
  })
  
  # Check if the fitting was successful
  if (!is.null(fit)) {
    b_value <- coef(fit)['b']
    
    temp <- data.frame(matrix(data = NA, ncol = 0, nrow = 1))
    temp$trt <- "GS_ext"
    temp$id <- id.b
    temp$b <- b_value
    b_bepa <- rbind(b_bepa, temp)
    
    plot.name <- paste0("res/", id.b, "plot.png")
    png(plot.name)
    plot(data.b$doy, data.b$value, xlab = "Time (doy)", ylab = "Value (A)",
         pch = 16, col = "blue")
    curve(predict(fit, newdata = data.frame(time = x), se = FALSE), 
          add = TRUE, col = "red")
    title(id.b)
    dev.off()
  }
}
png("res/bepa_box.png")
boxplot(b ~ trt, data = b_bepa, xlab = "treatment", ylab = "DOY for 50% decrease in A rate")
title("Predicted Bepa A50 DOY")
dev.off()
##Quma
#con
q_adat <- subset(qraw_dat, type == "A")
rep.id.q <- unique(q_adat$id)
con.id.q <- subset(rep.id.q, grepl("Con", rep.id.q))
b_quma <- data.frame(matrix(data = NA, ncol = 3, nrow = 0))
for (i in 1:length(con.id.q)){
  id.q <- con.id.q[i]
  data.q <- subset(q_adat, id == id.q)
  fit.dat <- data.frame(value = data.q$value, time = data.q$doy)
  fit <- tryCatch({
    nls(value ~ SSlogis(time, a, b, c), data = fit.dat)
  }, error = function(e) {
    cat("Error in fitting for id =", id.q, "Skipping.\n")
    return(NULL)  # Return NULL to indicate an error
  })
  
  # Check if the fitting was successful
  if (!is.null(fit)) {
    q_value <- coef(fit)['b']
    
    temp <- data.frame(matrix(data = NA, ncol = 0, nrow = 1))
    temp$trt <- "Con"
    temp$id <- id.q
    temp$b <- q_value
    b_quma <- rbind(b_quma, temp)
    
    plot.name <- paste0("res/", id.q, "plot.png")
    png(plot.name)
    plot(data.q$doy, data.q$value, xlab = "Time (doy)", ylab = "Value (A)",
         pch = 16, col = "blue")
    curve(predict(fit, newdata = data.frame(time = x), se = FALSE), 
          add = TRUE, col = "red")
    title(id.q)
    dev.off()
  }
}
#dro3
dro3.id.q <- subset(rep.id.q, grepl("Dro3", rep.id.q))
for (i in 1:length(dro3.id.q)){
  id.q <- dro3.id.q[i]
  data.q <- subset(q_adat, id == id.q)
  fit.dat <- data.frame(value = data.q$value, time = data.q$doy)
  fit <- tryCatch({
    nls(value ~ SSlogis(time, a, b, c), data = fit.dat)
  }, error = function(e) {
    cat("Error in fitting for id =", id.q, "Skipping.\n")
    return(NULL)  # Return NULL to indicate an error
  })
  
  # Check if the fitting was successful
  if (!is.null(fit)) {
    q_value <- coef(fit)['b']
    
    temp <- data.frame(matrix(data = NA, ncol = 0, nrow = 1))
    temp$trt <- "Dro3"
    temp$id <- id.q
    temp$b <- q_value
    b_quma <- rbind(b_quma, temp)
    
    plot.name <- paste0("res/", id.q, "plot.png")
    png(plot.name)
    plot(data.q$doy, data.q$value, xlab = "Time (doy)", ylab = "Value (A)",
         pch = 16, col = "blue")
    curve(predict(fit, newdata = data.frame(time = x), se = FALSE), 
          add = TRUE, col = "red")
    title(id.q)
    dev.off()
  }
}
#gs_ext
gs.id.q <- subset(rep.id.q, grepl("GS", rep.id.q))
for (i in 1:length(gs.id.q)){
  id.q <- gs.id.q[i]
  data.q <- subset(q_adat, id == id.q)
  fit.dat <- data.frame(value = data.q$value, time = data.q$doy)
  fit <- tryCatch({
    nls(value ~ SSlogis(time, a, b, c), data = fit.dat)
  }, error = function(e) {
    cat("Error in fitting for id =", id.q, "Skipping.\n")
    return(NULL)  # Return NULL to indicate an error
  })
  
  # Check if the fitting was successful
  if (!is.null(fit)) {
    q_value <- coef(fit)['b']
    
    temp <- data.frame(matrix(data = NA, ncol = 0, nrow = 1))
    temp$trt <- "GS_ext"
    temp$id <- id.q
    temp$b <- q_value
    b_quma <- rbind(b_quma, temp)
    
    plot.name <- paste0("res/", id.q, "plot.png")
    png(plot.name)
    plot(data.q$doy, data.q$value, xlab = "Time (doy)", ylab = "Value (A)",
         pch = 16, col = "blue")
    curve(predict(fit, newdata = data.frame(time = x), se = FALSE), 
          add = TRUE, col = "red")
    title(id.q)
    dev.off()
  }
}
png("res/quma_box.png")
boxplot(b ~ trt, data = b_quma, xlab = "treatment", ylab = "DOY for 50% decrease in A rate")
title("Predicted Quma A50 DOY")
dev.off()
