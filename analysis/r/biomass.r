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
#library()

#functions
truelength<-function(x){return(length(which(is.na(x)==FALSE)))}
#-------------------------------------
### Daten einlesen ###################################
##Monitoring data---------
dat<-read_xlsx("phaenoflex_data_12_merge_Jan_28_FB.xlsx", sheet = "dat_full", col_names = T, na = "NA")
dat<-readxl::read_excel("phaenoflex_data_12_merge_Jan_28_FB.xlsx", sheet = "dat_full", na = "NA") # nolint: line_length_linter.

summary(dat)
str(dat)
head(dat)

#Variablen bereinigen ---------------
str(dat)
dat$experiment = as.factor(dat$experiment)
dat$spec = as.factor(dat$spec)
dat$drought_timing = as.factor(dat$drought_timing)
dat$block<-as.factor(dat$block)

dat[dat$treatment=="drought_4", "treatment"] <- "defol3"
unique(dat$treatment)
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


# Calculate mean and standard error for every treatment and species
dat_summary <- dat %>%
  group_by(treatment, spec) %>%
  summarise(mean_biomass = mean(biomass_tot, na.rm = TRUE),
            se_biomass = sd(biomass_tot, na.rm = TRUE) / sqrt(n()),
            .groups = 'drop') # Drop the grouping

### Graphics -------------
## 1. Graph: show total biomass for all treatments

# Define custom labels for the facets
custom_labels <- c(Acma = "Acer macrophyllum", Bepa = "Betula papyrifera", Pico = "Pinus contorta", Potr = "Populus trichocarpa", Prvi = "Prunus virginiana", Quma = "Quercus garryana", Sese = "Sequoia sempervirens")

#Define order of treatment levels
dat$treatment <- factor(dat$treatment, levels = c("GS_extend", "GS_extend_heat", "control", "control_heat", "drought_1", "drought_2", "drought_3", "defol1", "defol2", "defol3"))

# Define your custom color palette
unique_treatments <- unique(dat_summary$treatment)
treatment_colors <- setNames(c('#241fb4', '#c03004', '#1c6cc8', '#ff460e', '#f0b400', '#af8403', '#634a00', '#b6fd60', '#7cb339', '#417009'), levels(dat$treatment))

# plot
biomass_tot <- ggplot(dat_summary, aes(x = treatment, y = mean_biomass, fill = treatment)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_errorbar(aes(ymin = mean_biomass - se_biomass, ymax = mean_biomass + se_biomass),
                                width = 0.2, position = position_dodge(0.9)) +
    facet_wrap(~ spec, labeller = labeller(spec = custom_labels), scales = "free_y", ncol = 1) +
    theme_minimal() +
    theme(panel.background = element_rect(fill = "white"),
                strip.text = element_text(face = "italic")) +
    labs(x = "Treatment", y = "Total Biomass (g)", title = "") +
    scale_fill_manual(values = treatment_colors)

print(biomass_tot)

# Export the plot as a PDF
ggsave(filename = "/Users/frederik/github/PhaenoFlex_clean/analysis/output/biomass_tot.pdf", plot = biomass_tot, device = "pdf", path = NULL, width = 4, height = 18)

######################################
######## 2. Graph: Same for shoot biomass
# Calculate mean and standard error for every treatment and species
str(dat)
dat_summary <- dat %>%
  group_by(treatment, spec) %>%
  summarise(mean_biomass = mean(biomass_new_shoot, na.rm = TRUE),
            se_biomass = sd(biomass_new_shoot, na.rm = TRUE) / sqrt(n()),
            .groups = 'drop') # Drop the grouping


# plot
biomass_shoot <- ggplot(dat_summary, aes(x = treatment, y = mean_biomass, fill = treatment)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_errorbar(aes(ymin = mean_biomass - se_biomass, ymax = mean_biomass + se_biomass),
                                width = 0.2, position = position_dodge(0.9)) +
    facet_wrap(~ spec, labeller = labeller(spec = custom_labels), scales = "free_y", ncol = 1) +
    theme_minimal() +
    theme(panel.background = element_rect(fill = "white"),
                strip.text = element_text(face = "italic")) +
    labs(x = "Treatment", y = "New Shoot Biomass (g)", title = "") +
    scale_fill_manual(values = treatment_colors)

print(biomass_shoot)

# Export the plot as a PDF
ggsave(filename = "/Users/frederik/github/PhaenoFlex_clean/analysis/output/biomass_new_shoot.pdf", plot = biomass_shoot, device = "pdf", path = NULL, width = 4, height = 18)


######################################
######## 3. Graph: Same for root biomass
# Calculate mean and standard error for every treatment and species
str(dat)
dat_summary <- dat %>%
  group_by(treatment, spec) %>%
  summarise(mean_biomass = mean(biomass_root, na.rm = TRUE),
            se_biomass = sd(biomass_root, na.rm = TRUE) / sqrt(n()),
            .groups = 'drop') # Drop the grouping


# plot
biomass_root <- ggplot(dat_summary, aes(x = treatment, y = mean_biomass, fill = treatment)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_errorbar(aes(ymin = mean_biomass - se_biomass, ymax = mean_biomass + se_biomass),
                                width = 0.2, position = position_dodge(0.9)) +
    facet_wrap(~ spec, labeller = labeller(spec = custom_labels), scales = "free_y", ncol = 1) +
    theme_minimal() +
    theme(panel.background = element_rect(fill = "white"),
                strip.text = element_text(face = "italic")) +
    labs(x = "Treatment", y = "Root Biomass (g)", title = "") +
    scale_fill_manual(values = treatment_colors)

print(biomass_root)

# Export the plot as a PDF
ggsave(filename = "/Users/frederik/github/PhaenoFlex_clean/analysis/output/biomass_root.pdf", plot = biomass_root, device = "pdf", path = NULL, width = 4, height = 18)


#-----------------------------------------------------
# Compare only treatments in June (all species that were exposed to heat as well)
######################################
######## 4. Graph: Same for root biomass
# Calculate mean and standard error for every treatment and species
str(dat)

# remove treatments levels "drought_1", "drought_3", "defol1", "defol3"
dat <- dat[!dat$treatment %in% c("drought_1", "drought_3", "defol1", "defol3"),]


unique(dat$treatment)
#Define order of treatment levels
dat$treatment <- factor(dat$treatment, levels = c("GS_extend", "GS_extend_heat", "control", "control_heat", "drought_1", "drought_2", "drought_3", "defol1", "defol2", "defol3"))

# Define your custom color palette
unique_treatments <- unique(dat_summary$treatment)
treatment_colors <- setNames(c('#241fb4', '#c03004', '#1c6cc8', '#ff460e', '#f0b400', '#af8403', '#634a00', '#b6fd60', '#7cb339', '#417009'), levels(dat$treatment))


dat_summary <- dat %>%
  group_by(treatment, spec) %>%
  summarise(mean_biomass = mean(biomass_tot, na.rm = TRUE),
            se_biomass = sd(biomass_tot, na.rm = TRUE) / sqrt(n()),
            .groups = 'drop') # Drop the grouping


# plot
biomass_tot <- ggplot(dat_summary, aes(x = treatment, y = mean_biomass, fill = treatment)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_errorbar(aes(ymin = mean_biomass - se_biomass, ymax = mean_biomass + se_biomass),
                                width = 0.2, position = position_dodge(0.9)) +
    facet_wrap(~ spec, labeller = labeller(spec = custom_labels), scales = "free_y", ncol = 1) +
    theme_minimal() +
    theme(panel.background = element_rect(fill = "white"),
                strip.text = element_text(face = "italic")) +
    labs(x = "Treatment", y = "Total Biomass (g)", title = "") +
    scale_fill_manual(values = treatment_colors)

print(biomass_tot)

# Export the plot as a PDF
ggsave(filename = "/Users/frederik/github/PhaenoFlex_clean/analysis/output/June_treats_biomass_total.pdf", plot = biomass_tot, device = "pdf", path = NULL, width = 4, height = 18)







############## Growth #####
#### ---- Correlations to choose best indicator----------
cor.test(dat$Biomass_shoot, dat$Inc_bio_tot)
cor.test(dat$Biomass_shoot, dat$dia_inc_tot)
cor.test(dat$Biomass_shoot, dat$height_inc_tot)
plot(dat$Biomass_shoot, dat$Inc_bio_tot)
plot(dat$Biomass_shoot, dat$dia_inc_tot)
plot(dat$Biomass_shoot, dat$height_inc_tot)

## Plot to show good correlation between calculated and measured biomass
    corelat<-ggplot(data=dat, aes(x=Biomass_shoot, y=Inc_bio_tot))+
    facet_wrap(~spec, nrow = 2, ncol=2, scales = "free", labeller = labeller(spec = spec.labs)) +
    geom_point(data=dat, aes(x=Biomass_shoot, y=Inc_bio_tot, col=frost))+
    scale_color_manual(name="Frost treatment", labels= c("LT100", "LT50", "control"), values = c("LT100"="lightblue", "LT50"="royalblue1", "no"="green"))+
    labs(y="Calculated Shoot-Biomass increment [g]", x="Measured Shoot-Biomass [g]") +
    theme_pubr(base_size = 10, margin = F, legend = "top") +
    geom_smooth(method="lm", se=FALSE, col="black") 
    # Facet wrap with free scales
    corelat + theme_classic() + theme(plot.title=element_text(face="italic")) +
    theme(strip.text.x = element_text(face = "italic"))+
    theme(strip.background = element_rect(color="white", fill="white", size=5, linetype="solid"))+
    theme(legend.position="bottom")

########Biomass----------
## Plot: calculated TOTAL biomass increment during 2 GS
# New facet label names for dose variable
spec.labs <- c("Carpinus betulus", "Prunus avium", "Fagus sylvatica", "Quercus robur")
names(spec.labs) <- c("Car", "Pru", "Fag", "Que")

gg<-ggplot(data=dat, aes(fill=frost, x=BB_order, y=Inc_bio_tot)) +
  facet_wrap(~spec, nrow = 2, ncol=2, scales = "free", labeller = labeller(spec = spec.labs)) +
  geom_boxplot(aes(fill=factor(frost))) +
  scale_fill_manual(name="Frost treatment", labels= c("LT100", "LT50", "control"),values = c("LT100"="lightblue", "LT50"="royalblue1", "no"="green"))+
  #coord_cartesian(xlim=c(1,5), ylim=c(-10, 600)) +
  labs(y="Biomass increment during two growing seasons [g]", x="Order of leaf-out") 
  #theme(legend.position="None")  # remove legend
  #scale_colour_brewer(palette = "Set1")  # change color palette
#scale_y_continuous(breaks=seq(0, 600, 100)) # Change breaks

gg + theme_classic() + theme(plot.title=element_text(face="italic")) +
  #gg + theme_bw() + labs(subtitle="BW Theme")
  theme(strip.text.x = element_text(face = "italic"))+
  theme(strip.background = element_rect(color="white", fill="white", size=5, linetype="solid"))+
  theme(legend.position="bottom")
  
  ### Plot: calculated biomass increment after FIRST GS
  # New facet label names for dose variable
  spec.labs <- c("Carpinus betulus", "Prunus avium", "Fagus sylvatica", "Quercus robur")
names(spec.labs) <- c("Car", "Pru", "Fag", "Que")

b_gs_1<-ggplot(data=dat, aes(fill=frost, x=BB_order, y=Inc_bio_1GS)) +
  facet_wrap(~spec, nrow = 4, ncol=1, scales = "free", labeller = labeller(spec = spec.labs)) +
  geom_boxplot(aes(fill=factor(frost))) +
  scale_fill_manual(name="Frost treatment", labels= c("LT100", "LT50", "control"),values = c("LT100"="lightblue", "LT50"="royalblue1", "no"="green"))+
  #coord_cartesian(xlim=c(1,5), ylim=c(-10, 600)) +
  labs(y="Biomass increment during first growing season [g]", x="Order of leaf-out") +
#theme(legend.position="None")  # remove legend
#scale_colour_brewer(palette = "Set1")  # change color palette
#scale_y_continuous(breaks=seq(0, 600, 100)) # Change breaks

  theme_classic() + theme(plot.title=element_text(face="italic")) +
  #gg + theme_bw() + labs(subtitle="BW Theme")
  theme(strip.text.x = element_text(face = "italic"))+
  theme(strip.background = element_rect(color="white", fill="white", size=5, linetype="solid"))+
  theme(legend.position="bottom")
names(dat)

### Plot: calculated biomass increment after SECOND GS
# New facet label names for dose variable
spec.labs <- c("Carpinus betulus", "Prunus avium", "Fagus sylvatica", "Quercus robur")
names(spec.labs) <- c("Car", "Pru", "Fag", "Que")

plot<-ggplot(data=dat, aes(fill=frost, x=treatment, y=biomass_tot)) +
  facet_wrap(~spec, nrow = 4, ncol=1, scales = "free", labeller = labeller(spec = spec.labs)) +
  geom_boxplot(aes(fill=factor(frost))) +
  scale_fill_manual(name="Frost treatment", labels= c("LT100", "LT50", "control"),values = c("LT100"="lightblue", "LT50"="royalblue1", "no"="green"))+
  #coord_cartesian(xlim=c(1,5), ylim=c(-10, 600)) +
  labs(y="Biomass increment during second growing season [g]", x="Order of leaf-out") +
#theme(legend.position="None")  # remove legend
#scale_colour_brewer(palette = "Set1")  # change color palette
#scale_y_continuous(breaks=seq(0, 600, 100)) # Change breaks

  theme_classic() + theme(plot.title=element_text(face="italic")) +
  #gg + theme_bw() + labs(subtitle="BW Theme")
  theme(strip.text.x = element_text(face = "italic"))+
  theme(strip.background = element_rect(color="white", fill="white", size=5, linetype="solid"))+
  theme(legend.position="bottom")

### Figure Biomass
figure <- ggarrange(b_gs_1, b_gs_2,
                    labels = c("A", "B"),
                    ncol = 2, nrow = 1)

b_gs_1 + b_gs_2
#########Height------------------
## Plot: TOTAL height increment during 2 GS
spec.labs <- c("Carpinus betulus", "Prunus avium", "Fagus sylvatica", "Quercus robur")
names(spec.labs) <- c("Car", "Pru", "Fag", "Que")

gg<-ggplot(data=dat, aes(fill=frost, x=BB_order, y=height_inc_tot)) +
  facet_wrap(~spec, nrow = 2, ncol=2, scales = "free", labeller = labeller(spec = spec.labs)) +
  geom_boxplot(aes(fill=factor(frost))) +
  scale_fill_manual(name="Frost treatment", labels= c("LT100", "LT50", "control"),values = c("LT100"="lightblue", "LT50"="royalblue1", "no"="green"))+
  labs(y="Height increment during two growing seasons [cm]", x="Order of leaf-out") 

gg + theme_classic() + theme(plot.title=element_text(face="italic")) +
  theme(strip.text.x = element_text(face = "italic"))+
  theme(strip.background = element_rect(color="white", fill="white", size=5, linetype="solid"))+
  theme(legend.position="bottom")

## Plot: height increment during first GS
spec.labs <- c("Carpinus betulus", "Prunus avium", "Fagus sylvatica", "Quercus robur")
names(spec.labs) <- c("Car", "Pru", "Fag", "Que")

gg<-ggplot(data=dat, aes(fill=frost, x=BB_order, y=height_inc_tot)) +
  facet_wrap(~spec, nrow = 2, ncol=2, scales = "free", labeller = labeller(spec = spec.labs)) +
  geom_boxplot(aes(fill=factor(frost))) +
  scale_fill_manual(name="Frost treatment", labels= c("LT100", "LT50", "control"),values = c("LT100"="lightblue", "LT50"="royalblue1", "no"="green"))+
  labs(y="Height increment during first growing seasons [cm]", x="Order of leaf-out") 

gg + theme_classic() + theme(plot.title=element_text(face="italic")) +
  theme(strip.text.x = element_text(face = "italic"))+
  theme(strip.background = element_rect(color="white", fill="white", size=5, linetype="solid"))+
  theme(legend.position="bottom")

## Plot: height increment during second GS
spec.labs <- c("Carpinus betulus", "Prunus avium", "Fagus sylvatica", "Quercus robur")
names(spec.labs) <- c("Car", "Pru", "Fag", "Que")

gg<-ggplot(data=dat, aes(fill=frost, x=BB_order, y=height_inc_tot)) +
  facet_wrap(~spec, nrow = 2, ncol=2, scales = "free", labeller = labeller(spec = spec.labs)) +
  geom_boxplot(aes(fill=factor(frost))) +
  scale_fill_manual(name="Frost treatment", labels= c("LT100", "LT50", "control"),values = c("LT100"="lightblue", "LT50"="royalblue1", "no"="green"))+
  labs(y="Height increment during second growing seasons [cm]", x="Order of leaf-out") 

gg + theme_classic() + theme(plot.title=element_text(face="italic")) +
  theme(strip.text.x = element_text(face = "italic"))+
  theme(strip.background = element_rect(color="white", fill="white", size=5, linetype="solid"))+
  theme(legend.position="bottom")

########----------------------------














































#Graph settings ----------
ymax<-550 #variabeln definieren
ymin<--50
xmin<-0 #as.numeric(temp[temp$YMD==20181001,"doy"][1])
xmax<-9.2 #as.numeric(temp[temp$YMD==20190501,"doy"])
par(tcl=0)#tcl gleich ticklength von der achse
par(mar=c(2,3.1,1,1))#martin definiert r??nder: unten links oben rechts
par(mgp=c(1.1,0.2,0))
par(cex.lab=1.0)
par(cex.axis=1.0)
Groesse1=1.2
Groesse2=0.8
barwidth<-1.0

x_con<-c(1,4.1,7.2)
x_LT<-c(2.1,5.2,8.3)
#m <- layout(matrix(c(1,2,3,4,5,6,7,8,9,10,11,12),6,2), widths=c(1,1), heights=c(0.2,1,1,1,1,0.3))
#layout.show(m)
#Plotting ------------
par(mar=c(0.1,0.3,0.1,0.1))#martin definiert r??nder: unten links oben rechts
plot.new()
bla<-"(a) First growing season"
mtext(bla, side=3,xpd=TRUE, bty="n", cex=Groesse2, line=-1.4,adj=0, at=0.1)
#Plotting Control ------------
par(mar=c(1,3.2,1,1))#martin definiert r??nder: unten links oben rechts

#rgb(t(col2rgb("green"))/255)
colfunc <- colorRampPalette(c("red","royalblue"))
colfunc(4)
color<-c("#FF0000", "#BF224A", "#00FF00", "#804595", "#4169E1")

#Plotting 1. Growing Season with calculated values ------------
## Prunus ===========================================
plot(NA,ylim=c(ymin,ymax),xlim=c(xmin,xmax), xaxt="n",yaxt="n",ylab=NA,xlab=NA,las=1,bty="l",xaxs="i",yaxs="i",xaxs="i",main=NA, cex=Groesse1)
axis(side=1,at=c(1.5, 4.6,7.7), labels=c("17.Mar","26.Mar","23.Apr"), hadj=0.5, cex.axis=Groesse1)#,labels=substr(as.character(seq(1950,2012,10)),3,4))
axis(side=2,at=seq(-50,550,50), labels=seq(-50,550,50),tcl=0.2,line=NA,las=1,cex.axis=Groesse1)
mtext(text="Biomass increment (g)", side=2, line=2, cex=Groesse2, at=-400)

textposx<-0
textposy<-300
summary(dat$Inc_bio_2GS)
text1<-c("Prunus avium")
text(textposx, textposy, text1,xpd=TRUE, bty="n", cex=Groesse1, font=3, pos=4)

# data selection species-------------
spec_1<-subset(dat, !is.na(diameter_oct_20) & spec=="Pru") #
    length(spec_1$diameter_mar_19)
    length(spec_1$height_mar_19)
    length(spec_1$diameter_jan_20)
    length(spec_1$height_jan_20)
    
#Increment after 1. GS
#Parameters for the allometric equation ln(x1) + x2*ln(RCD+RCD*H) Annigh??fer 2016 EJFR FAGUS
x1 <- 0.62342
x2 <- 0.87409

spec_1_inc <- (x1 * (spec_1$diameter_jan_20/10*spec_1$diameter_jan_20/10*spec_1$height_jan_2020)^x2) - 
  (x1*(spec_1$diameter_mar_19/10*spec_1$diameter_mar_19/10*spec_1$height_mar_19)^x2)




spec_inc_all<-subset(dat, !is.na(diameter_oct_20) & spec=="Pru")
spec_inc_all$diameter_mar_19
spec_inc_all$height_mar_19
names(spec_con)
#Parameters for the allometric equation ln(x1) + x2*ln(RCD+RCD*H) Annigh??fer 2016 EJFR FAGUS
x1 <- 0.34321 #x1 and x2 are species specific and can be found in the table 4 of the paper
x2 <- 0.91827

spec_inc_all$bio_increment_2y <- (x1 * (spec_inc_all$diameter_jan_20/10*spec_inc_all$diameter_jan_20/10*spec_inc_all$height_jan_2020)^x2) -  (x1*(spec_inc_all$diameter_mar_19/10*spec_inc_all$diameter_mar_19/10*spec_inc_all$height_mar_19)^x2)
spec_inc_all$bio_increment_2y <- (x1 * (spec_inc_all$diameter_oct_20/10*spec_inc_all$diameter_oct_20/10*spec_inc_all$height_oct_20)^x2) -  (x1*(spec_inc_all$diameter_mar_19/10*spec_inc_all$diameter_mar_19/10*spec_inc_all$height_mar_19)^x2)






# data selection Control-------------
spec_con<-subset(dat, (spec=="Pru") & frost=="no") #
spec_con$diameter_jan_20
spec_con$height_jan_2020

#Parameters for the allometric equation ln(x1) + x2*ln(RCD+RCD*H) Annigh??fer 2016 EJFR FAGUS
x1 <- 0.62342
x2 <- 0.87409
#AGB_2019=exp(log(b1_2019)+b2_2019*log((diameter_2019/10)*(diameter_2019/10)*height_2019)))
Growth.Fag$biomass <- (x1 * (Growth.Fag$Diameter2020/10*Growth.Fag$Diameter2020/10*Growth.Fag$Height2020)^x2) -  (x1*(Growth.Fag$Diameter2019/10*Growth.Fag$Diameter2019/10*Growth.Fag$Height2019)^x2)

#x1 and x2 are species specific and can be found in the table 4 of the paper

bio_mean<-tapply(spec_con$Biomass_shoot, spec_con$BB_order, function(x)mean(x, na.rm=TRUE))[c(1,3,5)]
bio_sd<-tapply(spec_con$Biomass_shoot, spec_con$BB_order, function(x)sd(x, na.rm=TRUE))[c(1,3,5)]
bio_length<-tapply(spec_con$Biomass_shoot, spec_con$BB_order, function(x)truelength(x))[c(1,3,5)]
bio_se<-bio_sd/sqrt(bio_length)

rect(x_con-0.5*barwidth,0,x_con+0.5*barwidth,bio_mean,col=0)
segments(x_con,bio_mean-bio_se,x_con,bio_mean+bio_se)

# data selection LT100  
spec_LT100<-subset(dat, (spec=="Pru") & frost=="LT100") #
bio_mean<-tapply(spec_LT100$Biomass_shoot, spec_LT100$BB_order, function(x)mean(x, na.rm=TRUE))[c(1,3,5)]
bio_sd<-tapply(spec_LT100$Biomass_shoot, spec_LT100$BB_order, function(x)sd(x, na.rm=TRUE))[c(1,3,5)]
bio_length<-tapply(spec_LT100$Biomass_shoot, spec_LT100$BB_order, function(x)truelength(x))[c(1,3,5)]
bio_se<-bio_sd/sqrt(bio_length)

rect(x_LT-0.5*barwidth,0,x_LT+0.5*barwidth,bio_mean,col="grey55")
segments(x_LT,bio_mean-bio_se,x_LT,bio_mean+bio_se)

## Carpinus ===========================================
ymax<-120 #variabeln definieren
plot(NA,ylim=c(0,ymax),xlim=c(xmin,xmax), xaxt="n",yaxt="n",ylab=NA,xlab=NA,las=1,bty="l",xaxs="i",yaxs="i",xaxs="i",main=NA, cex=Groesse1)
axis(side=1,at=c(1.5, 4.6,7.7), labels=c("24.Mar","14.Apr","29.Apr"), hadj=0.5, cex.axis=Groesse1)#,labels=substr(as.character(seq(1950,2012,10)),3,4))
axis(side=2,at=seq(0,120,20), labels=seq(0,120,20),tcl=0.2,line=NA,las=1,cex.axis=Groesse1)
#83 104  119
textposx<-0
textposy<-120
text1<-c("Carpinus betulus")
text(textposx, textposy, text1,xpd=TRUE, bty="n", cex=Groesse1, font=3, pos=4)


# data selection Control-------------
spec_con<-subset(dat, (spec=="Car") & frost=="no") #
bio_mean<-tapply(spec_con$Biomass_shoot, spec_con$BB_order, function(x)mean(x, na.rm=TRUE))[c(1,3,5)]
bio_sd<-tapply(spec_con$Biomass_shoot, spec_con$BB_order, function(x)sd(x, na.rm=TRUE))[c(1,3,5)]
bio_length<-tapply(spec_con$Biomass_shoot, spec_con$BB_order, function(x)truelength(x))[c(1,3,5)]
bio_se<-bio_sd/sqrt(bio_length)

rect(x_con-0.5*barwidth,0,x_con+0.5*barwidth,bio_mean,col=0)
segments(x_con,bio_mean-bio_se,x_con,bio_mean+bio_se)

# data selection LT100  
spec_LT100<-subset(dat, (spec=="Car") & frost=="LT100") #
bio_mean<-tapply(spec_LT100$Biomass_shoot, spec_LT100$BB_order, function(x)mean(x, na.rm=TRUE))[c(1,3,5)]
bio_sd<-tapply(spec_LT100$Biomass_shoot, spec_LT100$BB_order, function(x)sd(x, na.rm=TRUE))[c(1,3,5)]
bio_length<-tapply(spec_LT100$Biomass_shoot, spec_LT100$BB_order, function(x)truelength(x))[c(1,3,5)]
bio_se<-bio_sd/sqrt(bio_length)

rect(x_LT-0.5*barwidth,0,x_LT+0.5*barwidth,bio_mean,col="grey55")
segments(x_LT,bio_mean-bio_se,x_LT,bio_mean+bio_se)
## Quercus ===========================================
ymax<-120 #variabeln definieren
plot(NA,ylim=c(0,ymax),xlim=c(xmin,xmax), xaxt="n",yaxt="n",ylab=NA,xlab=NA,las=1,bty="l",xaxs="i",yaxs="i",xaxs="i",main=NA, cex=Groesse1)
axis(side=1,at=c(1.5, 4.6,7.7), labels=c("24.Mar","14.Apr","29.Apr"), hadj=0.5, cex.axis=Groesse1)#,labels=substr(as.character(seq(1950,2012,10)),3,4))
axis(side=2,at=seq(0,120,20), labels=seq(0,120,20),tcl=0.2,line=NA,las=1,cex.axis=Groesse1)
#83 104  119
textposx<-0
textposy<-120
text1<-c("Quercus robur")
text(textposx, textposy, text1,xpd=TRUE, bty="n", cex=Groesse1, font=3, pos=4)

# data selection Control-------------
spec_con<-subset(dat, (spec=="Que") & frost=="no") #
bio_mean<-tapply(spec_con$Biomass_shoot, spec_con$BB_order, function(x)mean(x, na.rm=TRUE))[c(1,3,5)]
bio_sd<-tapply(spec_con$Biomass_shoot, spec_con$BB_order, function(x)sd(x, na.rm=TRUE))[c(1,3,5)]
bio_length<-tapply(spec_con$Biomass_shoot, spec_con$BB_order, function(x)truelength(x))[c(1,3,5)]
bio_se<-bio_sd/sqrt(bio_length)

rect(x_con-0.5*barwidth,0,x_con+0.5*barwidth,bio_mean,col=0)
segments(x_con,bio_mean-bio_se,x_con,bio_mean+bio_se)

# data selection LT100  
spec_LT100<-subset(dat, (spec=="Que") & frost=="LT100") #
bio_mean<-tapply(spec_LT100$Biomass_shoot, spec_LT100$BB_order, function(x)mean(x, na.rm=TRUE))[c(1,3,5)]
bio_sd<-tapply(spec_LT100$Biomass_shoot, spec_LT100$BB_order, function(x)sd(x, na.rm=TRUE))[c(1,3,5)]
bio_length<-tapply(spec_LT100$Biomass_shoot, spec_LT100$BB_order, function(x)truelength(x))[c(1,3,5)]
bio_se<-bio_sd/sqrt(bio_length)

rect(x_LT-0.5*barwidth,0,x_LT+0.5*barwidth,bio_mean,col="grey55")
segments(x_LT,bio_mean-bio_se,x_LT,bio_mean+bio_se)

textposx<-x_LT[1]
textposy<-bio_mean[1]+15
text1<-c("LT100")
text(textposx, textposy, text1,xpd=TRUE, bty="n", cex=Groesse1, font=1, pos=4, srt=90)
textposx<-x_con[1]
textposy<-bio_mean[1]+25
text1<-c("Control")
text(textposx, textposy, text1,xpd=TRUE, bty="n", cex=Groesse1, font=1, pos=4, srt=90)

## Fagus ===========================================
ymax<-120 #variabeln definieren
plot(NA,ylim=c(0,ymax),xlim=c(xmin,xmax), xaxt="n",yaxt="n",ylab=NA,xlab=NA,las=1,bty="l",xaxs="i",yaxs="i",xaxs="i",main=NA, cex=Groesse1)
axis(side=1,at=c(1.5, 4.6,7.7), labels=c("24.Mar","14.Apr","29.Apr"), hadj=0.5, cex.axis=Groesse1)#,labels=substr(as.character(seq(1950,2012,10)),3,4))
axis(side=2,at=seq(0,120,20), labels=seq(0,120,20),tcl=0.2,line=NA,las=1,cex.axis=Groesse1)
#83 104  119
textposx<-0
textposy<-120
text1<-c("Fagus sylvatica")
text(textposx, textposy, text1,xpd=TRUE, bty="n", cex=Groesse1, font=3, pos=4)
textposx<-4.6
textposy<--13.5
text1<-c("Date of leaf-out")
text(textposx, textposy, text1,xpd=TRUE, bty="n", cex=Groesse1, font=1, pos=1)

# data selection Control-------------
spec_con<-subset(dat, (spec=="Fag") & frost=="no") #
bio_mean<-tapply(spec_con$Biomass_shoot, spec_con$BB_order, function(x)mean(x, na.rm=TRUE))[c(1,3,5)]
bio_sd<-tapply(spec_con$Biomass_shoot, spec_con$BB_order, function(x)sd(x, na.rm=TRUE))[c(1,3,5)]
bio_length<-tapply(spec_con$Biomass_shoot, spec_con$BB_order, function(x)truelength(x))[c(1,3,5)]
bio_se<-bio_sd/sqrt(bio_length)

rect(x_con-0.5*barwidth,0,x_con+0.5*barwidth,bio_mean,col=0)
segments(x_con,bio_mean-bio_se,x_con,bio_mean+bio_se)

# data selection LT100  
spec_LT100<-subset(dat, (spec=="Fag") & frost=="LT100") #
bio_mean<-tapply(spec_LT100$Biomass_shoot, spec_LT100$BB_order, function(x)mean(x, na.rm=TRUE))[c(1,3,5)]
bio_sd<-tapply(spec_LT100$Biomass_shoot, spec_LT100$BB_order, function(x)sd(x, na.rm=TRUE))[c(1,3,5)]
bio_length<-tapply(spec_LT100$Biomass_shoot, spec_LT100$BB_order, function(x)truelength(x))[c(1,3,5)]
bio_se<-bio_sd/sqrt(bio_length)

rect(x_LT-0.5*barwidth,0,x_LT+0.5*barwidth,bio_mean,col="grey55")
segments(x_LT,bio_mean-bio_se,x_LT,bio_mean+bio_se)

##-----------------------
par(mar=c(0.1,3.2,0.1,0.1))#martin definiert r??nder: unten links oben rechts
plot(NA,ylim=c(0,ymax),xlim=c(xmin,xmax), xaxt="n",yaxt="n",ylab=NA,xlab=NA,las=1,bty="n",xaxs="i",yaxs="i",xaxs="i",main=NA, cex=Groesse1)
textposx<-6.5
textposy<-50
text1<-c("Date of Leaf-out")
text(textposx, textposy, text1,xpd=TRUE, bty="n", cex=Groesse1, font=1, pos=2)
##-----------------------
par(mar=c(0.1,0.3,0.1,0.1))#martin definiert r??nder: unten links oben rechts
plot.new()
bla<-"(b) Root"
mtext(bla, side=3,xpd=TRUE, bty="n", cex=Groesse2, line=-1.4,adj=0, at=0.1)

##======================================
#############--------------------------
#Plotting 2. Growing season with measured data------------
par(mar=c(1,1.8,1,1))#martin definiert r??nder: unten links oben rechts

## Prunus ===========================================
ymax<-300
plot(NA,ylim=c(0,ymax),xlim=c(xmin,xmax), xaxt="n",yaxt="n",ylab=NA,xlab=NA,las=1,bty="l",xaxs="i",yaxs="i",xaxs="i",main=NA, cex=Groesse1)
axis(side=1,at=c(1.5, 4.6,7.7), labels=c("17.Mar","26.Mar","23.Apr"), hadj=0.5, cex.axis=Groesse1)#,labels=substr(as.character(seq(1950,2012,10)),3,4))
axis(side=2,at=seq(0,300,50), labels=seq(0,300,50),tcl=0.2,line=NA,las=1,cex.axis=Groesse1)
#mtext(text="Biomass (g)", side=2, line=2, cex=Groesse2, at=-40)

textposx<-0
textposy<-300
text1<-c("Prunus avium")
text(textposx, textposy, text1,xpd=TRUE, bty="n", cex=Groesse1, font=3, pos=4)


# data selection Control-------------
spec_con<-subset(dat, (spec=="Pru") & frost=="no") #
bio_mean<-tapply(spec_con$Biomass_root, spec_con$BB_order, function(x)mean(x, na.rm=TRUE))[c(1,3,5)]
bio_sd<-tapply(spec_con$Biomass_root, spec_con$BB_order, function(x)sd(x, na.rm=TRUE))[c(1,3,5)]
bio_length<-tapply(spec_con$Biomass_root, spec_con$BB_order, function(x)truelength(x))[c(1,3,5)]
bio_se<-bio_sd/sqrt(bio_length)

rect(x_con-0.5*barwidth,0,x_con+0.5*barwidth,bio_mean,col=0)
segments(x_con,bio_mean-bio_se,x_con,bio_mean+bio_se)

# data selection LT100  
spec_LT100<-subset(dat, (spec=="Pru") & frost=="LT100") #
bio_mean<-tapply(spec_LT100$Biomass_root, spec_LT100$BB_order, function(x)mean(x, na.rm=TRUE))[c(1,3,5)]
bio_sd<-tapply(spec_LT100$Biomass_root, spec_LT100$BB_order, function(x)sd(x, na.rm=TRUE))[c(1,3,5)]
bio_length<-tapply(spec_LT100$Biomass_root, spec_LT100$BB_order, function(x)truelength(x))[c(1,3,5)]
bio_se<-bio_sd/sqrt(bio_length)

rect(x_LT-0.5*barwidth,0,x_LT+0.5*barwidth,bio_mean,col="grey55")
segments(x_LT,bio_mean-bio_se,x_LT,bio_mean+bio_se)

## Carpinus ===========================================
ymax<-120 #variabeln definieren
plot(NA,ylim=c(0,ymax),xlim=c(xmin,xmax), xaxt="n",yaxt="n",ylab=NA,xlab=NA,las=1,bty="l",xaxs="i",yaxs="i",xaxs="i",main=NA, cex=Groesse1)
axis(side=1,at=c(1.5, 4.6,7.7), labels=c("24.Mar","14.Apr","29.Apr"), hadj=0.5, cex.axis=Groesse1)#,labels=substr(as.character(seq(1950,2012,10)),3,4))
axis(side=2,at=seq(0,120,20), labels=seq(0,120,20),tcl=0.2,line=NA,las=1,cex.axis=Groesse1)
#83 104  119
textposx<-0
textposy<-120
text1<-c("Carpinus betulus")
text(textposx, textposy, text1,xpd=TRUE, bty="n", cex=Groesse1, font=3, pos=4)


# data selection Control-------------
spec_con<-subset(dat, (spec=="Car") & frost=="no") #
bio_mean<-tapply(spec_con$Biomass_root, spec_con$BB_order, function(x)mean(x, na.rm=TRUE))[c(1,3,5)]
bio_sd<-tapply(spec_con$Biomass_root, spec_con$BB_order, function(x)sd(x, na.rm=TRUE))[c(1,3,5)]
bio_length<-tapply(spec_con$Biomass_root, spec_con$BB_order, function(x)truelength(x))[c(1,3,5)]
bio_se<-bio_sd/sqrt(bio_length)

rect(x_con-0.5*barwidth,0,x_con+0.5*barwidth,bio_mean,col=0)
segments(x_con,bio_mean-bio_se,x_con,bio_mean+bio_se)

# data selection LT100  
spec_LT100<-subset(dat, (spec=="Car") & frost=="LT100") #
bio_mean<-tapply(spec_LT100$Biomass_root, spec_LT100$BB_order, function(x)mean(x, na.rm=TRUE))[c(1,3,5)]
bio_sd<-tapply(spec_LT100$Biomass_root, spec_LT100$BB_order, function(x)sd(x, na.rm=TRUE))[c(1,3,5)]
bio_length<-tapply(spec_LT100$Biomass_root, spec_LT100$BB_order, function(x)truelength(x))[c(1,3,5)]
bio_se<-bio_sd/sqrt(bio_length)

rect(x_LT-0.5*barwidth,0,x_LT+0.5*barwidth,bio_mean,col="grey55")
segments(x_LT,bio_mean-bio_se,x_LT,bio_mean+bio_se)
## Quercus ===========================================
ymax<-120 #variabeln definieren
plot(NA,ylim=c(0,ymax),xlim=c(xmin,xmax), xaxt="n",yaxt="n",ylab=NA,xlab=NA,las=1,bty="l",xaxs="i",yaxs="i",xaxs="i",main=NA, cex=Groesse1)
axis(side=1,at=c(1.5, 4.6,7.7), labels=c("24.Mar","14.Apr","29.Apr"), hadj=0.5, cex.axis=Groesse1)#,labels=substr(as.character(seq(1950,2012,10)),3,4))
axis(side=2,at=seq(0,120,20), labels=seq(0,120,20),tcl=0.2,line=NA,las=1,cex.axis=Groesse1)
#83 104  119
textposx<-0
textposy<-120
text1<-c("Quercus robur")
text(textposx, textposy, text1,xpd=TRUE, bty="n", cex=Groesse1, font=3, pos=4)

# data selection Control-------------
spec_con<-subset(dat, (spec=="Que") & frost=="no") #
bio_mean<-tapply(spec_con$Biomass_root, spec_con$BB_order, function(x)mean(x, na.rm=TRUE))[c(1,3,5)]
bio_sd<-tapply(spec_con$Biomass_root, spec_con$BB_order, function(x)sd(x, na.rm=TRUE))[c(1,3,5)]
bio_length<-tapply(spec_con$Biomass_root, spec_con$BB_order, function(x)truelength(x))[c(1,3,5)]
bio_se<-bio_sd/sqrt(bio_length)

rect(x_con-0.5*barwidth,0,x_con+0.5*barwidth,bio_mean,col=0)
segments(x_con,bio_mean-bio_se,x_con,bio_mean+bio_se)

# data selection LT100  
spec_LT100<-subset(dat, (spec=="Que") & frost=="LT100") #
bio_mean<-tapply(spec_LT100$Biomass_root, spec_LT100$BB_order, function(x)mean(x, na.rm=TRUE))[c(1,3,5)]
bio_sd<-tapply(spec_LT100$Biomass_root, spec_LT100$BB_order, function(x)sd(x, na.rm=TRUE))[c(1,3,5)]
bio_length<-tapply(spec_LT100$Biomass_root, spec_LT100$BB_order, function(x)truelength(x))[c(1,3,5)]
bio_se<-bio_sd/sqrt(bio_length)

rect(x_LT-0.5*barwidth,0,x_LT+0.5*barwidth,bio_mean,col="grey55")
segments(x_LT,bio_mean-bio_se,x_LT,bio_mean+bio_se)


ymax<-120 #variabeln definieren
plot(NA,ylim=c(0,ymax),xlim=c(xmin,xmax), xaxt="n",yaxt="n",ylab=NA,xlab=NA,las=1,bty="l",xaxs="i",yaxs="i",xaxs="i",main=NA, cex=Groesse1)
axis(side=1,at=c(1.5, 4.6,7.7), labels=c("24.Mar","14.Apr","29.Apr"), hadj=0.5, cex.axis=Groesse1)#,labels=substr(as.character(seq(1950,2012,10)),3,4))
axis(side=2,at=seq(0,120,20), labels=seq(0,120,20),tcl=0.2,line=NA,las=1,cex.axis=Groesse1)
#83 104  119
textposx<-0
textposy<-120
text1<-c("Fagus sylvatica")
text(textposx, textposy, text1,xpd=TRUE, bty="n", cex=Groesse1, font=3, pos=4)
textposx<-4.6
textposy<--13.5
text1<-c("Date of leaf-out")
text(textposx, textposy, text1,xpd=TRUE, bty="n", cex=Groesse1, font=1, pos=1)

# data selection Control-------------
spec_con<-subset(dat, (spec=="Fag") & frost=="no") #
bio_mean<-tapply(spec_con$Biomass_root, spec_con$BB_order, function(x)mean(x, na.rm=TRUE))[c(1,3,5)]
bio_sd<-tapply(spec_con$Biomass_root, spec_con$BB_order, function(x)sd(x, na.rm=TRUE))[c(1,3,5)]
bio_length<-tapply(spec_con$Biomass_root, spec_con$BB_order, function(x)truelength(x))[c(1,3,5)]
bio_se<-bio_sd/sqrt(bio_length)

rect(x_con-0.5*barwidth,0,x_con+0.5*barwidth,bio_mean,col=0)
segments(x_con,bio_mean-bio_se,x_con,bio_mean+bio_se)

# data selection LT100  
spec_LT100<-subset(dat, (spec=="Fag") & frost=="LT100") #
bio_mean<-tapply(spec_LT100$Biomass_root, spec_LT100$BB_order, function(x)mean(x, na.rm=TRUE))[c(1,3,5)]
bio_sd<-tapply(spec_LT100$Biomass_root, spec_LT100$BB_order, function(x)sd(x, na.rm=TRUE))[c(1,3,5)]
bio_length<-tapply(spec_LT100$Biomass_root, spec_LT100$BB_order, function(x)truelength(x))[c(1,3,5)]
bio_se<-bio_sd/sqrt(bio_length)

rect(x_LT-0.5*barwidth,0,x_LT+0.5*barwidth,bio_mean,col="grey55")
segments(x_LT,bio_mean-bio_se,x_LT,bio_mean+bio_se)

##-----------------------
par(mar=c(0.1,3.2,0.1,0.1))#martin definiert r??nder: unten links oben rechts
plot(NA,ylim=c(0,ymax),xlim=c(xmin,xmax), xaxt="n",yaxt="n",ylab=NA,xlab=NA,las=1,bty="n",xaxs="i",yaxs="i",xaxs="i",main=NA, cex=Groesse1)
textposx<-6.5
textposy<-50
text1<-c("Date of Leaf-out")
text(textposx, textposy, text1,xpd=TRUE, bty="n", cex=Groesse1, font=1, pos=2)
