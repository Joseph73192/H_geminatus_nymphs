#set working directory
library(readxl)
library(ggplot2)
library(tidyr)
library(vegan)
library(labdsv)
library(dplyr)

###########################################
####### East/West of river graph ##########
###########################################

#Read nymph data in
nymph <- read_xlsx("DataSheets_with_data.xlsx", sheet=3, col_names = T, trim_ws = T, na = "")
nymph <- subset(nymph, nymph$Long != 'na')

#This makes boxplots of HW vs E/W for each instar. You can only sort by sex for F0
t <- ggplot(nymph, aes(x=factor(EW.River, level=c('West', 'East')), y=HW.mm)) + geom_boxplot(aes(fill = EW.River)) +
  theme_bw() + scale_fill_brewer(palette="Set2")
t + xlab("") + ylab("Head Width (mm)") + geom_vline(xintercept=1.5, col = "blue", lwd=1) +
  theme(axis.text=element_text(size=17), axis.title=element_text(size=19), legend.position = "none") + facet_grid(~Instar)

#This will make a plot of HW over longitude, colored by sex and ordered by instar
n <- ggplot(nymph, aes(x=Long, y=HW.mm)) + theme_bw() + geom_point(aes(col=Sex)) +geom_vline(xintercept=-85, col = "blue", lwd=1)
n + xlab("Longitude") + ylab("Head Width (mm)") + scale_x_continuous(n.breaks = 10) +
  theme(axis.text=element_text(size=15), axis.title=element_text(size=17)) + facet_grid(Instar~.)

########F-0#####################
###############################

#For Wilcox Testing East vs. West F-0s
F0 <- subset(nymph, nymph$Instar == "F-0")
East_F0 <- subset(F0, EW.River == "East")
West_F0 <- subset(F0, EW.River =="West")
wilcox.test(East_F0$HW.mm, West_F0$HW.mm, alternative="less") 
# West > East but this may be due to more females in west than in east, etc. 

#For Wilcox Testing East Female vs. Male
East_F0_F <- subset(East_F0, Sex=="Female")
East_F0_M <- subset(East_F0, Sex=="Male")
wilcox.test(East_F0_F$HW.mm, East_F0_M$HW.mm, alternative="greater") 
# Female slightly larger, but statistically not powerful

#For Wilcox Testing West Female vs. Male
West_F0_F <- subset(West_F0, Sex=="Female")
West_F0_M <- subset(West_F0, Sex=="Male")
wilcox.test(West_F0_F$HW.mm, West_F0_M$HW.mm, alternative="greater")
# Female slightly larger

#For Wilcox Testing East Male vs. West Male
wilcox.test(East_F0_M$HW.mm, West_F0_M$HW.mm, alternative="less") 
#West males slightly larger

#For Wilcox Testing East Female vs. West Female
wilcox.test(East_F0_F$HW.mm, West_F0_F$HW.mm, alternative="less") 
# West slightly larger

F0_F <- subset(F0, F0$Sex == "Female")
F0_M <- subset(F0, F0$Sex == "Male")
wilcox.test(F0_F$HW.mm, F0_M$HW.mm, alternative="greater") 

########F-1#####################
###############################

#For Wilcox Testing East vs. West F-1s
F1 <- subset(nymph, nymph$Instar == "F-1")
East_F1 <- subset(F1, EW.River == "East")
West_F1 <- subset(F1, EW.River =="West")
wilcox.test(East_F1$HW.mm, West_F1$HW.mm, alternative="greater") 
# East > West (barely) 

#For Wilcox Testing East Female vs. Male
East_F1_F <- subset(East_F1, Sex=="Female")
East_F1_M <- subset(East_F1, Sex=="Male")
wilcox.test(East_F1_F$HW.mm, East_F1_M$HW.mm, alternative="greater") 
# Female slightly larger, but not significant

#For Wilcox Testing West Female vs. Male
West_F1_F <- subset(West_F1, Sex=="Female")
West_F1_M <- subset(West_F1, Sex=="Male")
wilcox.test(West_F1_F$HW.mm, West_F1_M$HW.mm, alternative="greater")
# Female slightly larger

#For Wilcox Testing East Male vs. West Male
wilcox.test(East_F1_M$HW.mm, West_F1_M$HW.mm, alternative="greater") 
#East males larger

#For Wilcox Testing East Female vs. West Female
wilcox.test(East_F1_F$HW.mm, West_F1_F$HW.mm, alternative="greater") 
# East slightly larger

##################F2+###############
####################################
F2 <- subset(nymph, nymph$Instar == "F-2+")
East_F2 <- subset(F2, EW.River == "East")
West_F2 <- subset(F2, EW.River =="West")
wilcox.test(East_F2$HW.mm, West_F2$HW.mm, alternative="less") 
#West > East

######################################
######Using GIS Attribute table#######
######################################

nymphs <- read.csv("nymph_join.csv", na = "", header = T) #This is spatial join from GIS of just nymphs
nymphs <- subset(nymphs, HW_mm != "NA") #Get rid of individuals w/out total length
nymphs$HUC_8 <- as.factor(nymphs$HUC_8)
#All used HUC-8 codes go into a string for later use
hucs <- c('3160205','3140103','3140104','3140102','3140203','3130004','3130011','3120003','3130003')
east_huc <- c('3130004','3130011','3120003','3130003')
west_huc <- c('3160205','3140103','3140104','3140102','3140203')

F0 <- subset(nymphs, nymphs$Instar == "F-0")
F1 <- subset(nymphs, nymphs$Instar == "F-1")
F2 <- subset(nymphs, nymphs$Instar == "F-2+")

############# F-0 ##############
t <- ggplot(F0, aes(x=factor(HUC_8, level=hucs), y=HW_mm)) + geom_boxplot(fill="gray") + theme_bw()

t + xlab("Watershed") + ylab("Head Width (mm)") + geom_vline(xintercept=4.5, col = "blue", lwd=1) + 
  geom_hline(yintercept=mean(F0$HW_mm), col = "black", lwd=0.5, linetype="dotdash")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + theme(legend.position="none")

############# F-1 ##############
t <- ggplot(F1, aes(x=factor(HUC_8, level=hucs), y=HW_mm)) + geom_boxplot(fill="gray") + theme_bw()

t + xlab("Watershed") + ylab("Head Width (mm)") + geom_vline(xintercept=4.5, col = "blue", lwd=1) + 
  geom_hline(yintercept=mean(F1$HW_mm), col = "black", lwd=0.5, linetype="dotdash")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + theme(legend.position="none")

############# F-2+ ##############
t <- ggplot(F2, aes(x=factor(HUC_8, level=hucs), y=HW_mm)) + geom_boxplot(fill="gray") + theme_bw()

t + xlab("Watershed") + ylab("Head Width (mm)") + geom_vline(xintercept=3.5, col = "blue", lwd=1) + 
  geom_hline(yintercept=mean(F2$HW_mm), col = "black", lwd=0.5, linetype="dotdash")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + theme(legend.position="none")

############### All instars ###################

t <- ggplot(nymphs, aes(x=factor(HUC_8, level=hucs), y=HW_mm)) + geom_boxplot(aes(fill = HUC_8)) + theme_bw() + 
  scale_fill_brewer(palette = "Set2")
# Change geom_boxplot fill to "Sex" for lengths by sex

t + xlab("Watershed") + ylab("Head Width (mm)") + geom_vline(xintercept=5.5, col = "blue", lwd=1) + 
  geom_hline(yintercept=mean(nymphs$HW_mm), col = "black", lwd=0.5, linetype="dotdash")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + theme(legend.position="none") + 
  facet_wrap(~Instar)
#Comment out legend position for boxplot showing sex

#Kruskal-Wallis test
kruskal.test(F0$HW_mm ~ F0$HUC_8) #not significant
kruskal.test(F1$HW_mm ~ F1$HUC_8) #not significant
kruskal.test(F2$HW_mm ~ F2$HUC_8) #not significant

#This section not necessary since none are significant as per K-W above
pairwise <- pairwise.wilcox.test(F0$HW_mm, F0$HUC_8, p.adjust.method = "BH")
pairwise <- as.data.frame(pairwise$p.value)
ifelse(pairwise > 0.05, NA, "check") #just for ease of use

