#Set working directory

####################################
## For nymph instar determination ####
####################################
library(readxl)
library(ggplot2)
library(ggforce)
nymph <- read_excel("DataSheets_with_data.xlsx", sheet = 3, na = "", col_names = T, trim_ws = T)
nymph <- subset(nymph, nymph$HW.mm != "na")

ggplot(nymph, aes(x=HW.mm, y=HwL.mm, group=Instar, col=Instar)) + theme_classic() + 
  geom_point() + xlab("Head Width (mm)") + ylab("Hindwing Sheath Length (mm)") + 
  geom_mark_ellipse(aes(color = as.factor(Instar)), expand = unit(1,"mm")) + 
  theme(legend.position=c(.85,.4))


####################################
## For nymph instar frequency ######
####################################
library(readxl)
nymph <- read_excel("DataSheets_with_data.xlsx", sheet = 3, na = "", col_names = T, trim_ws = T)
nymph <- subset(nymph, nymph$HW.mm != "na")

# Create a new column of months based on the date collected
nymph$month <- format(nymph$Date, "%b")
nymph$month <- factor(nymph$month)
nymph$month <- ordered(nymph$month, levels=c("Feb", "Mar", "Apr", "May", "Oct", "Dec"))

# This creates a single plot colored by instar, shaped by sex, sized by HW
library(RColorBrewer)
library(ggplot2)

n <- ggplot(nymph, aes(factor(month), group=Instar, fill=Instar)) + xlab("") + 
  ylab("Number of Nymphs Collected") + geom_histogram(stat="count", col = "black")

n + theme_classic() + stat_count(geom='text', color='white', size=4, 
                                 aes(label=..count.., group=Instar), 
                                 position=position_stack(vjust=0.5)) + 
  scale_fill_brewer(palette = "Set2") 

###########################################
##########Correlated metrics###############
###########################################
library(readxl)

################# Nymphs #################

nymph <- read_xlsx("DataSheets_with_data.xlsx", sheet=3, col_names = T, trim_ws = T, na = "n/a")
nymph <- subset(nymph, !is.na(nymph$Long))

library(corrplot)
nymph_num <- nymph[,10:15] #eliminates non-numeric values
colnames(nymph_num) <- c("HW", "HwL", "HwL:HW", "Epiproct L.", "Prementum W.", "Prementum L.")
corrplot(cor(nymph_num), method='number', type="lower", diag = F, tl.col = "black") #correlation plot of all instar measurements

############## F0 ##############
nymph_F0 <- subset(nymph, nymph$Instar == "F-0") #Subsetting only F-0s
nymph_F0 <- nymph_F0[,10:15] #eliminating non-numeric values
colnames(nymph_F0) <- c("HW", "HwL", "HwL:HW", "Epiproct L.", "Prementum W.", "Prementum L.")
corrplot(cor(nymph_F0), method='number', type = 'lower', diag = F, tl.col = "black") #correlation plot of F0 measurements

############## F1 ##############
nymph_F1 <- subset(nymph, nymph$Instar == "F-1") #Subsetting only F-1s
nymph_F1 <- nymph_F1[,10:15] #eliminating non-numeric values
colnames(nymph_F1) <- c("HW", "HwL", "HwL:HW", "Epiproct L.", "Prementum W.", "Prementum L.")
corrplot(cor(nymph_F1), method='number', type = 'lower', diag = F, tl.col = "black") #correlation plot of F1 measurements

############## F2+ ##############
nymph_F2 <- subset(nymph, nymph$Instar == "F-2+") #Subsetting only F-2+s
nymph_F2 <- nymph_F2[,10:15] #eliminating non-numeric values
colnames(nymph_F2) <- c("HW", "HwL", "HwL:HW", "Epiproct L.", "Prementum W.", "Prementum L.")
corrplot(cor(nymph_F2), method='number', type = 'lower', diag = F, tl.col = "black") #correlation plot of F2+ measurements

#########################################################
########## Combining nymph and stream data frames #######
#########################################################

nymphs <- read.csv("nymph_join.csv")
nymphs$Date <- strptime(nymphs$Date, format = "%m/%d/%Y")
#Subset data only for nymphs associated with habitat variables
nymphs <- subset(nymphs, nymphs$Date > as.POSIXct('2020-01-01 00:00', tz = "America/New_York"))

Stream <- read_xlsx("DataSheets_with_data.xlsx", sheet=1, col_names = T, trim_ws = T, na = "n/a")
#Subset data only for sites associated with habitat variables
Stream <- subset(Stream, Stream$Date > as.POSIXct('2020-01-01 00:00',tz = ""))
Stream <- subset(Stream, Stream$Unique.site != "JMVthw")
Stream <- subset(Stream, Stream$Unique.site != "JMVtiti")
Stream <- subset(Stream, Stream$Unique.site != "JMVhog")

#Combining stream and nymph dataframes to set up mixed methods
colnames(nymphs)[10] <- "Unique.site"
nymph.stream <- merge(nymphs, Stream, by = "Unique.site")
nymph.stream <- nymph.stream[,c(-2,-22,-24, -26)]
nymph.stream$Depth <- as.numeric(nymph.stream$Depth)

############## Stream ##############
nymph.stream <- nymph.stream[!duplicated(nymph.stream$Unique.site),]
nymph.stream <- nymph.stream[,-c(1:22)]
nymph.stream <- nymph.stream[,-c(11:13)]
nymph.stream <- na.omit(nymph.stream) #get rid of rows with NA's

colnames(nymph.stream) <- c("Canopy cov.", "Water Temp.", "Velocity", "pH", "DO", "TDS", "Sp. cond.", "Turbidity", "Width", "Depth")
corrplot(cor(nymph.stream), method='number', type = 'lower', diag = F, tl.col = "black", 
         number.cex = .8, tl.cex = .95) #correlation plot of stream measurements

###########################################
####### Outlier analysis ############
###########################################
library(EnvStats)

#For all collected data

rosnerTest(Stream$Turbidity, k=15) #4 outliers
rosnerTest(Stream$Sp.cond, k=10) #7 outliers
rosnerTest(Stream$TDS, k=15) #7 outliers (Not using TDS because correlation)
rosnerTest(Stream$Stream.width, k=15) #5 outliers

#For only streams where nymphs were collected
rosnerTest(nymph.stream$Sp.cond, k=10) #6 outliers
rosnerTest(nymph.stream$TDS, k=10) #6 outliers (Not using TDS because correlation)
rosnerTest(nymph.stream$Stream.width, k=10) #2 outliers
rosnerTest(nymph.stream$Depth, k=10) #5 outliers
rosnerTest(nymph.stream$Velocity, k=5) #all outliers? 

#########################################################
################## Mixed-effects models #################
#########################################################
library(lme4)

################### F-2+ ################
library(sjPlot)
library(effects)

nymph.stream.F2 <- subset(nymph.stream, Instar == "F-2+")
#4 TDS outliers can stay because TDS not incorporated into model
nymph.stream.F2 <- subset(nymph.stream.F2, Sp.cond < (mean(nymph.stream.F2$Sp.cond) + 
                                                      0.5*sd(nymph.stream.F2$Sp.cond))) #remove 4 outliers (there are 4)
nymph.stream.F2 <- subset(nymph.stream.F2, Depth < (mean(nymph.stream.F2$Depth) + 
                                                      2*sd(nymph.stream.F2$Depth))) #remove 2 outliers
nymph.stream.F2 <- subset(nymph.stream.F2, Stream.width < (mean(nymph.stream.F2$Stream.width) + 
                                                      2*sd(nymph.stream.F2$Stream.width))) #remove 5 outliers (there are 6)


nymph.stream.F2[,c(23:32)] <- scale(nymph.stream.F2[,c(23:32)])
row.names(nymph.stream.F2) <- as.vector(1:length(nymph.stream.F2$Unique_ID))
#I've removed canopy cover because it varies along the length of the stream too much
model.F2 <- lmer(formula = HW_mm ~ pH + DO +  Water.temp + 
                Sp.cond + Turbidity + Stream.width + Depth +
                (1|Unique.site), data = nymph.stream.F2)
summary(model.F2)

plot_model(model.F2,
           axis.labels=c("Stream Depth", "Stream Width", "Turbidity", "Specific Cond.", 
                         "Water Temp",  "DO", "pH"),
           show.values=TRUE, show.p=TRUE, value.offset = .29,
           title="Scaled Effect of Environment on F-2+ Head Width")
tab_model(model.F2)

effects_cond <- effect(term= "Sp.cond", mod= model.F2)
summary(effects_cond)
x_cond <- as.data.frame(effects_cond)
cond_plot <- ggplot() + 
  geom_point(data=nymph.stream.F2, aes((Sp.cond), HW_mm)) + 
  geom_point(data=x_cond, aes(x=(Sp.cond), y=fit), color="blue") +
  geom_line(data=x_cond, aes(x=(Sp.cond), y=fit), color="blue") +
  geom_ribbon(data= x_cond, aes(x=(Sp.cond), ymin=lower, ymax=upper), alpha= 0.3, fill="blue") +
  labs(x="Specific Conductance ()", y="Head Width (mm)")
cond_plot

#Correlation analyses for F2+ sites
##################################
nymph.stream.F2 <- nymph.stream.F2[!duplicated(nymph.stream.F2$Unique.site),]
F2_env <- nymph.stream.F2[,(23:32)]
colnames(F2_env) <- c("Canopy cov.", "Water Temp.", "Velocity", "pH", "DO", "TDS", "Sp. cond.", "Turbidity", "Width", "Depth")
F2_env <- subset(F2_env, pH != "na")
F2_env <- subset(F2_env, Turbidity != "na")
corrplot(cor(F2_env), method='number', type = 'lower', diag = F, tl.col = "black", 
         number.cex = .8, tl.cex = .95) #correlation plot of stream measurements

################### F-1 ################
#########################################
nymph.stream.F1 <- subset(nymph.stream, Instar == "F-1")
nymph.stream.F1 <- subset(nymph.stream.F1, nymph.stream.F1$Sp.cond < (mean(nymph.stream.F1$Sp.cond) + 
                                                                  1*sd(nymph.stream.F1$Sp.cond))) # remove 2 outliers

nymph.stream.F1[,c(23:32)] <- scale(nymph.stream.F1[,c(23:32)])
row.names(nymph.stream.F1) <- as.vector(1:length(nymph.stream.F1$Unique_ID))

model.F1 <- lmer(formula = HW_mm ~ pH + DO + Water.temp + 
                   Sp.cond + Turbidity + Stream.width + Depth +
                   (1|Unique.site), data = nymph.stream.F1) #don't include velocity for various reasons
summary(model.F1)

plot_model(model.F1,
           axis.labels=c("Stream Depth", "Stream Width", "Turbidity", "Specific Cond.", 
                         "Water Temp", "DO", "pH"),
           show.values=TRUE, show.p=TRUE, value.offset = .29,
           title="Scaled Effect of Environment on F-1 Head Width")
tab_model(model.F1)
effects_DO <- effect(term= "DO", mod= model.F1)
summary(effects_DO)
x_DO <- as.data.frame(effects_DO)
DO_plot <- ggplot() + 
  geom_point(data=nymph.stream.F1, aes(DO, HW_mm)) + 
  geom_point(data=x_DO, aes(x=DO, y=fit), color="blue") +
  geom_line(data=x_DO, aes(x=DO, y=fit), color="blue") +
  geom_ribbon(data=x_DO, aes(x=DO, ymin=lower, ymax=upper), alpha= 0.3, fill="blue") +
  labs(x="Dissolved Oxygen (%)", y="Head Width (mm)")
DO_plot

effects_temp <- effect(term= "Water.temp", mod= model.F1)
summary(effects_temp)
x_temp <- as.data.frame(effects_temp)
temp_plot <- ggplot() + 
  geom_point(data=nymph.stream.F1, aes((Water.temp), HW_mm)) + 
  geom_point(data=x_temp, aes(x=(Water.temp), y=fit), color="blue") +
  geom_line(data=x_temp, aes(x=(Water.temp), y=fit), color="blue") +
  geom_ribbon(data= x_temp, aes(x=(Water.temp), ymin=lower, ymax=upper), alpha= 0.3, fill="blue") +
  labs(x="Water Temp (C)", y="Head Width (mm)")
temp_plot

effects_cond <- effect(term= "Sp.cond", mod= model.F1)
summary(effects_cond)
x_cond <- as.data.frame(effects_cond)
cond_plot <- ggplot() + 
  geom_point(data=nymph.stream.F1, aes((Sp.cond), HW_mm)) + 
  geom_point(data=x_cond, aes(x=(Sp.cond), y=fit), color="blue") +
  geom_line(data=x_cond, aes(x=(Sp.cond), y=fit), color="blue") +
  geom_ribbon(data= x_cond, aes(x=(Sp.cond), ymin=lower, ymax=upper), alpha= 0.3, fill="blue") +
  labs(x="Specific Conductance ()", y="Head Width (mm)")
cond_plot

#Correlation analyses for F1 sites
##################################
nymph.stream.F1 <- nymph.stream.F1[!duplicated(nymph.stream.F1$Unique.site),]
F1_env <- nymph.stream.F1[,(23:32)]
colnames(F1_env) <- c("Canopy cov.", "Water Temp.", "Velocity", "pH", "DO", "TDS", "Sp. cond.", "Turbidity", "Width", "Depth")
F1_env <- subset(F1_env, Turbidity != "na")
F1_env <- subset(F1_env, pH != "na")
corrplot(cor(F1_env), method='number', type = 'lower', diag = F, tl.col="black",
         number.cex = .8, tl.cex = .95) #correlation plot of stream measurements