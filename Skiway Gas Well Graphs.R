# First look at Skiway gas wells

library(ggplot2)
library(plotrix)
library(data.table)
library(stringr)
library(lme4)
library(lmerTest)

setwd("/Users/f00502n/Documents/Dartmouth/Fibox_Processing/Skiway/ARQ")

files <- (Sys.glob("*.csv"))
alldata <- lapply(files, function(x) read.csv(x, header = TRUE)) 
names(alldata) <- files
alldata <- rbindlist(alldata,  idcol = T)
alldata$Date <- as.Date(alldata$Date, format = "%Y-%m-%d")
#extract block information
alldata$Block <- substr(alldata$Plot, 1, 1)
alldata$Quadrant <- substr(alldata$Plot, 4, 4)
alldata$Plot2 <- substr(alldata$Plot, 1, 2)

#check there are no differences between fibox/IRGA setups
alldata$ID <- substr(alldata$.id, 11, 18)

ggplot(alldata) + geom_boxplot(aes(x = ID, y = corrCO2, color = ID))
ggplot(alldata) + geom_boxplot(aes(x = ID, y = corrO2, color = ID))

#look at standards only
standards <- alldata %>%
  subset(Treatment == "standard")
summary(aov(data = standards, corrCO2 ~ ID))
summary(aov(data = standards, corrO2 ~ ID))

ggplot(standards) + geom_boxplot(aes(x = ID, y = corrCO2, color = ID))
ggplot(standards) + geom_boxplot(aes(x = ID, y = corrO2, color = ID))


#make some graphs!!
alldata <- alldata %>%
  subset(Treatment != "ambient" & Treatment != "standard") # & Treatment != "50_percent" )

alldata$Depth <- factor(alldata$Depth, levels = c("SNOW", "5", "10", "20", "30"))

#color by block

ggplot(alldata, aes(x = Date, y = corrCO2, color = Treatment, label = Plot)) + geom_point() +
  facet_wrap(~Depth) + geom_text()

ggplot(alldata, aes(x = Date, y = corrO2, color = Treatment, label = Plot)) + geom_point() +
  facet_wrap(~Depth) + geom_text()

ggplot(alldata, aes(x = Date, y = ARQ, color = Treatment, label = Plot)) + geom_point() +
  facet_wrap(~Depth) + geom_text()
 
averages <- alldata %>%
    dplyr::group_by(Date, Depth, Treatment) %>%
    dplyr::summarize(Avg.CO2 = mean(corrCO2, na.rm = T), Avg.O2 = mean(corrO2, na.rm = T),Avg.ARQ = mean(ARQ, na.rm = T),
              sterr.CO2 = std.error(corrCO2, na.rm = T), sterr.O2 = std.error(corrO2, na.rm = T),
              sterr.ARQ = std.error(ARQ, na.rm = T))

ggplot(averages) + geom_point(aes(x = Date, y = Avg.CO2, color = Treatment)) +
  geom_errorbar(aes(x = Date, ymin = Avg.CO2 - sterr.CO2, ymax = Avg.CO2 + sterr.CO2, color = Treatment)) +
  geom_line(aes(x = Date, y = Avg.CO2, group = Treatment, color = Treatment)) +
  facet_wrap(~Depth) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggplot(averages) + geom_point(aes(x = Date, y = Avg.O2, color = Treatment)) +
  geom_errorbar(aes(x = Date, ymin = Avg.O2 - sterr.O2, ymax = Avg.O2 + sterr.O2, color = Treatment)) +
  geom_line(aes(x = Date, y = Avg.O2, group = Treatment, color = Treatment)) +
  facet_wrap(~Depth) 

ggplot(averages) + geom_point(aes(x = Date, y = Avg.ARQ, color = Treatment)) +
  geom_errorbar(aes(x = Date, ymin = Avg.ARQ - sterr.ARQ, ymax = Avg.ARQ + sterr.ARQ, color = Treatment)) +
  geom_line(aes(x = Date, y = Avg.ARQ, group = Treatment, color = Treatment)) +
  facet_wrap(~Depth) 


pre.treatment <- alldata %>%
  subset(Date < "2024-01-09" )

mod_try <- lmer(corrCO2 ~ Treatment + Block + (1|Plot2:Quadrant), data = pre.treatment) 
summary(mod_try)
anova(mod_try)



post.treatment <- alldata %>%
  subset(Date > "2024-01-17")

mod_try <- lmer(corrCO2 ~ Treatment + Block + (1|Plot2:Quadrant), data = post.treatment) 
summary(mod_try)
anova(mod_try)





