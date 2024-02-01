
library(data.table)
library(stringr)
library(ggplot2)
library(reshape2)

setwd("/Users/f00502n/Documents/Dartmouth/Skiway/TomsT")
wd <- getwd()
collection.date <- "2024_01_30"

files <- dir("/Users/f00502n/Documents/Dartmouth/Skiway/TomsT", 
             recursive=TRUE, full.names=TRUE, pattern="\\.csv$")
files <- files[which(str_detect(files, "data") == TRUE)]
files <- files[which(str_detect(files, collection.date) == TRUE)] #shouldn't have to do this again but it wasn't workign otherwise
alldata <- lapply(files, function(x) read.csv(x, header = FALSE, sep = ";"))
names(alldata) <- files
alldata <- rbindlist(alldata,  idcol = T)[, c(1, 3, 5:8)]

colnames(alldata) <- c("ID", "DateTime", "T1", "T2", "T3", "Hu")

#read in sensor ID key
ID.key <- read.csv("TomsT ID.csv")
ID.key$ID <- as.character(ID.key$ID)
ID <- paste(ID.key$ID, collapse = "|")
alldata$ID2 <- str_extract(alldata$ID, ID)
alldata <- merge(alldata, ID.key, by.x = "ID2", by.y = "ID")

alldata$DateTime <- as.POSIXct(alldata$DateTime, format = "%Y.%m.%d %H:%M")
alldata$DateTime2 <- alldata$DateTime - 5*3600 #correct for 5h difference 

alldata$VWC.default <- (alldata$Hu ^2 * -1.34E-8 + alldata$Hu * .000249622 - 0.157889) *100
alldata$VWC.sandy.loam.B <- (alldata$Hu ^2 * -9E-10 + alldata$Hu * .000261847 - 0.158618303) *100

#
treatment.key <-read.csv("/Users/f00502n/Documents/Dartmouth/Skiway/TomsT/manual measurement/Treatment Key.csv")
alldata <- merge(alldata, treatment.key, by = "Plot")

manual.STM <-read.csv("/Users/f00502n/Documents/Dartmouth/Skiway/TomsT/manual measurement/STM - Sheet1.csv") 
manual.STM$DateTime <- paste(manual.STM$Date, "10:00")
manual.STM$DateTime <- as.POSIXct(manual.STM$DateTime, format = "%m/%d/%y %H:%M")
#merge with TomsT
comparison <- merge(manual.STM, alldata, by = c("DateTime", "Plot"))



#trim to start when TomsT were deployed
alldata2 <- alldata %>%
  subset(DateTime > "2023-11-14 18:00:00")

ggplot(alldata2) + geom_line(aes(x = DateTime, y = T1, color = Treatment))
ggplot(alldata2) + geom_line(aes(x = DateTime, y = VWC.sandy.loam.B, color = Plot))

ggplot(alldata2) + geom_point(aes(x = VWC.default, y = VWC.sandy.loam.B))


ggplot(comparison) + geom_point(aes(x = VWC, y = VWC.default, color = Date))
ggplot(comparison) + geom_point(aes(x = VWC, y = VWC.sandy.loam.B))

comparison2 <- melt(comparison, id.vars = c("Plot", "Date", "Treatment"), measure.vars = c("VWC", "VWC.default"))
ggplot(comparison2) + geom_boxplot(aes(x = Treatment, y = value, group.by = variable, color = variable)) +
  facet_wrap(~Date)

#tomsT average melted vs control
ggplot(alldata2) + geom_boxplot(aes(x = Treatment, y = VWC.default, color = Treatment))

ggplot(alldata)+geom_line(aes(x=DateTime,y=VWC.default))

write.csv(alldata2, "TomsT_compiled.csv")

