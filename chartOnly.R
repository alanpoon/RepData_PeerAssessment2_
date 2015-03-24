library(ggplot2)
library(data.table)
library(plyr)
##check if storm.csv exist

if (!"stormData.csv.bz2" %in% dir("../data/")) {
  print("hhhh")
  download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", destfile = "../data/stormData.csv.bz2")
  bunzip2("../data/stormData.csv.bz2", overwrite=T, remove=F)
}

if (!"stormData" %in% ls()) {
  stormData <- data.table(read.csv("../data/stormData.csv", sep = ","))
}
dim(stormData)
head(stormData, n = 2)


#function that will sort the 15 most impactful weather type
sort15<- function(data,impactedField){
##use get() in the sum function in the datatable structure
  df<-as.data.frame(data[, sum(get(impactedField), na.rm=TRUE), by=EVTYPE])
  top<-15
    sorted<- arrange(df,desc(V1))
  sorted[c(1:15),]
}


simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

plotChart<-function(sortedData,impactedField,plotName){
  # on png device
  formattedString<-simpleCap(impactedField)
  ggTitleString<-paste('Total ',formattedString,' in US From 1995-2011',sep="")
  png(plotName, width=480, height=480)
  g<-ggplot(data=sortedData, aes(x=reorder(EVTYPE,desc(V1)), y=V1)) +
    geom_bar(stat="identity") +
    
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    labs(y=formattedString,x='Weather Types') +
    ggtitle(ggTitleString)
  print(g)
  dev.off()
}
sortedFatalities<-sort15(stormData,"FATALITIES")
sortedPropDmg<-sort15(stormData,"PROPDMG")

plotChart(sortedFatalities,"FATALITIES","Fatalities.png")
plotChart(sortedPropDmg,"PROPDMG","PropertyDmg.png")