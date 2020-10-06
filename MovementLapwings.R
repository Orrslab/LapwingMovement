#############1) Import the data and evaluate what tag works:############

####a) setting libraries ####
#Set the directory
rm(list=ls())
cat("\014")
###Lines that need to change to the current the date: 53,173,174,176,177, 229,230.
memory.limit(size=56000)

#Write the date you want:

#Beggining <- ""

#EndTime <- ""


setwd('D:/OrrS4/Desktop/Miki/SQL')
# Load libraries 
library(reshape2)
library(ggpubr)# package needed to the ATLAS package (for plotting)
library(htmltools) # to add "pop-ups" to leaflet maps
library(dbscan) # clustering algorithm
library(toolsForAtlas)
library(RMySQL)
library(ggplot2)
library(leaflet)
library(sp)
library(rgdal)
library(lubridate)
library(dplyr)
library(plyr)
library(RSQLite)
library(shiny)
library(dismo)
library(readxl)
#library(tictoc)
source("func\\visualMaps.R")    # my functions for maps
source("func\\Movement_Metrics_Functions.R")   # my enhancements for ADP
source("func/SplitNights.R")# helper functions for time-segmentation and data filtering

####b) import from SQL ####

# Functions to interact with databases
# --- Database Connection 
dbc <- dbConnect(RMySQL::MySQL(),
                 user = 'roatlasharod',            # username 
                 password = 'roatlasHarodOrr5678#',# password
                 host = '132.66.79.21',            # host ip address
                 port=5900,                        # port Number
                 dbname='harod')                   # name of data base


# --- Examine the tables contained in the database 
dbListTables(dbc)           

# --- Examine the names of the columns in a table
dbListFields(dbc, 'DETECTIONS')
dbListFields(dbc, 'LOCALIZATIONS')

# --- Set start & end time and convert to ATLAS time

Start_Time_Str ='2020-03-31 06:00:00' # define start time
Start_Time_Str_Temp <- as.character.Date(Start_Time_Str) 
ATLAS_Start_Time<-as.numeric(as.POSIXct(Start_Time_Str_Temp,
                                        "%Y-%m-%d %H:%M:%S", tz="UTC"))*1000

End_Time_Str ='2020-09-19 06:00:00' # Need to change to corrent date
End_Time_Str_Temp <- as.character.Date(End_Time_Str)
ATLAS_End_Time<-as.numeric(as.POSIXct(End_Time_Str_Temp,
                                      "%Y-%m-%d %H:%M:%S", tz="UTC"))*1000 

# --- Set Tags ID

####c) import the tags names and the start time of tags and creating a new raw data of locations and detections ####

#The list of tags with the beggining dates (from the captured data)
ListOfStart <- read.csv("TAG_dates.csv")

TagListing <- ListOfStart$TAG  #Create a list with only the tags
#Make the tag list as full tag name:

for (i in 1:length(TagListing)){
  d <- ListOfStart$TAG[i]
  if (d < 100){ 
    ListOfStart$FullTag[i] <- paste(9720060000, d, sep = "")
    ListOfStart$TAG2[i] <- paste(0, d, sep = "")
  } else { ListOfStart$FullTag[i] <- paste(972006000, d, sep = "")
  ListOfStart$TAG2[i] <- paste(d)}
}

ListOfStart$TAG <- ListOfStart$TAG2
FullTag <- ListOfStart$FullTag  #Create a list with only the tags
AllTagsDet <- list() #make an empty list for detections

for (i in 1:length(FullTag)) {
  query = paste('select TAG,TIME from DETECTIONS WHERE TAG=',FullTag[i],
                'AND TIME >=', ATLAS_Start_Time, 'AND TIME <=', ATLAS_End_Time)
  All_Data <- dbGetQuery(dbc,query)
  AllTagsDet[[i]] <- All_Data
}


AllTagsLoc <- list() #make an empty list for localizations

for (i in 1:length(FullTag)) {
  query = paste('select TAG,TIME,X,Y,Z,VARX,VARY,COVXY from LOCALIZATIONS WHERE TAG=',FullTag[i],
                'AND TIME >=', ATLAS_Start_Time, 'AND TIME <=', ATLAS_End_Time)
  All_Data <- dbGetQuery(dbc,query)
  AllTagsLoc[[i]] <- All_Data
}

AllthetagsDet <- do.call(rbind.data.frame, AllTagsDet)
AllthetagsLoc <- do.call(rbind.data.frame, AllTagsLoc)

rm(AllTagsDet,AllTagsLoc)

dbDisconnect(dbc)
head(AllthetagsLoc)

names(AllthetagsDet)[names(AllthetagsDet) == "TIME"] <- "Detection_Time"
names(AllthetagsLoc)[names(AllthetagsLoc) == "TIME"] <- "Location_Time"

# Make the locations as ITM
AllthetagsLoc <-convertSpatial.ITM2WGS84(AllthetagsLoc, xyColNames=c("X","Y"))
AllthetagsLoc <- as.data.frame(AllthetagsLoc)

AllthetagsDet$DateDetection<-as.POSIXct((AllthetagsDet$Detection_Time)/1000, tz="UTC", origin="1970-01-01")
AllthetagsLoc$DateLocation<-as.POSIXct((AllthetagsLoc$Location_Time)/1000, tz="UTC", origin="1970-01-01")

AllthetagsDet$TAG <- substr(AllthetagsDet$TAG, 10, 13)
AllthetagsLoc$TAG <- substr(AllthetagsLoc$TAG, 10, 13)

AllthetagsDet<-AllthetagsDet[order(AllthetagsDet$TAG,AllthetagsDet$Detection_Time),] #make sure data is sorted chronologically (per tag)
AllthetagsLoc<-AllthetagsLoc[order(AllthetagsLoc$TAG,AllthetagsLoc$Location_Time),] #make sure data is sorted chronologically (per tag)
####d) Create a list for detection and locations ####

taillistDet <- list() #empty list
taillistLoc <- list() #empty list

Lisoftags <- unique(AllthetagsDet$TAG)  #Create a list with only the tags

#For detection
for(i in 1:length(Lisoftags)) {
  d <- AllthetagsDet[AllthetagsDet$TAG==Lisoftags[i],]
  d2 <- tail(d, 1)
  d2 <- subset(d2, select=c("TAG", "DateDetection"))
  taillistDet[[i]] <- d2
  print(d2)
}
#For locations
for(i in 1:length(Lisoftags)) {
  d <- AllthetagsLoc[AllthetagsLoc$TAG==Lisoftags[i],]
  d2 <- tail(d, 1)
  d2 <- subset(d2,  select=c("TAG", "DateLocation", "LON", "LAT"))
  taillistLoc[[i]] <- d2
  print(d2)
}

Lastdet <- do.call(rbind.data.frame, taillistDet)
Lastloc <- do.call(rbind.data.frame, taillistLoc)

AllLastDetLoc <- merge(Lastdet, Lastloc, by = 'TAG')
AllLastDetLoc <- merge(AllLastDetLoc, ListOfStart, by = 'TAG')

AllLastDetLoc<-AllLastDetLoc[order(AllLastDetLoc$TAG),] #make sure data is sorted chronologically (per tag)
AllLastDetLoc$fulllast <- AllLastDetLoc$DateTime
AllLastDetLoc$fullstart <- paste(AllLastDetLoc$date_capture, AllLastDetLoc$start_hour, sep = " ")
str(AllLastDetLoc)
AllLastDetLoc$fullstart <- as.POSIXct(AllLastDetLoc$fullstart, format="%d/%m/%Y %H:%M:%S", tz="UTC")

head(AllLastDetLoc)
#Start_to_det
AllLastDetLoc$Start_To_Det <- as.numeric(difftime(AllLastDetLoc$DateDetection, AllLastDetLoc$fullstart, units = "days"))
AllLastDetLoc$Start_To_Det <- round(AllLastDetLoc$Start_To_Det, digits = 0)
#Start_to_loc
AllLastDetLoc$Start_To_Loc <- as.numeric(difftime(AllLastDetLoc$DateLocation, AllLastDetLoc$fullstart, units = "days"))
AllLastDetLoc$Start_To_Loc <- round(AllLastDetLoc$Start_To_Loc, digits = 0)
#Loc_to_det
AllLastDetLoc$Loc_To_det <- as.numeric(difftime(AllLastDetLoc$DateDetection, AllLastDetLoc$DateLocation, units = "days"))
AllLastDetLoc$Loc_To_det <- round(AllLastDetLoc$Loc_To_det, digits = 0)

#Order the data so it show what I want
AllLastDetLoc$End_Det <- substr(AllLastDetLoc$DateDetection, 0,10)
AllLastDetLoc$End_Loc <- substr(AllLastDetLoc$DateLocation, 0,10)

AllLastDetLoc$ProblemDetections<- NA
AllLastDetLoc$ProblemDetections[AllLastDetLoc$End_Det == "2020-09-19"] <- "Work" # Need to change according to the last wanted date
AllLastDetLoc$ProblemDetections[AllLastDetLoc$End_Loc == "2020-09-19"] <- "Work" # Need to change according to the last wanted date

AllLastDetLoc$ProblemDetections[AllLastDetLoc$End_Det == "2020-09-19" & AllLastDetLoc$End_Loc != "2020-09-19"] <- "No Location" # Need to change to corrent date
AllLastDetLoc$ProblemDetections[AllLastDetLoc$End_Det != "2020-09-19" & AllLastDetLoc$End_Loc != "2020-09-19"] <- "Dont work" # Need to change to corrent date

#Without Ofri
Workornot <- AllLastDetLoc[with(AllLastDetLoc, !((TAG >= 131 & TAG <= 135))), ] #Without Ofri
counts <- table(Workornot$ProblemDetections)

####e) Mapping the locations and detections ####

ggplot(Workornot,
       aes(y = TAG)) +
  geom_point(aes(x = fullstart), 
             color = "Black",
             alpha = 1,
             size = 2) +
  labs(x = "Date", 
       y = "Tags",
       title = "Tags life spane") +
  theme_minimal() + 
  geom_point(aes( x = DateLocation)) +
  geom_segment(aes(x = fullstart,
                   y = TAG,
                   xend = DateDetection,
                   yend = TAG),
               color = "Red",
               size = 1) +
  geom_segment(aes(x = fullstart,
                   y = TAG,
                   xend = DateLocation,
                   yend = TAG),
               color = "Black",
               size = 1) +
  theme(legend.position = "bottom") + 
  geom_point(aes(x = DateLocation, y = TAG), 
             color = "Red",
             alpha = 1,
             size = 1) + 
  geom_point(aes(x = DateDetection), 
             color = "Red",
             alpha = 1,
             size = 1) + geom_text(aes( x = DateDetection, label=Start_To_Det), vjust=-0.2, size=2.8)

head(Lastdet)
head(Lastloc)

AllLastDetLoc$End_Det <- na_if(AllLastDetLoc$End_Det, "2020-09-19") # Need to change to corrent date
AllLastDetLoc$End_Loc <- na_if(AllLastDetLoc$End_Loc, "2020-09-19") # Need to change to corrent date
AllLastDetLoc$Loc_To_det <- na_if(AllLastDetLoc$Loc_To_det, 0) # Need to change to corrent date

#Removing unwanted columns:

AllLastDetLoc <- subset(AllLastDetLoc, select = -c(FullTag, TAG2,date_capture,start_hour) )
names(AllLastDetLoc)[names(AllLastDetLoc) == "fullstart"] <- "Capture time"
names(AllLastDetLoc)[names(AllLastDetLoc) == "DateDetection"] <- "Last detection"
names(AllLastDetLoc)[names(AllLastDetLoc) == "DateLocation"] <- "Last Location"
names(AllLastDetLoc)[names(AllLastDetLoc) == "Start_To_Loc"] <- "Location days"
names(AllLastDetLoc)[names(AllLastDetLoc) == "Loc_To_det"] <- "Detection without location"
names(AllLastDetLoc)[names(AllLastDetLoc) == "Start_To_Det"] <- "Detection days"
names(AllLastDetLoc)[names(AllLastDetLoc) == "End_Det"] <- "Last detection date"
names(AllLastDetLoc)[names(AllLastDetLoc) == "End_Loc"] <- "Last location date"
names(AllLastDetLoc)[names(AllLastDetLoc) == "ProblemDetections"] <- "Problem Detections"

#Write as CSV:
write.csv(AllLastDetLoc, file = paste('TagsDecLoc', 'csv', sep = '.'))


Lastdet$Indent <- "Detection"
Lastdet$Time <- AllLastDetLoc$`Detection days`

Lastloc$Indent <- "Location"
Lastloc$Time <- AllLastDetLoc$`Location days`

names(Lastdet)[names(Lastdet) == "DateDetection"] <- "Last Time"
names(Lastloc)[names(Lastloc) == "DateLocation"] <- "Last Time"

barplots <- bind_rows(Lastloc, Lastdet)

#Order the data so it show what I want
barplots$`Last Time`<- substr(barplots$`Last Time`, 0,10)
barplots$ProblemDetections<- NA
barplots$ProblemDetections[barplots$`Last Time` == "2020-09-19"] <- "Work" # Need to change according to the last wanted date
barplots$ProblemDetections[barplots$`Last Time` != "2020-09-19"] <- "Dont Work" # Need to change according to the last wanted date

ggplot(barplots) + 
  geom_col(aes(x = reorder(TAG, -Time), y = Time, fill = ProblemDetections), size = 1) +
  geom_line(aes(x = reorder(TAG, -Time), y = Time, color=Indent), size = 1.5, group = 1)

ggplot(data=barplots, aes(x = reorder(TAG, -Time), y=Time , fill = ProblemDetections)) +
  geom_bar(stat="identity", position=position_dodge()) + scale_fill_brewer(palette="Blues")+
  geom_line(aes(x = reorder(TAG, -Time), y = Time, color=Indent), size = 1.5, group = 1) +
  geom_text(aes(label=Time), vjust=-0.3, size=3.5)+
  theme_minimal() + ggtitle("Life span of all battery") +
  xlab("Tags") + ylab("Life span (Days)")  + labs(fill = "Tag Condition", color= " ")

# I will take AllLastDetLoc and remove the detection from the data - and will use it as the base of my metadata for tags:

TagMetadata <- AllLastDetLoc[ , -which(names(AllLastDetLoc) %in% c("Last detection","Detection days","Detection without location","Last detection date","Problem Detections"))]

#This will be saved for later use
#############2) removing the car time and/or the two first days:############
####a) Removing car  ####

#Date I need to start with: "Allthetags" and ""
# I will continue working on the data I made in the first script - 
#data I need to start with:
cat("\014")
#rm(AllDates)

#ListOfStart
#Allthetags

#Order it:
rm(All_Data,AllthetagsDet,d)
head(AllthetagsLoc)
AllthetagsLoc<-AllthetagsLoc[order(AllthetagsLoc$TAG,AllthetagsLoc$Location_Time),]

#Now I need to remove all the data before the lapwings were tagged (driving in the car),
# I need to subset ListOfStart to just tag, date_capture and start_hour:

TagStart <- ListOfStart[c("TAG", "date_capture", "start_hour")]

head(AllthetagsLoc)
str(AllthetagsLoc$DateLocation)
# I need to combine the date_capture and start_hour to one row and make it as time (DateTime POSIXct, format: "2020-03-31 06:05:10")

# I will create a list of the first time 

TagStart$DateLocation <- paste(TagStart$date_capture, TagStart$start_hour, sep = " ")
TagStart$DateLocation<- as.POSIXct(TagStart$DateLocation, format="%d/%m/%Y %H:%M:%S", tz="UTC")
str(TagStart) #Good


# Now i need to create a if function, that if the TAG column are the same, remove the data from
# that is smaller them DateTime

Lisoftags
FirstCleaning <- list()

for (i in Lisoftags){
  d <- subset(TagStart , TAG == i)
  tag <- subset(AllthetagsLoc, TAG == i)
  DF <- tag[!(as.POSIXlt(tag$DateLocation)< as.POSIXlt(d$DateLocation)),]
  FirstCleaning[[i]] <- DF
  print(head(DF,1))}
rm(tag,d,DF)


FirstTimeTaged <- do.call(rbind.data.frame, FirstCleaning)
rm(FirstCleaning,AllthetagsLoc)


####b) Removing Two days ####


names(FirstTimeTaged)[names(FirstTimeTaged) == "Location_Time"] <- "TIME"
rm(barplots,AllLastDetLoc,d2,Lastdet,Lastloc,ListOfStart,TagStart,taillistDet,
   taillistLoc,Workornot)
Listoftags <- unique(FirstTimeTaged$TAG)
FirstCleaning <- list()
#FirstCleaning<-addLocAttribute(FirstTimeTaged, locAttributs=c("distanceSpeed", "locQuality")) # This will take long time and will use a lot of CPU!!! close what you dont need

for (i in Lisoftags){
  tag <- subset(FirstTimeTaged, TAG == i)
  DF <-addLocAttribute(tag, locAttributs=c("distanceSpeed", "locQuality"))
  FirstCleaning[[i]] <- DF
  print(head(DF,1))}
rm(tag,DF)

RawData <- do.call(rbind.data.frame, FirstCleaning)
rm(FirstCleaning,FirstTimeTaged)

head(RawData)
RawData <- subset(RawData, select = -c(DateLocation) )
RawData <-RawData[order(RawData$TAG,RawData$TIME),]

unique(RawData$TAG)

#First - I will remove the first 2 days from the data, and the last day - I will move it to a seperate data from the first 2 days
first2days <- list()
DayFilter <- list()
listofdates <- unique(RawData$date)

for (a in Listoftags) {
  tag <- subset(RawData, TAG == a)
  listofdatestag <- unique(tag$date)
  removedates <- head(listofdatestag,2)
  firstdays <- subset(tag, date == removedates)
  first2days[[a]] <- firstdays
  tagremove <- filter(tag, !date %in% removedates)
  DayFilter[[a]] <- tagremove
  
}
warnings()
FirstDays <- do.call(rbind.data.frame, first2days)
unique(FirstDays$date)

rm(firstdays)

FirstFilter <- ldply(DayFilter, data.frame)
unique(DayFilter$date)

#Can couse problems!

#Can couse problems!

rm(first2days,removedates,tag,tagremove,firstdays, DayFilter)


#############3) Filtering the data:############

####a) stdVarXY filter ####
str(FirstFilter)
hist(FirstFilter$stdVarXY,xlim =c(0,100), breaks=10000, main= "Stdev Distribution") # Now the distribution is more informative.
hist(FirstFilter$spd,xlim =c(0,100), breaks=150, main= "Stdev Distribution") # Now the distribution is more informative.

summary(FirstFilter$stdVarXY)

FirstCleaning <- as.data.frame(FirstFilter %>%  filter(stdVarXY<20)) 
hist(FirstCleaning$stdVarXY,xlim =c(0,20), breaks=50, main= "Stdev Distribution") # Now the distribution is more informative.

TagList<-unique(FirstCleaning$TAG) # choose a radom TAG. 

TAG_ex<-sample(TagList, 1)

tag<-FirstCleaning[which(FirstCleaning$TAG==TAG_ex),]
tagname <- unique(tag$TAG)
####b) Speed filter ####
hist(tag$spd,xlim =c(0,40), breaks=10000,main= tagname)
summary(tag$spd)

#larger <- subset(FirstCleaning, spd >= 60 )

#hist(larger$spd,xlim =c(60,100), breaks=10000,main= "Speed Distribution") # Note that the speed units are (meters/seconds)
#nrow(FirstCleaning[FirstCleaning$spd >= 60,])

#1-nrow(FirstCleaning[FirstCleaning$spd > 60,])/nrow(FirstCleaning[FirstCleaning$spd <= 60,])

#spdThreshold<-25 # speed after analyzing the threshold is 25
summary(FirstCleaning$spd)

SecondCleaning <- as.data.frame(FirstCleaning %>%  filter(spd <= 25)) 

hist(SecondCleaning$spd,xlim =c(0,25), breaks=100,main= "Speed Distribution") # Note that the speed units are (meters/seconds)
summary(SecondCleaning$spd)

#nrow(SecondCleaning[SecondCleaning$spd > 5,])
#nrow(FirstCleaning[FirstCleaning$spd > 60,])


#1-nrow(SecondCleaning)/nrow(FirstCleaning)

#FirstCleaning$dT<-ceiling(FirstCleaning$dT) # roude time differences betweeb localisations (upwards) 

# create table of tag frequencies:
#TagsFreq<-as.data.frame(FirstCleaning %>%  
#       group_by(TAG)%>% 
#      summarise("Frequency"=min(dT, na.rm = TRUE)))
head(SecondCleaning)
####c) Smoothing and giving atribute anew####
names(SecondCleaning)
Thirdcleaning1 <- list()
FullTag <- as.factor(unique(SecondCleaning$TAG))
for (i in FullTag){
  tag <- subset(SecondCleaning, TAG == i)
  ronum <- length(tag$X)
  if (ronum > 0) {
    Thirdc<-triangularSmooth(tag$X, tag$Y, tag$TIME)
    
    smooth.1<-cbind("TAG"=tag$TAG, "VARX" = tag$VARX,
                    "VARY" = tag$VARY,"COVXY" = tag$COVXY,
                    "LON" = tag$LON,"LAT" = tag$LAT,
                    Thirdc[,c("X","Y","TIME")])
    smooth.1<-addLocAttribute(smooth.1, 
                              locAttributs =c("speed","angle"))
    Thirdcleaning1[[i]] <- smooth.1
    print(head(smooth.1,2))
  } else {
    print("have 0 rows and was not calculate")
  }}
#"distanceSpeed", "locQuality"
Firstsmooth <- do.call(rbind.data.frame, Thirdcleaning1)
names(Firstsmooth)

####d) Removing places where lapwings did not move (speed = 0)####
rm(Thirdcleaning1)
zeroz <- subset(Firstsmooth,spd <= 0 )
zeroz2 <- subset(Firstsmooth,distance <= 0  )

hist(zeroz$spd)
summary(zeroz$spd)
unique(zeroz$TAG)
Tag48 <- subset(zeroz,TAG == "048" )

plot(Tag48$X,Tag48$Y)

Thirdcleaning <- as.data.frame(Firstsmooth %>%  filter(spd>0)) 

Tag48 <- subset(Thirdcleaning,TAG == "048" )
unique(Tag48$date)
Tag48 <- subset(Tag48,date == "2020-08-02" )

plot(Tag48$X,Tag48$Y)
lines(Tag48$X,Tag48$Y)

####e) recalculating dT####
names(Thirdcleaning)
Thirdcleaning <- addLocAttribute(Thirdcleaning, 
                                 locAttributs =c("locQuality"))

#############4) Creating metadata:############
####a) Daily metadata ####
rm(DayFilter,dbc,FirstDays)
library("SpatialTools")

# I need the RawData for this method and ListOfTags

# I will first make a metadata with how much point i have and how much i clean

ListOfTags <- unique(RawData$TAG)
str(ListOfTags)
countpointday <- list()
countpointday2 <- list()
dataforlenght <- data.frame(matrix(ncol = 6, nrow = 0))
dataforlenght<- dataforlenght[nrow(dataforlenght) +1,]
x <- c("TAG","date","N_of_Total_Points","Avrage_speed_NoFILTER","Avrage_dT_NoFILTER","Distance_NoFILTER")
colnames(dataforlenght) <- x
colnames(RawData)
# Subset for only flying speed:
names(Thirdcleaning)
Flyfilter <- subset(Thirdcleaning, spd >=5)
#Walkfilter <- subset(Thirdcleaning, spd <5)
# Data per day:
rm(tag,Tag48,Thirdc,zeroz,zeroz2,FirstFilter,Firstsmooth,smooth.1,SecondCleaning)
for (i in ListOfTags) {
  d <- subset(RawData, TAG == i)
  ddd <- subset(FirstCleaning, TAG == i)
  dddd <- subset(Thirdcleaning, TAG == i)
  flyd <- subset(Flyfilter, TAG == i)
  #walkd <- subset(Walkfilter, TAG == i)
  DatesOfTags <- as.character(unique(d$date))
  countpointday <- list()
  for (b in DatesOfTags) {
    #if(b >= length(DatesOfTags)) break()
    dd <- subset(d, date == b)
    bbb <- subset(ddd, date == b)
    bbbb <- subset(dddd, date == b)
    flydd <- subset(flyd, date == b)
    #walkdd <- subset(walkd, date == b)
    
    daypoints <- nrow(dd)
    daypointsStdevFilter <- nrow(bbb)
    daypointsSpdFilter <- nrow(bbbb)
    FlyFil <- nrow(flydd)
    #WalkFil <- nrow(walkdd)
    
    dataforlenght$N_of_Total_Points <- daypoints
    dataforlenght$date <- unique(dd$date)
    dataforlenght$TAG <- i
    dataforlenght$Avrage_speed_NoFILTER <- mean(dd$spd, na.rm = TRUE)
    dataforlenght$Avrage_dT_NoFILTER <- mean(dd$dT, na.rm = TRUE)
    dataforlenght$Distance_NoFILTER <- sum(dd$distance, na.rm = TRUE)
    
    dataforlenght$N_of_Points_StdevFilter <- daypointsStdevFilter
    dataforlenght$Avrage_speed_StdevFilter <- mean(bbb$spd, na.rm = TRUE)
    dataforlenght$Avrage_dT_StdevFilter <- mean(bbb$dT, na.rm = TRUE)
    dataforlenght$Distance_StdevFilter <- sum(bbb$distance, na.rm = TRUE)
    
    dataforlenght$N_of_Points_SpdFilter <- daypointsSpdFilter
    dataforlenght$Avrage_speed_SpdFilter <- mean(bbbb$spd, na.rm = TRUE)
    dataforlenght$Avrage_dT_SpdFilter <- mean(bbbb$dT, na.rm = TRUE)
    dataforlenght$Distance_SpdFilter <- sum(bbbb$distance, na.rm = TRUE)
    
    dataforlenght$N_of_Fly <- FlyFil
    dataforlenght$Avrage_speed_Fly <- mean(flydd$spd, na.rm = TRUE)
    dataforlenght$Avrage_dT_Fly <- mean(flydd$dT, na.rm = TRUE)
    dataforlenght$Distance_Fly <- sum(flydd$distance, na.rm = TRUE)
    
    x1<-as.data.frame(c(bbbb[1],bbbb[2]), ncol=2, nrow= length(bbb$X), byrow=TRUE)
    dataforlenght$FilterMaxDisp <- max(dist(x1))
    
    x2<-as.data.frame(c(dd[1],dd[2]), ncol=2, nrow= length(dd$X), byrow=TRUE)
    dataforlenght$NoFilterMaxDisp <- max(dist(x2))
    #dataforlenght$N_of_walk <- WalkFil
    #dataforlenght$Avrage_speed_walk <- mean(walkdd$spd, na.rm = TRUE)
    #dataforlenght$Avrage_dT_walk <- mean(walkdd$dT, na.rm = TRUE)
    #dataforlenght$Distance_walk <- sum(walkdd$distance, na.rm = TRUE)
    #print(dataforlenght)
    countpointday[[b]] <- dataforlenght
    
  }
  tt <- do.call(rbind.data.frame, countpointday)
  countpointday2[[i]] <- tt
  print(paste(i, "Finish"))
} 

#?dist
#library("SpatialTools")
#x1<-as.data.frame(c(bbb[2],bbb[3]), ncol=2, nrow= length(bbb$X), byrow=TRUE)

#x2<-matrix(c(Flyfilter$X[3],Flyfilter$Y[3]), nrow = 1, ncol=2, byrow=TRUE)
#max(dist(x1))
#max(dist2(Flyfilter$X,Flyfilter$Y))

DayMetadata <- do.call(rbind.data.frame, countpointday2)
DayMetadata$month <- month(DayMetadata$date)
DayMetadata2 <- subset(DayMetadata, N_of_Fly > 0 )
DayMetadata$Distance_SpdFilter[DayMetadata$Distance_SpdFilter == 0] <- NA

write.csv(DayMetadata2, file = paste('DayMetadata', 'csv', sep = '.'), row.names = FALSE)

####b) Plot Daily Metadata ####

rm(countpointday,countpointday2,d,dd,tt)

DatesOfTagss <- as.character(unique(DayMetadata$date))

#for (d in DatesOfTagss) {
# ff <- subset(pointlenght, date == d)
#fdf <- ggplot(data=ff, aes(x = date, y = lenght, fill = TAG )) +
# geom_bar(stat="identity", position=position_dodge())  +
#theme_minimal() + ggtitle("Life span of all battery")
#  print(fdf)
#}

small_points <- subset(pointlenght, N_of_Points <= 50)
colnames(DayMetadata)

ggplot(data=DayMetadata, aes(x = date, y = Avrage_speed_SpdFilter)) +
  geom_line(aes(color = TAG)) +
  geom_point(aes(color=TAG), size=1)+
  theme_minimal() +
  facet_grid(~month, scales = "free_x", space = "free_x")
ggplot(data=DayMetadata, aes(x = date, y = Avrage_speed_NoFILTER)) +
  geom_line(aes(color = TAG)) +
  geom_point(aes(color=TAG), size=1)+
  theme_minimal() +
  facet_grid(~month, scales = "free_x", space = "free_x")
ggplot(data=DayMetadata, aes(x = date, y = Avrage_dT_NoFILTER)) +
  geom_line(aes(color = TAG)) +
  geom_point(aes(color=TAG), size=1)+
  theme_minimal() +
  facet_grid(~month, scales = "free_x", space = "free_x")
ggplot(data=DayMetadata, aes(x = date, y = Distance_NoFILTER)) +
  geom_line(aes(color = TAG)) +
  geom_point(aes(color=TAG), size=1)+
  theme_minimal() +
  facet_grid(~month, scales = "free_x", space = "free_x")

ggplot(data=DayMetadata, aes(x = Avrage_dT_NoFILTER, fill = TAG)) +
  geom_histogram(alpha = 0.5, position = "identity") +
  theme_minimal()

?stat_bin

####c) Individual Metadata ####
#Data per tag:
TagData <- data.frame(matrix(ncol = 5, nrow = 0))
TagData<- TagData[nrow(TagData) +1,]
x2 <- c("TAG","N_of_Total_Points","Avrage_speed_NoFILTER","Avrage_dT_NoFILTER","Distance_NoFILTER")
colnames(TagData) <- x2
TagMeta <- list()

for (b in ListOfTags) {
  dd <- subset(RawData, TAG == b)
  ddd <- subset(FirstCleaning, TAG == b)
  dddd <- subset(SecondCleaning, TAG == b)
  daypoints <- nrow(dd)
  daypointsStdevFilter <- nrow(ddd)
  daypointsSpdFilter <- nrow(dddd)
  TagData$N_of_Total_Points <- daypoints
  TagData$TAG <- b
  TagData$Avrage_speed_NoFILTER <- mean(dd$spd, na.rm = TRUE)
  TagData$Avrage_dT_NoFILTER <- mean(dd$dT, na.rm = TRUE)
  TagData$Distance_NoFILTER <- sum(dd$distance, na.rm = TRUE)
  TagData$N_of_Points_StdevFilter <- daypointsStdevFilter
  TagData$Avrage_speed_StdevFilter <- mean(ddd$spd, na.rm = TRUE)
  TagData$Avrage_dT_StdevFilter <- mean(ddd$dT, na.rm = TRUE)
  TagData$Distance_StdevFilter <- sum(ddd$distance, na.rm = TRUE)
  TagData$N_of_Points_SpdFilter <- daypointsSpdFilter
  TagData$Avrage_speed_SpdFilter <- mean(dddd$spd, na.rm = TRUE)
  TagData$Avrage_dT_SpdFilter <- mean(dddd$dT, na.rm = TRUE)
  TagData$Distance_SpdFilter <- sum(dddd$distance, na.rm = TRUE)
  TagMeta[[b]] <- TagData
  
}

TagMetadata2 <- do.call(rbind.data.frame, TagMeta)

TagMetadatafull <- merge(TagMetadata,TagMetadata2,by="TAG")

rm(TagMetadata2,TagMetadata)

write.csv(TagMetadatafull, file = paste('TagMetadata', 'csv', sep = '.'))


########################### NEXT ONLY RUN OVER NIGHT#########################
#############5) Mapping the data:############


library(mapview)
# Continue filter on SecondCleaning:

# I will loop the filtered csv as maps and save each lapwing to a different library:
TagList <- unique(SecondCleaning$TAG)
itm<-"+init=epsg:2039 +proj=tmerc +lat_0=31.73439361111111 +lon_0=35.20451694444445 +k=1.0000067 +x_0=219529.584 +y_0=626907.39 +ellps=GRS80 +towgs84=-48,55,52,0,0,0,0 +units=m +no_defs"
wgs84<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

for (i in TagList){
  d <- subset(SecondCleaning , TAG == i)
  Tagnum <- paste("Tag", i)
  if (file.exists(Tagnum)){
    setwd(file.path(Tagnum))
  } else {
    dir.create(file.path(Tagnum))
  }
  write.csv(d, file.path(paste("Tag", i), paste("Tag", i, '.csv')), row.names=FALSE)
  
  daysl <- unique(d$date)
  
  for (ii in 1:length(daysl)) {
    co <- rainbow(length(daysl))
    v2<-palette(co)
    dd <- subset(d , date == daysl[ii])
    Filedir <- paste(paste("Tag", i),"/",daysl[ii],".png", sep = '')
    dir.create(dirname(Filedir), showWarnings = FALSE)
    png(filename= Filedir)
    plot(dd$X,dd$Y)
    lines(dd$X,dd$Y)
    dev.off()
    coordinates(dd)<-~X+Y
    proj4string(dd)<-CRS(itm)
    #lines_sp <- as(dd, "SpatialLines")
    #plot(lines_sp, main = daysl[ii])
    #Filedir2 <- paste(paste("Tag", i),"/",daysl[ii],".kml", sep = '')
    # dir.create(dirname(Filedir2), showWarnings = FALSE)
    # plotKML::kml(dd[ii], extrude = TRUE,z.scale = 1,dir.create(dirname(Filedir), showWarnings = FALSE),
    #             file.name = Filedir2, 
    #            kmz = get("kmz", envir = plotKML.opts), open.kml = TRUE, width = 3, colour = v2[ii],points_names = dd$dateTime)
    
    llpd <- spTransform(dd, wgs84)
    #binpal <- colorBin(v2[i], 6, pretty = FALSE)
    ll<-leaflet() %>% addTiles() %>%
      addProviderTiles('Esri.WorldImagery') %>%
      addCircles(data=llpd, weight = 5, fillOpacity = 1,color = v2[ii]) %>%
      addPolylines(data=llpd@coords, weight = 1, opacity = 1)
    
    ll[ii]
    Filedir3 <- paste(paste("Tag", i),"/",daysl[ii],".png", sep = '')
    mapshot(ll,file = Filedir3) 
  }
  
}
