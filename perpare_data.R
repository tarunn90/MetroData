library(dplyr)
library(reshape2)
library(ggplot2)

rm(list=ls())
setwd("/Users/johnricco/Documents/Projects/metro_time")

### Read in CSV files
rides <- read.csv("export.csv", stringsAsFactors=FALSE)
stations <- read.csv("my_metro.csv", stringsAsFactors=FALSE)

########################## Rides data ########################## 

### Filter and collapse by sums
rideSums <- rides %>% 
  select(day=ENTDATEDAYOFWEEK, station=ENTSTATION, time=ENTQUARTHOUR, trips=AVG_TRIPS ) %>%
  filter(day != "Sat", day != "Sun") %>%
  arrange(station,time) %>%
  group_by(station, time) %>%
  summarise(totalTrips=sum(trips))

stopifnot(nrow(rideSums) <= 24*4*91) # check no. of rows
stopifnot(length(unique(rideSums$station))==91) # check no. of stations

rm(rides)

### Convert to military time
rideSums <- rideSums %>%
  mutate(mlTime = substr(time,1,8) ) %>%
  mutate(mlTime = as.character(strptime(mlTime, "%I:%M %p" )) ) %>%
  mutate(mlTime = substr(mlTime,12,20) ) %>%
  arrange(mlTime) %>%
  group_by()

########################## Stations data ########################## 

statnsReshaped <- stations %>%
  select(station,line,stationorder) %>%
  dcast(station ~ line)

stopifnot(nrow(statnsReshaped)==91)

########################## Merge data ########################## 

### Check the station names are both in sequential order for merging
mergeNames <- statnsReshaped %>%
  arrange(station) %>%
  select(stationName = station) %>%
  mutate(has = statnsReshaped$station %in% rideSums$station) %>%
  mutate(rideSumsName = unique(rideSums$station)) 
rm(mergeNames)

### Fix station names to order correctly
statnsReshaped <- statnsReshaped %>%
  mutate(station = replace(station, station=="Rhose Island Ave-Brentwood","Rhode Island Avenue") ) %>%
  mutate(station = replace(station, station=="Ronald Reagan Washington National Airport","Reagan Washington National Airport") ) %>%
  arrange(station)

### Create indices to merge on
statnsReshaped <- statnsReshaped %>%
  mutate(mergeIndex = row(statnsReshaped)[,1])

rideSums$mergeIndex <- as.numeric(interaction(rideSums$station))

merged <- inner_join(rideSums,statnsReshaped, by="mergeIndex")

### Filter out early times
times <- unique(merged$mlTime)
times <- data.frame(mlTime = times[order(times)])
times$id <- as.numeric(as.factor((times$mlTime)))
times <- times %>% 
  filter( id >= 19 ) %>% 
  select( -id )

filtered <- semi_join(merged,times, by = 'mlTime')

filtered <- filtered %>%
  group_by() %>%
  arrange(station.x, mlTime) %>%
  select(-mergeIndex , -time, -station.y)  %>%
  rename(Station=station.x , Entrances = totalTrips, Time = mlTime ) %>%
  mutate(Time = substr(Time,1,5) )

### Split into 6 data frames
mySplit <- function(Line) {
  index <- which(colnames(filtered)==Line)  
  df1 <- filtered[,c(1:3,index)]
  df1 <- df1[which(complete.cases(df1)),]
  return( df1 )
}

myList <- c(colnames(filtered)[4:9] )

dfList <- lapply(myList, mySplit)
names(dfList) <- myList

rm(times, merged, rideSums, statnsReshaped)

### Create balanced panel for each line
myPanel <- function(dfPassed) {
  allStations <- unique(dfPassed[[1]])
  allTimes <- unique(dfPassed[[3]])
  Station <- unlist(lapply(allStations, function(x) rep(x, length(allTimes))))
  Time <- rep(allTimes, length(allStations))
  NewDf <- data.frame(Station, Time)
  return(NewDf)
}

dfListPanel <- lapply(dfList, myPanel)

### Merge the data into each balanced panel

myMerge <- function(z) { 
  df1 <- left_join(dfListPanel[[z]],dfList[[z]],by=c("Station","Time"))
  df1 <- df1[,1:3]

  colnames(dfList[[z]])[4] <- "Order"
  
  df2 <- dfList[[z]] %>%
    group_by(Station) %>%
    summarise(Order=mean(Order))
  
  df3 <- left_join(df1,df2,by="Station")
  return(df3)
}

dfListBalanced <- lapply(1:6, myMerge )
names(dfListBalanced) <- myList

for (i in 1:6) {
  dfListBalanced[[i]][3] <- replace(dfListBalanced[[i]][3], is.na(dfListBalanced[[i]][3]), 0)
}

myVerySmartPlan <- function(x) {
  thisName <- names(dfListBalanced[x])
  dfListBalanced[[x]] <- dfListBalanced[[x]] %>%
    mutate(line=thisName)
}

dfListBalanced <- lapply(1:6, myVerySmartPlan)
names(dfListBalanced) <- myList

myUberMerge <- function(x) {
  stationsMerge <- distinct(stations[,14:18])
  dfListBalanced[[x]] <- left_join(dfListBalanced[[x]],stationsMerge,by="line")
}

d <- lapply(1:6, myUberMerge)
d <- lapply(1:6, function(x) d[[x]] %>% arrange(Order))
names(d) <- myList

rm(filtered, stations, dfList, dfListBalanced, dfListPanel, i, myMerge, myPanel, mySplit, myUberMerge, myVerySmartPlan)

### Write to csv
lapply(1:6, function(x) write.csv(d[[x]], paste0("cleaned_data/", myList[x], ".csv")))

