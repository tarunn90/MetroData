library(ggplot2)
library(dplyr)
library(shiny)

green <- dfListBalanced$Green

  
# # Bar
# p1 <- ggplot(greenSelect,aes(x=reorder(Station,Green),y=Entrances)) + geom_bar(stat="identity")
# p1 <- p1 + theme(axis.text.x = element_text(angle = 45, hjust = 1))
# p1
# 
# # Scatter with connected dots
# p2 <- ggplot(greenSelect, aes(x=reorder(Station,Green), y=Entrances,group=1)) + geom_point(color="green")
# p2 <- p2 + theme(axis.text.x = element_text(angle = 45, hjust = 1))
# p2 <- p2 + theme(axis.title.x = element_blank())
# p2 <- p2 + geom_line(color="green")
# p2 <- p2 + ggtitle("Green: 08:00")
# p2

### For each line:
# Put all times in vector
greenTimes <- unique(dfListBalanced$Green[,2])

# Create plot for single time in Green line
greenTrial1 <- dfListBalanced$Green %>%
  filter(Time==greenTimes[1]) %>%
  arrange(Order)

title = paste("Green:", as.character(greenTimes[1]))

plotTrial <- ggplot(greenTrial1, aes(x=reorder(Station,Order), y=Entrances,group=1)) + geom_point(color="green") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.title.x = element_blank()) +
  geom_line(color="green") +
  ggtitle(title)

plotTrial



# Cycle through every time for green line
myPlot <- function(TimePassed) {
  greenSubset1 <- dfListBalanced$Green %>%
      filter(Time==as.character(TimePassed))
  
  title = paste("Green: ", as.character(TimePassed))
  
  p3 <- ggplot(greenSubset1, aes(x=reorder(Station,Green), y=Entrances,group=1)) + geom_point(color="green")
  p3 <- p3 + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  p3 <- p3 + theme(axis.title.x = element_blank())
  p3 <- p3 + geom_line(color="green")
  p3 <- p3 + ggtitle(title)
  p3
  
  return(p3)
}


plotArray <- lapply(greenTimes, myPlot)

rm(plotArray,plotTrial,greenTrial1,greenSelect,greenTimes)

### NOTES
# Need to fix axis scale (within line; maybe across lines)
# General beyoo-tification (text, wider lines, white dots, etc.)
# Change color of lines
# Make lines smooth - geom? stat-smooth?
# put photo for background?
