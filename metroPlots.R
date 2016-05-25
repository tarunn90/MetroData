library(ggplot2)
library(dplyr)
library(shiny)

rm(list=ls())
home_path = "/Users/johnricco/Documents/Projects/metro_time/"
data_path = "/Users/johnricco/Documents/Projects/metro_time/cleaned_data/"
plot_path = "/Users/johnricco/Documents/Projects/metro_time/Plots"
setwd(data_path)

### Read data
d <- lapply(list.files(pattern = ".csv"), function(x) read.csv(x, stringsAsFactors = F))
times <- read.csv("./times/times.csv", stringsAsFactors = F)
times <- times[, 2]

################################ Plots ################################

setwd(plot_path)
ml_times <- unique(d[[1]]$Time)
lines <- unlist(lapply(1:6, function(x) unique(d[[x]]$line)))

plot_function <- function(time_id, this_line) {
  
  ex <- filter(d[[this_line]], Time == ml_times[time_id])
  ex_color <- unique(ex$color1)
  
  p <- ggplot(ex, aes(x = reorder(Station, Order), y = Entrances, group = 1)) + 
    
    #Aesthetic formatting
    geom_line(size = 6, color = ex_color) + 
    geom_point(size = 5, color = "black", fill = "white", shape = 21) +
    
    #Axis formatting
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(axis.title.x = element_blank()) +
    scale_y_continuous(limits = c(0, 10000)) +
    
    #Text formatting
    ggtitle(times[time_id])
  
  p
  ggsave(paste0(lines[this_line], time_id,".png"), width = 9, height = 8)

}

for (i in 1:6) {
  for (j in 1:78) {
    plot_function(j, i)
  }
}
