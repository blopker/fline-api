library(ggplot2)
library(zoo)
library(lubridate)
library(magrittr)
library(dplyr)
library(digitize)
library(tidyverse)
library(grid)
library("stringi", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library("stringr", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")

#select the folder. The folder contains "bg.csv" which is the output of the digitizer, and "food_log.csv"
data_folder <- "Alison_03_19_19"

#This is used in the plot titles
day <- "March 19th"

#wrapper function that I use to wrap the labels in the graph annotations
wrapper <- function(x, ...) paste(stri_wrap(x, ...), collapse = "\n")

#import food_log. It's already correctly formatted.
food_log <- as.data.frame(read.csv(paste("~/Dropbox/Fline/Plots from 2019 pilot/", data_folder, "/food_log.csv", sep="")), header=TRUE, sep=",")

#import the BG data
raw <- as.data.frame(read.csv(paste("~/Dropbox/Fline/Plots from 2019 pilot/", data_folder, "/bg", ".csv", sep=""), as.is=TRUE, sep = "\t"))
#only keep the the hour, minute, second.
colnames(raw) <- c("time", "bg")
#I use the lubridate library to format the time to look like this: "3H 30M 0S"
raw$time <- hm(mapply(paste, hour(ymd_hms(raw$time)), ":", minute(ymd_hms(raw$time)), SIMPLIFY=FALSE))
raw$id <- seq.int(nrow(raw))

#you can ignore this - this was the special formatting for Susan's android raw data
#raw$time <- hm(mapply(paste, hour(ymd_hms(raw$time)), ":", minute(ymd_hms(raw$time)), SIMPLIFY=FALSE))

#this function takes in the time of a food from "food_log", and searches in "raw" for what the user's
#glucose was at that time. it returns the row number of that glucose value.
#the matching is based on taking the difference between the food time and all the times listed in "raw",
#and selecting the one with the smallest difference. 
find_beginning <- function (food_time) {
  #converts into good format + sets 2 hours later. 
  line_start <- hm(food_time)
  raw$difference <- (as.duration(raw$time - line_start))^2
  raw[order(raw$difference),]
  closest_time <- head((raw[order(raw$difference),]), n= 1)
  return(closest_time$id)
}

#same concept as above, but finds what the glucose value was 2 hours after the food time.
#please make the "2 hours" a variable - because I can imagine instances where we want to plot
#graphs for a different window than 2 hours
find_end <- function (food_time) {
  #converts into good format + sets 2 hours later. 
  line_start <- hm(food_time) + hours(2)
  raw$difference <- (as.duration(raw$time - line_start))^2
  raw[order(raw$difference),]
  closest_time <- head((raw[order(raw$difference),]), n= 1)
  return(closest_time$id)
}

#this measures the area under the glucose curve for the 2 hour window. It's an approximation
#of the "glucose response"
area <- function (beginning, end){
  sum = round(sum(raw$bg[beginning:end] - raw$bg[beginning])/((end-beginning)/10)) #I divide by end-beginning in case different plots have a differnt number of data points per cm of line. Otherwise for the exact same curve, there will be a bigger sum if the digitization step made more points.
  return(sum)
}
plot_indiv <- function(beginning, end, score, food_name, food_time) {
  xddf <- data.frame(x=raw$id,y=raw$bg)
  bottom_of_area <- raw$bg[beginning]
  one_hour <- find_beginning("16:00")-find_beginning("15:00")
  
  xddf$title <- "title"
  p <- {ggplot() + 
    geom_line(data=xddf, aes(x, y)) +
    geom_rect(aes(xmin=-Inf, xmax=Inf, ymin=4,ymax=7), fill="green", alpha=0.2) +
      geom_rect(aes(xmin=-Inf, xmax=Inf, ymin=-Inf,ymax=4), fill="red", alpha=0.2) +
    geom_rect(aes(xmin=-Inf, xmax=Inf, ymin=7,ymax=Inf), fill="red", alpha=0.2) +
    theme_bw(base_size = 20) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=20), plot.title=element_text(size=18), legend.position="none") +
    ggtitle(paste("response = ", score, sep ="")) +
    xlab("") + ylab("mmol/L") +
    ylim( min(4, min(raw$bg[beginning:end])-1) , min(10,max(raw$bg[beginning:end])+3) ) +
    scale_x_continuous(limits=c(max(1, (beginning - one_hour)), min(nrow(raw),(end + one_hour))), breaks=c(beginning, end), labels=c(paste(" ", food_time, sep=""), "+2 hours")) +
    annotate('segment', x = beginning, xend = beginning, y =min(10,max(raw$bg[beginning:end])+3)-0.5, yend = raw$bg[beginning]+0.25, size = 1, colour = "black", alpha = 1, arrow = arrow(length = unit(0.5, "cm"))) + 
    geom_label(aes(x = beginning, y = min(10,max(raw$bg[beginning:end])+3)-0.5, label = str_trunc(wrapper(food_name, width = 30), 60, ellipsis = "..."), fill="white"), size = 8, label.padding = unit(0.5, "lines")) +
      geom_ribbon(data=subset(xddf, x>=beginning & x<=end), aes(x=x, ymin=bottom_of_area, ymax=y))
  }
  #if all the values are smaller than the orginal, light grey box and 
  #"this event didn't increase your BG"
  #if (all(raw$bg[beginning:end] <= raw$bg[beginning]) == TRUE) {
  #  p <- p + geom_ribbon(data=subset(xddf, x>=beginning & x<end), aes(x=x, ymin=-Inf, ymax=y, alpha=0.5))
  
  #if all are bigger than beginning, do the normal reponse
  #} else if (all(raw$bg[beginning:end] >= raw$bg[beginning]) == TRUE) {
  #  p <- p + geom_ribbon(data=subset(xddf, x>=beginning & x<=end), aes(x=x, ymin=bottom_of_area, ymax=y))
  #} 
  #else {
  #  first_time_below_start <- beginning -1 + min(which(raw$bg[beginning:end] < raw$bg[beginning])) 
  #  p <- p + geom_ribbon(data=subset(xddf, x>=beginning & x<first_time_below_start), 
  #                         aes(x=x, ymin=bottom_of_area, ymax=y))
  #  p <- p + geom_ribbon(data=subset(xddf, x>=first_time_below_start-1 & x<=end), 
  #                       aes(x=x, ymin=bottom_of_area, ymax=y,fill='red'))
  #}

  #this section annotates potential next foods in the 2hour window
  if (which(food_log$food_time == food_time) < nrow(food_log)) {
    time_of_next_food <- hm(food_log$food_time[which(food_log$food_time == food_time) + 1])
    index_of_next_food <- find_beginning(paste(hour(hms(time_of_next_food)), ":", minute(hms(time_of_next_food)), sep=""))
    name_of_next_food <- food_log$description[which(food_log$food_time == food_time)+1]
    
    if (time_of_next_food - hm(food_time) < hours(2)) {
      p <- p + 
        annotate('segment', x = index_of_next_food, xend = index_of_next_food, y = min(max(raw$bg[beginning], raw[index_of_next_food,]$bg)+1, 9), yend = max(raw$bg[beginning], raw[index_of_next_food,]$bg)+0.25, size = 1, colour = "black", alpha = 0.8, arrow = arrow(length = unit(0.5, "cm"))) +
        geom_text(aes(x = index_of_next_food, y = min(max(raw$bg[beginning], raw[index_of_next_food,]$bg)+1.25, 9.25), label = str_trunc(wrapper(name_of_next_food, width = 30),60, ellipsis = "..."), size = 6))
    }
  }
  p
   
}
plot_day <- function(beginning, end, score, food_name) {
  xddf <- data.frame(x=raw$id,y=raw$bg)
  bottom_of_area <- raw$bg[beginning]
  #need to plot the x axies correctly. look through the table. find nearest
  #to 9am, 12, 3pm, 6pm, 9pm. and use those values. 
  six <- find_beginning("06:00")
  twelve <- find_beginning("12:00")
  sixpm <- find_beginning("18:00")
  average <- round(mean(raw$bg), digits = 1)
  ggplot() + 
    geom_rect(aes(xmin=-Inf, xmax=Inf, ymin=4,ymax=6.9),fill="green",alpha=0.2) +
    geom_rect(aes(xmin=-Inf, xmax=Inf, ymin=6.9,ymax=Inf),fill="red",alpha=0.2) +
    #geom_rect(aes(xmin=-Inf, xmax=Inf, ymin=70,ymax=Inf), fill="green", alpha=0.2) +
    #geom_rect(aes(xmin=-Inf, xmax=Inf, ymin=-Inf,ymax=70), fill="red", alpha=0.2) +
    geom_hline(data=xddf, aes(x, y), yintercept=average, linetype="dashed", color = "dark gray", size=1) + 
    geom_line(data=xddf, aes(x, y), size=2) +
    #geom_ribbon(data=subset(xddf, x>beginning & x<end), aes(x, y, ymin=bottom_of_area, ymax=y)) +
    theme_bw(base_size = 20) + 
    theme(axis.text=element_text(size=20)) +
    ggtitle(paste(day, "|", "Average BG =", average, sep=" ")) +
    xlab("") + 
    scale_x_continuous(limits = c(raw$id[1], raw$id[nrow(raw)]), breaks=c(six, twelve, sixpm), labels=c("6am", "noon", "6pm")) +
    ylab("mmol/L") +
    scale_y_continuous(limits = c(2,11))
    #ylab("mg/dL") +
    #scale_y_continuous(limits = c(40,90))
  
  #annotate("text", -Inf, Inf, label = paste(food_name, "\n","    food line magnitude = ", score), hjust = 0, vjust = 5)
  
}
master <- function (food_time, food_name) {
  beginning <- find_beginning(food_time)
  end <- find_end(food_time)
  score <- area(beginning, end)
  print(score)
  filename_day <- (paste("~/Dropbox/Fline/Plots from 2019 pilot/", data_folder, "/", data_folder , "day", ".png", sep=""))
  plot_day(beginning, end, score, food_name) 
  ggsave(filename_day, plot = last_plot(), device = NULL, path = NULL,
         scale = 1, width = 9.07, height = 5.81, units = c("in", "cm", "mm"),
         dpi = 300, limitsize = TRUE)
  filename <- (paste("~/Dropbox/Fline/Plots from 2019 pilot/", data_folder, "/", data_folder, "", which(food_log$food_time == food_time), food_name, ".png", sep=""))
  plot_indiv(beginning, end, score, food_name, food_time) 
  ggsave(filename, plot = last_plot(), device = NULL, path = NULL,
         scale = 1, width=9.07, height = 5.81, units = c("in", "cm", "mm"),
         dpi = 300, limitsize = TRUE)
}
mapply(master, food_log$food_time, food_log$description)








#extra_subset <- subset(xddf, x>=10 & x<=20)
#geom_ribbon(data=rbind(subset(xddf, x>=beginning & x<=end), extra_subset), aes(x=x, ymin=bottom_of_area, ymax=y)) +
  



#percent_out_of_range <- function() {
  #in order to do this i also have to smooth it to be more than just datapoints
  #maybe i can create one data points per minute. 
  #let's try this digitize library, maybe it has an option to 
  #get data at regular intervals.
  #return(percent_out_range)
#}

#TO-DO
#One daily graph should have green/yellow/red colors
#All the times there was food
#and the proportion of time for each of them
#small graphs for individual foods, all same scale.
#dotted line at 2 hours

#problem to fix
#the 2 hour window is sometimes messed up because instead of really
#taking 2 hours, it works with "nearest". 
#need to find a way to make a smooth function and select from the smooth line
#instead of the actual data points.  

#digitization of images
#https://cran.r-project.org/web/packages/digitize/index.html

