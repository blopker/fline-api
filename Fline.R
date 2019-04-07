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

#   This program takes one day's digitized glucose data, the user's food log for that day,
#   and returns individual plots for each of the food entries for that day
#   and a full day plot. 

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

#this function takes in the time of a food from "food_log", and finds in "raw" for what the user's
#glucose was at that time. it returns the row number of the glucose value.
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
#"2 hours" should be a variable - because I can imagine instances where we want to plot
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

#plot_indiv plots a graph for each individual foods in that day's food log
#in "master" function below, "beginning" and "end" are created and passed to plot_indiv
plot_indiv <- function(beginning, end, score, food_name, food_time) {
  
  #xddf is the glucose data without the time column. I use row ids to plot, instead of time
  #because time is a total hassle. Later I add to the graph some x-axis labels that make it look
  #like i was plotting time all along. 
  xddf <- data.frame(x=raw$id,y=raw$bg)
  
  #bottom_of_area is the y value I use to draw the area under the curve. It's the glucose value 
  #at the beginning of the food
  bottom_of_area <- raw$bg[beginning]
  
  #for each day, the number of rows in the glucose data may vary - so i check how many rows represent
  #one hour. This is because when I plot the graph i want to pad the x limits with one hour on each end
  one_hour <- find_beginning("16:00")-find_beginning("15:00")
  
  #define the plot function
  p <- {
    #call the main plotting function from ggplot
    ggplot() + 
      
    #plot the glucose line
    geom_line(data=xddf, aes(x, y)) +
      
    #draw the background green and red areas
    geom_rect(aes(xmin=-Inf, xmax=Inf, ymin=4,ymax=7), fill="green", alpha=0.2) +
    geom_rect(aes(xmin=-Inf, xmax=Inf, ymin=-Inf,ymax=4), fill="red", alpha=0.2) +
    geom_rect(aes(xmin=-Inf, xmax=Inf, ymin=7,ymax=Inf), fill="red", alpha=0.2) +
      
    #remove the default background grid, change labels, and add title
    theme_bw(base_size = 20) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=20), plot.title=element_text(size=18), legend.position="none") +
    ggtitle(paste("response = ", score, sep ="")) +
    xlab("") + ylab("mmol/L") +
      
    #depending on the particular food response it's plotting, adjust the y axis so that the whole
    #curve always shows
    ylim( min(4, min(raw$bg[beginning:end])-1) , min(10,max(raw$bg[beginning:end])+3) ) +
      
    #add one hour before and one hour after the 2 hour window, so we get more context on the glucose
    scale_x_continuous(limits=c(max(1, (beginning - one_hour)), min(nrow(raw),(end + one_hour))), breaks=c(beginning, end), labels=c(paste(" ", food_time, sep=""), "+2 hours")) +
    
    #add the arrow pointing to the beginning
    annotate('segment', x = beginning, xend = beginning, y =min(10,max(raw$bg[beginning:end])+3)-0.5, yend = raw$bg[beginning]+0.25, size = 1, colour = "black", alpha = 1, arrow = arrow(length = unit(0.5, "cm"))) + 
    
    #add the red text box with the name of the food that points to the beginning time
    geom_label(aes(x = beginning, y = min(10,max(raw$bg[beginning:end])+3)-0.5, label = str_trunc(wrapper(food_name, width = 30), 60, ellipsis = "..."), fill="white"), size = 8, label.padding = unit(0.5, "lines")) +
    
    #fill in the area under the curve in black
    geom_ribbon(data=subset(xddf, x>=beginning & x<=end), aes(x=x, ymin=bottom_of_area, ymax=y))
  }

  #in case another food is eaten within the 2 hour window, add a mention of it to the plot
  
  #only check if the food is not the last food in food log
  if (which(food_log$food_time == food_time) < nrow(food_log)) {
    #find the start time of the next food
    time_of_next_food <- hm(food_log$food_time[which(food_log$food_time == food_time) + 1])
    #find the beginning time of the next food
    index_of_next_food <- find_beginning(paste(hour(hms(time_of_next_food)), ":", minute(hms(time_of_next_food)), sep=""))
    #find the name of the next food
    name_of_next_food <- food_log$description[which(food_log$food_time == food_time)+1]
    
    #check that this next food is within 2 hours of the original food
    if (time_of_next_food - hm(food_time) < hours(2)) {
      p <- p + 
        #add an arrow and a text box
        annotate('segment', x = index_of_next_food, xend = index_of_next_food, y = min(max(raw$bg[beginning], raw[index_of_next_food,]$bg)+1, 9), yend = max(raw$bg[beginning], raw[index_of_next_food,]$bg)+0.25, size = 1, colour = "black", alpha = 0.8, arrow = arrow(length = unit(0.5, "cm"))) +
        geom_text(aes(x = index_of_next_food, y = min(max(raw$bg[beginning], raw[index_of_next_food,]$bg)+1.25, 9.25), label = str_trunc(wrapper(name_of_next_food, width = 30),60, ellipsis = "..."), size = 6))
    }
  }
  #finally, plot it all ! 
  p
   
}

#plots the full day's glucose data
plot_day <- function(beginning, end, score, food_name) {
  xddf <- data.frame(x=raw$id,y=raw$bg)
  bottom_of_area <- raw$bg[beginning]
  #the x axis is currently plotted based on row index.
  #i need to annotate it with actual times of the day.
  #find nearest row index of 6am, noon, 6pm - we will use them below.
  six <- find_beginning("06:00")
  twelve <- find_beginning("12:00")
  sixpm <- find_beginning("18:00")
  
  #find that day's glucose average because we'll mention it in the plot title
  average <- round(mean(raw$bg), digits = 1)
  
  ggplot() + 
    #add background green and red
    geom_rect(aes(xmin=-Inf, xmax=Inf, ymin=4,ymax=6.9),fill="green",alpha=0.2) +
    geom_rect(aes(xmin=-Inf, xmax=Inf, ymin=6.9,ymax=Inf),fill="red",alpha=0.2) +
    #add average line
    geom_hline(data=xddf, aes(x, y), yintercept=average, linetype="dashed", color = "dark gray", size=1) + 
    #plot the glucose
    geom_line(data=xddf, aes(x, y), size=2) +
    theme_bw(base_size = 20) + 
    theme(axis.text=element_text(size=20)) +
    ggtitle(paste(day, "|", "Average BG =", average, sep=" ")) +
    xlab("") + 
    #here is where i annotate the x axis with the times
    scale_x_continuous(limits = c(raw$id[1], raw$id[nrow(raw)]), breaks=c(six, twelve, sixpm), labels=c("6am", "noon", "6pm")) +
    ylab("mmol/L") +
    scale_y_continuous(limits = c(2,11))
}

#master saves the individual graph for each food to the folder, and also saves the daily graph 
#to that folder (it's redundant because it saves the daily graph each time)
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

#this is where the magic happens - for each row in food_log, runs master
mapply(master, food_log$food_time, food_log$description)






#TO-DO

#In the daily graph, color the area under the curve for the 2h windows after each food

#Automate heuristics
#if all the values are smaller than the orginal, light grey box and 
#"this event didn't increase your BG"
#if all are bigger than beginning, do the normal reponse


#Percent out of range
#give the % out of target range

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