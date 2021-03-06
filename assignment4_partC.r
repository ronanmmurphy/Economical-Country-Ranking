---
title: "partC"
author: "Ronan Murphy"
date: "3/20/2020"
output: html_document
---

#```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(ggplot2)
library(countrycode)
#```



#```{r part3}
#import csv file
data <- read.csv("OECD_PISA.csv")

#extract the data that contrains total values
data <- subset(data, SUBJECT == "TOT")
#remove rows, keep Location, Time and Value
data <- select (data,-c(2, 3, 4, 5, 8))

#convert the countries from 3 letter to full name
data[[1]] <- countrycode(data[[1]], "iso3c", "country.name")

#change the location column name
names(data)[1] <- "Location"

#change the name of United Kingdom to UK
data$Location <- as.character(data$Location)
data$Location[data$Location == "United Kingdom"] <- "UK"

#take a subset of the data for the 5 countries with highest GDP in Europe
data <-
  subset(data,
         Location %in% c("Italy", "France", "Spain", "UK", "Germany"))

#convert the time data so it wont be read as continuous
data$TIME <- as.character(data$TIME)
data$TIME[data$TIME == "2000"] <- " 2000"
data$TIME[data$TIME == "2003"] <- " 2003"
data$TIME[data$TIME == "2006"] <- " 2006"
data$TIME[data$TIME == "2009"] <- " 2009"
data$TIME[data$TIME == "2012"] <- " 2012"
data$TIME[data$TIME == "2015"] <- " 2015"
data$TIME[data$TIME == "2018"] <- " 2018"
data

#set the colours from colour blind friendly palette that associate with each country
Loc_colours = (values = c("#0072b2", "#854484", "#009e73",	"#f0e442",  "#d55e00"))
#```


#```{r fig.width=10, fig.height=6}
#^adjust the size of plot so it will fit all the values

#create the ggplot plotting x to TIMe and location combination, and y-axis to Value
p <- ggplot(data, aes(x = interaction(TIME, Location), y = Value)) +
  #create a bar chart with geom_col
  geom_col(
    #the fill of the bar is based on the location, width 0.5 and seperated by 0.5
    aes(fill = Location),
    width = 0.5,
    #seperate the groups by location
    position = position_dodge(width = 0.5),
    stat = "identity"
  ) +
  
  #expand the discrete x-axis on left and right
  scale_x_discrete(expand = expand_scale(add = c(1, 1))) +
  #set the fill of the bars to the colours defined above
  scale_fill_manual(values = Loc_colours) +
  
  #set title for plot
  ggtitle("OECD Reading scores - Top 5 GDP Countries Europe") +
  
  #create y-axis limits for the plot to make bars easier to compare
  coord_cartesian(ylim = c(450, 520)) +
  
  #add white hline to intercept the bar
  geom_hline(yintercept = c(460, 480, 500),
             color = "white",
             size = 0.5) +
  #create annotation for the years placing it at the bottom of the plot, rotate 45 degrees
  annotate(
    geom = "text",
    x = seq_len(nrow(data)),
    y = 449,
    label = data$TIME,
    size = 4,
    angle = 45
  ) +
  
  #make annotation for the Location, colour set to the location colours on top of the bars
  #seperate for each bar group
  annotate(
    geom = "text",
    x = 4 + 6.5 * (0:4),
    y = 515,
    label = unique(data$Location),
    size = 6,
    color = Loc_colours
  ) +
  
  theme(
    #remove the axis titles, ticks and text on the x-axis
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    
    #set the background colour to same as previous
    panel.background = element_rect(fill = "#E2EDF3"),
    #remove all gridlines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    #set border on the plot of same colour as background
    panel.border = element_rect(
      colour = "#E2EDF3",
      fill = NA,
      size = 1
    ),
    
    #set text colour and size of legend
    legend.text = element_text(size = 9, colour = '#406D89'),
    #move legend to the bottom left and remove title, same as part B
    legend.justification = "left",
    legend.position = "bottom",
    legend.title = element_blank()
  )
p

#```

