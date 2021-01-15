---
title: "partb"
author: "Ronan Murphy"
date: "3/10/2020"
output: html_document
---

#```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(ggplot2)
library(countrycode)
library(gghighlight)
#```


#```{r part1}
#import the csv file
data <- read.csv("OECD_PISA.csv")

#remove all rows with TOT subject as only need boy and girl data, and rows that arent 2018
data <- subset(data, SUBJECT != "TOT" & TIME == "2018")
#select the columns for subject, location and value only
data <- select (data, -c(2, 4, 5, 6, 8))

#use the countrycode library to convert the 3 letter abbreviations into full names
data[[1]] <- countrycode(data[[1]], "iso3c", "country.name")
#change the name of location column to more readable format
names(data)[1] <- "Location"

#convert the OECD_AVG data as it isnt in the country code library
data$Location <- as.character(data$Location)
data$Location[is.na(data$Location)] <- "OECD - Average"

#convert the names of BOY and GIRL to format required for legend
data$SUBJECT <- as.character(data$SUBJECT)
data$SUBJECT[data$SUBJECT == "BOY"] <- "Boys"
data$SUBJECT[data$SUBJECT == "GIRL"] <- "Girls"

#rearrange the data in order of Value this will make it easier instead of reordering later
data <- data %>% arrange(SUBJECT, Value) %>%
  mutate(Location = factor(Location, levels = unique(Location)))

#set the text colour to grey colour and Red if Location is Ireland
text_colour <-
  ifelse(data$Location == "Ireland", "#EA272C", "#8A8686")

#set the points colour to blue unless Ireland is location then set to red
point_colour <-
  ifelse(data$Location == "Ireland", '#EA272C', '#406D89')

#set fill to Background colour unless Ireland set to red
point_fill <-
  ifelse(data$Location == "Ireland", '#EA272C', "#E2EDF3")

#```


#```{r fig.width=10, fig.height=6}
#^adjust the size of plot so it will fit all the values

#create ggplot based on Location and value, shape is dependant on SUBJECT
p <- ggplot(data , aes(x = Location, y = Value, shape = SUBJECT)) +
  
  #set the shape of the objects for boy and girl and circle and diamond respectively
  scale_shape_manual(values = c(16, 23)) +
  #set the scale of x and y axis
  #y-axis iterates in 20s from 340 to 560, expand the limits to include space at top and bottom
  scale_y_continuous(
    limits = c(330, 570),
    expand = c(0, 0),
    breaks = c(340, 360, 380, 400, 420, 440, 460, 480, 500, 520, 540, 560)
  ) +
  
  #expand x axis to include space at left and right
  scale_x_discrete(expand = expand_scale(add = c(2.5, 1.5))) +
  
  #create white segement between x-axis and the bottom point
  geom_segment(aes(
    x = Location,
    y = 340,
    xend = Location,
    yend = Value
  ), color = "white") +
  
  
  #create grey line connecting boy and girl points for each Location
  geom_line(aes(group = Location), colour = "#8A8686") +
  #create points of size 3, colour and fill depends on Location and outline is 1
  geom_point(
    size = 3,
    color = point_colour,
    fill = point_fill,
    stroke = 1
  ) +
  
  theme(
    #set background colour to light blue and remove x-major gridlines and all minor gridlines
    panel.background = element_rect(fill = "#E2EDF3"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    
    #remove axis titles
    axis.title = element_blank(),
    #rotate x-axis tet to 45 degrees horizontally adjusing 1.2 and colour depends on Location
    axis.text.x = element_text(
      angle = 45,
      size = 7,
      hjust = 1.2,
      color = text_colour
    ),
  
    #y-axis text adjusted onto the plot slightly bigger and constant colour grey,
    #move margin onto the graph -0.8 cm to the right
    axis.text.y = element_text(
      size = 9,
      margin = margin(r = -0.8, unit = "cm"),
      vjust = -0.6,
      colour = "#8A8686"
    ),
    #remove all axis ticks
    axis.ticks = element_blank(),
    
    #change the size and colour of legend text
    legend.text = element_text(size = 9, colour = '#406D89'),
    #move the legend to the bottom left
    legend.justification = "left",
    legend.position = "bottom",
    #remove legend title
    legend.title = element_blank(),
    #make the background colour of legend to white, removes the outside box around object
    legend.key = element_rect(colour = NA, fill = NA),
    legend.background = element_rect(colour = "white", fill = "white"),
  )
p

#```
