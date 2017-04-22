# Created by: nazmul
# Created on: 21/04/17
# http://www.cricketabstract.com/

# Define required packages and install new packages
list.of.packages <- c("jsonlite", "lubridate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


setwd("~/Desktop/kdd-cricket")
raw.data <- read.csv("2-target-data/cricinfo_odi_data.csv")

# find the date range of the cricket matches
library(lubridate)
sapply(raw.data$date, class)
raw.data$date <- ymd(as.character(raw.data$date))
summary(raw.data$date)

# Find the missing odi matches
library(jsonlite)
json <- fromJSON("1-data-sources/odi.json")
json <- as.vector(as.integer(json))
ids <- raw.data$id
ids <- as.vector(ids)

setdiff(json, ids)

# find the number of duplicated ids
dim(raw.data[duplicated(raw.data$id),])[1]


# Create a new column for gender
raw.data$gender <- "male"

raw.data$gender <- mapply(grepl, pattern="Women", x= raw.data$team_a)


