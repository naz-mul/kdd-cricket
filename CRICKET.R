# Created by: nazmul
# Created on: 21/04/17
# http://www.cricketabstract.com/

# Define required packages and install new packages
list.of.packages <- c("jsonlite", "lubridate", "dplyr")
new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
(if (length(new.packages))
  install.packages(new.packages))


# Set working directory and import raw data
setwd("~/Desktop/kdd-cricket")
raw.data <- read.csv("2-target-data/cricinfo_odi_data.csv")

# Find the missing odi matches
# The missing matches are still to be played as of 23rd April 2017
library(jsonlite)
json <- fromJSON("1-data-sources/odi.json")
json <- as.vector(as.integer(json))
setdiff(json, raw.data$id)

# find the number of duplicated ids
dim(raw.data[duplicated(raw.data$id),])[1]

# Explore raw dataset
summary(raw.data)

##### Task 2 - pre-processing countries #####
# Explore teams
# Subset full ICC members for both men and women teams
levels(raw.data$team_a) # 90 teams
levels(raw.data$team_b) # 95 teams
setdiff(raw.data$team_a, raw.data$team_b)
setdiff(raw.data$team_b, raw.data$team_a)

library(dplyr)
full_members <- c("Australia", "Australia Women","Bangladesh", "Bangladesh Women", "England", "England Women", "India", "India Women", "New Zealand", "New Zealand Women", "Pakistan", "Pakistan Women", "South Africa", "South Africa Women","Sri Lanka", "Sri Lanka Women","West Indies", "West Indies Women","Zimbabwe", "Zimbabwe Women")
raw.data.sub <- filter(raw.data, team_a %in% full_members & team_b %in% full_members)
raw.data.sub <- droplevels(raw.data.sub) # 4231 observations
levels(raw.data.sub$team_a)
levels(raw.data.sub$team_b)
(nrow(raw.data.sub)/nrow(raw.data)*100) # 65.12% full ICC teams

# Create a new column for gender
raw.data.sub$gender <- NA
raw.data.sub$gender <-
  mapply(grepl,
         pattern = "Women",
         x = raw.data.sub$team_a)

set.gender = function(gendercol) {
  for (i in 1:length(gendercol)) {
    if (gendercol[i] == TRUE)
      gendercol[i] <- "female"
    else
      gendercol[i] <- "male"
  }
  gendercol
}

# set gender for each teams
raw.data.sub$gender <- set.gender(raw.data.sub$gender)
raw.data.sub$gender <- as.factor(raw.data.sub$gender)
summary(raw.data.sub$gender) # women: 796, men: 3435
prop.table(table(raw.data.sub$gender)) # 19 vs 81


# truncate 'women' from tream names
raw.data.sub$team_a<- sub(" Women", "", raw.data.sub$team_a)
raw.data.sub$team_b<- sub(" Women", "", raw.data.sub$team_b)
raw.data.sub$team_a <- as.factor(raw.data.sub$team_a)
raw.data.sub$team_b <- as.factor(raw.data.sub$team_b)
raw.data.sub <- droplevels(raw.data.sub)
sapply(raw.data.sub$team_a, class)
levels(raw.data.sub$team_b)
levels(raw.data.sub$team_b)


##### Step 3 - removing no result and abandoned matches #####
summary(raw.data.sub$outcome) # 135 no result, 92 abandoned, 33 tied
raw.data.sub <- raw.data.sub[raw.data.sub$outcome != "No result", ]
raw.data.sub <- raw.data.sub[raw.data.sub$outcome != "Match abandoned without a ball bowled", ]

# set TIE to winner variable for all the tied matches
raw.data.sub$winner <- as.character(raw.data.sub$winner)
raw.data.sub$winner[raw.data.sub$outcome == "Match tied"] <- as.character("TIE")
raw.data.sub$winner <- as.factor(raw.data.sub$winner)
sum(raw.data.sub$winner == "") # 5 empty winners values
sum(raw.data.sub$toss_winner == "") # 7 empty toss winner values
summary(raw.data.sub$winner)

# first innings total
raw.data.sub <- droplevels(raw.data.sub)
summary(raw.data.sub)
raw.data.sub[raw.data.sub$first_innings_total == 0, 1] # 66310, 251493 and 3 NA
raw.data.sub[raw.data.sub$id == c(66310, 251493), ]
raw.data.sub[is.na(raw.data.sub$first_innings_total), 1]










# find missing toss decisions
summary(raw.data$toss_decision) # 2576 missing values
raw.data$toss_decision[raw.data$toss_decision == ""] <- NA # set empty strings to NA
raw.data <- droplevels(raw.data)
summary(raw.da1ta$toss_decision)


# find the date range of the cricket matches
library(lubridate)
sapply(raw.data$date, class)
raw.data$date <- ymd(as.character(raw.data$date))
summary(raw.data$date)

# find toss decision values within a date range
library(dplyr)
na.toss.decision <- filter(raw.data, is.na(toss_decision))
min(na.toss.decision$date) # minimum date toss decision is NA
na.toss.decision[na.toss.decision$date == "1973-06-20",] # find the row with minimum date
max(na.toss.decision$date)
