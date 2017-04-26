# Created by: nazmul
# Created on: 21/04/17
# http://www.cricketabstract.com/

# Define required packages and install new packages
list.of.packages <- c("jsonlite", "lubridate", "dplyr", "ggplot2")
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
str(raw.data)


##### Task 2 - pre-processing countries #####
# Explore teams
# Subset full ICC members for both men and women teams
levels(raw.data$team_a) # 90 teams
levels(raw.data$team_b) # 95 teams
setdiff(raw.data$team_a, raw.data$team_b) # 11 teams
setdiff(raw.data$team_b, raw.data$team_a) # 16 teams

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
      gendercol[i] <- "F"
    else
      gendercol[i] <- "M"
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


##### Step 3 - filling in missing values/ignore the tuple #####
# removing no result, tied and abandoned matches
summary(raw.data.sub$outcome) # 135 no result, 92 abandoned, 33 tied
raw.data.sub <- raw.data.sub[raw.data.sub$outcome != "No result", ]
raw.data.sub <- raw.data.sub[raw.data.sub$outcome != "Match abandoned without a ball bowled", ]
raw.data.sub <- raw.data.sub[raw.data.sub$outcome != "No result (abandoned with a toss)", ]
raw.data.sub <- raw.data.sub[raw.data.sub$outcome != "Match tied", ]


# remove walkover/forfeited matches
raw.data.sub$winner <- droplevels(raw.data.sub$winner)
summary(raw.data.sub)
str(raw.data.sub)
raw.data.sub[is.na(raw.data.sub$first_innings_total), 1] # 3 walkver matches
raw.data.sub <- raw.data.sub[!is.na(raw.data.sub$first_innings_total), ]
summary(raw.data.sub)


# check other variables with empty values
summary(raw.data.sub)
str(raw.data.sub)
raw.data.sub$day_night <- as.factor(raw.data.sub$day_night)
raw.data.sub$rain <- as.factor(raw.data.sub$rain)
raw.data.sub$duckworth_lewis <- as.factor(raw.data.sub$duckworth_lewis)


levels(raw.data.sub$team_a) # 10 unique teams
levels(raw.data.sub$team_b) # 10 uniuqe teams
levels(raw.data.sub$series) # 317 unique series'
levels(raw.data.sub$venue_country) # 20 unique venues
levels(raw.data.sub$toss_winner) # 19 unique toss winners, 1 empty level
levels(raw.data.sub$winner) # 20 unique winners, 1 empty level
levels(raw.data.sub$day_night) # two binary levels of 0 and 1
levels(raw.data.sub$rain) # two binary levels of 0 and 1
levels(raw.data.sub$duckworth_lewis) # error in data
levels(raw.data.sub$outcome)

# convert date from factor to date
library(lubridate)
sapply(raw.data.sub$date, class)
raw.data.sub$date <- ymd(as.character(raw.data.sub$date))
summary(raw.data.sub$date)
str(raw.data.sub)


sum(raw.data.sub$day_night == "")
sum(raw.data.sub$toss_winner == "") # 4 empty toss winners
sum(raw.data.sub$winner == "") # 3 empty winners
sum(raw.data.sub$venue_country == "")
sum(raw.data.sub$series == "")
sum(raw.data.sub$first_innings_total == "")
sum(is.na(raw.data.sub$date))
sum(is.na(raw.data.sub$day_night))
sum(is.na(raw.data.sub$toss_winner))
sum(is.na(raw.data.sub$winner))

summary(raw.data.sub)


# find and remove empty/unknown toss winners
raw.data.sub[raw.data.sub$toss_winner == "", 1] # 4 unknown toss winners
raw.data.sub <- raw.data.sub[raw.data.sub$toss_winner != "", ]

# find and remove empty winners
raw.data.sub[raw.data.sub$winner == "", 1] # 3 matches tied (D/L method)
raw.data.sub <- raw.data.sub[raw.data.sub$winner != "", ]


# find missing toss decisions
summary(raw.data.sub$toss_decision) # 2576 missing values
raw.data.sub$toss_decision[raw.data.sub$toss_decision == ""] <- NA # set empty strings to NA
raw.data.sub <- droplevels(raw.data.sub)
summary(raw.data.sub$toss_decision)

## find toss decision values within a date range
library(dplyr)
na.toss.decision <- filter(raw.data.sub, is.na(toss_decision)) # 820 matches
prop.table(table(is.na(raw.data.sub$toss_decision), !is.na(raw.data.sub$toss_decision))) # 21%
nrow(na.toss.decision)/nrow(raw.data.sub)*100 # 20.71%

## find number of matches with missing toss decision per year
total.toss <- aggregate(cbind(id)~year(date),
          data=na.toss.decision,FUN=length)

bp <- barplot(total.toss$id, horiz = FALSE, 
        names.arg = total.toss$`year(date)`,
        ylim = c(0, 120),
        main = "Number of empty toss decisions by year",
        xlab = "Year",
        ylab = "No of missing toss decisions")
text(x = bp, y = total.toss$id, label = total.toss$id, pos = 4, cex = 0.8, col = "red")


## remove missing toss decision observations
raw.data.sub <- filter(raw.data.sub, !is.na(toss_decision))

summary(raw.data.sub)



# Set D/L method values
# duckworth lewis matches have error values
# does not have 1 assigned to matches played in D/L method
table(raw.data.sub$duckworth_lewis, grepl('D/L method', raw.data.sub$outcome)) # 96 matches

raw.data.sub$duckworth_lewis <-
  mapply(grepl,
         pattern = "D/L method",
         x = raw.data.sub$outcome)

set.duckworth.lewis= function(dlcol) {
  for (i in 1:length(dlcol)) {
    if (dlcol[i] == TRUE)
      dlcol[i] <- 1
    else
      dlcol[i] <- 0
  }
  dlcol
}

raw.data.sub$duckworth_lewis <- set.duckworth.lewis(raw.data.sub$duckworth_lewis)
raw.data.sub$duckworth_lewis <- as.factor(raw.data.sub$duckworth_lewis)
summary(raw.data.sub$duckworth_lewis) # 0: 3043, 1: 96
prop.table(table(raw.data.sub$duckworth_lewis))




##### Step 4 - altering values #####
# rename toss winner and winner abbreviations to country names
levels(raw.data.sub$toss_winner)
raw.data.sub$toss_winner<- sub("AUS", "Australia", raw.data.sub$toss_winner)
raw.data.sub$toss_winner<- sub("AUSWN", "Australia", raw.data.sub$toss_winner)
raw.data.sub$toss_winner<- sub("AustraliaWN", "Australia", raw.data.sub$toss_winner)
raw.data.sub$toss_winner<- sub("BD-W", "Bangladesh", raw.data.sub$toss_winner)
raw.data.sub$toss_winner<- sub("BDESH", "Bangladesh", raw.data.sub$toss_winner)
raw.data.sub$toss_winner<- sub("ENG", "England", raw.data.sub$toss_winner)
raw.data.sub$toss_winner<- sub("ENGWN", "England", raw.data.sub$toss_winner)
raw.data.sub$toss_winner<- sub("EnglandWN", "England", raw.data.sub$toss_winner)
raw.data.sub$toss_winner<- sub("IND-W", "India", raw.data.sub$toss_winner)
raw.data.sub$toss_winner<- sub("INDIA", "India", raw.data.sub$toss_winner)
raw.data.sub$toss_winner<- sub("NZ", "New Zealand", raw.data.sub$toss_winner)
raw.data.sub$toss_winner<- sub("NZWN", "New Zealand", raw.data.sub$toss_winner)
raw.data.sub$toss_winner<- sub("New ZealandWN", "New Zealand", raw.data.sub$toss_winner)
raw.data.sub$toss_winner<- sub("PAK", "Pakistan", raw.data.sub$toss_winner)
raw.data.sub$toss_winner<- sub("PAK-W", "Pakistan", raw.data.sub$toss_winner)
raw.data.sub$toss_winner<- sub("Pakistan-W", "Pakistan", raw.data.sub$toss_winner)
raw.data.sub$toss_winner<- sub("SA", "South Africa", raw.data.sub$toss_winner)
raw.data.sub$toss_winner<- sub("SA-W", "South Africa", raw.data.sub$toss_winner)
raw.data.sub$toss_winner<- sub("South Africa-W", "South Africa", raw.data.sub$toss_winner)
raw.data.sub$toss_winner<- sub("SL", "Sri Lanka", raw.data.sub$toss_winner)
raw.data.sub$toss_winner<- sub("SL-W", "Sri Lanka", raw.data.sub$toss_winner)
raw.data.sub$toss_winner<- sub("Sri Lanka-W", "Sri Lanka", raw.data.sub$toss_winner)
raw.data.sub$toss_winner<- sub("WI", "West Indies", raw.data.sub$toss_winner)
raw.data.sub$toss_winner<- sub("WIWN", "West Indies", raw.data.sub$toss_winner)
raw.data.sub$toss_winner<- sub("West IndiesWN", "West Indies", raw.data.sub$toss_winner)
raw.data.sub$toss_winner<- sub("ZIM", "Zimbabwe", raw.data.sub$toss_winner)
raw.data.sub$toss_winner <- as.factor(raw.data.sub$toss_winner)
raw.data.sub <- droplevels(raw.data.sub)
levels(raw.data.sub$toss_winner) # 10 unique countries, 1 empty level
sum(raw.data.sub$toss_winner == "") # 4 empty toss winners


levels(raw.data.sub$winner)
raw.data.sub$winner<- sub("AUS", "Australia", raw.data.sub$winner)
raw.data.sub$winner<- sub("AUSWN", "Australia", raw.data.sub$winner)
raw.data.sub$winner<- sub("AustraliaWN", "Australia", raw.data.sub$winner)
raw.data.sub$winner<- sub("BD-W", "Bangladesh", raw.data.sub$winner)
raw.data.sub$winner<- sub("BDESH", "Bangladesh", raw.data.sub$winner)
raw.data.sub$winner<- sub("ENG", "England", raw.data.sub$winner)
raw.data.sub$winner<- sub("ENGWN", "England", raw.data.sub$winner)
raw.data.sub$winner<- sub("EnglandWN", "England", raw.data.sub$winner)
raw.data.sub$winner<- sub("IND-W", "India", raw.data.sub$winner)
raw.data.sub$winner<- sub("INDIA", "India", raw.data.sub$winner)
raw.data.sub$winner<- sub("NZ", "New Zealand", raw.data.sub$winner)
raw.data.sub$winner<- sub("NZWN", "New Zealand", raw.data.sub$winner)
raw.data.sub$winner<- sub("New ZealandWN", "New Zealand", raw.data.sub$winner)
raw.data.sub$winner<- sub("PAK", "Pakistan", raw.data.sub$winner)
raw.data.sub$winner<- sub("PAK-W", "Pakistan", raw.data.sub$winner)
raw.data.sub$winner<- sub("Pakistan-W", "Pakistan", raw.data.sub$winner)
raw.data.sub$winner<- sub("SA", "South Africa", raw.data.sub$winner)
raw.data.sub$winner<- sub("SA-W", "South Africa", raw.data.sub$winner)
raw.data.sub$winner<- sub("South Africa-W", "South Africa", raw.data.sub$winner)
raw.data.sub$winner<- sub("SL", "Sri Lanka", raw.data.sub$winner)
raw.data.sub$winner<- sub("SL-W", "Sri Lanka", raw.data.sub$winner)
raw.data.sub$winner<- sub("Sri Lanka-W", "Sri Lanka", raw.data.sub$winner)
raw.data.sub$winner<- sub("WI", "West Indies", raw.data.sub$winner)
raw.data.sub$winner<- sub("WIWN", "West Indies", raw.data.sub$winner)
raw.data.sub$winner<- sub("West IndiesWN", "West Indies", raw.data.sub$winner)
raw.data.sub$winner<- sub("ZIM", "Zimbabwe", raw.data.sub$winner)
raw.data.sub$winner <- as.factor(raw.data.sub$winner)
raw.data.sub <- droplevels(raw.data.sub)
levels(raw.data.sub$winner) # 10 unique countries, 1 empty level
sum(raw.data.sub$winner == "") # 3 empty winners

summary(raw.data.sub)


# compute series in categories low < medium < high
levels(raw.data.sub$series)
summary(raw.data.sub$series)
raw.data.sub$series <- as.character(raw.data.sub$series)

set.series.significance <- function(seriescol, match, replace) {
  for (i in 1:length(seriescol)) {
    if (grepl(match, seriescol[i]))
      seriescol[i] <- replace
  }
  seriescol
}

## set all the high importance series
## World Cup, Champions Trophy, Women's Championship
raw.data.sub$series <- set.series.significance(raw.data.sub$series, "World Cup", "high")
raw.data.sub$series <- set.series.significance(raw.data.sub$series, "Champions Trophy", "high")
raw.data.sub$series <- set.series.significance(raw.data.sub$series, "Women's Championship", "high")



## set all the medium importance series
## wills trophy, Asia Cup, NatWest Series
raw.data.sub$series <- set.series.significance(raw.data.sub$series, "Asia Cup", "medium")
raw.data.sub$series <- set.series.significance(raw.data.sub$series, "NatWest Series", "medium")
raw.data.sub$series <- set.series.significance(raw.data.sub$series, "Pakistan in India", "medium")
raw.data.sub$series <- set.series.significance(raw.data.sub$series, "India in Pakistan", "medium")



## set all the low importance series
## Bangladesh in, in Bangladesh, 
raw.data.sub$series <- set.series.significance(raw.data.sub$series, "Bangladesh in", "low")
raw.data.sub$series <- set.series.significance(raw.data.sub$series, "in Bangladesh", "low")





raw.data.sub$series <- as.factor(raw.data.sub$series)
raw.data.sub <- droplevels(raw.data.sub)
levels(raw.data.sub$series)
summary(raw.data.sub$series)





# compute away teams - neutral venues are away for both teams







