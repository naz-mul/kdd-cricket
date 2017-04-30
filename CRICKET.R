# Define required packages and install new packages
dm.required.packages <- c("lubridate", "dplyr", "ggplot2", "scales")

package.install.func <- function(x) {
  for (i in x) {
    #  require returns TRUE invisibly if it was able to load package
    if (!require(i , character.only = TRUE)) {
      #  If package was not able to be loaded then re-install
      install.packages(i , dependencies = TRUE)
      #  Load package after installing
      require(i , character.only = TRUE)
    }
  }
}

package.install.func(dm.required.packages)


# Set working directory and import raw data
setwd("~/Desktop/kdd-cricket")

# Source
source("PREPROCESSING.R")


##### Summarising Data - Data Exploration #####
cricket <- read.csv("3-transformed-data/cric_transformed_odi.csv")
str(cricket)

# set factor variables and level orders
cricket$day_night <- as.factor(cricket$day_night)
cricket$rain <- as.factor(cricket$rain)
cricket$duckworth_lewis <- as.factor(cricket$duckworth_lewis)
cricket$series <-
  factor(raw.data.sub$series, c("low", "medium", "high"))
library(lubridate)
cricket$date <- ymd(as.character(cricket$date))

# see what data looks like
dim(cricket)
names(cricket)
str(cricket)
summary(cricket)

# avoid printing whole data frame
class(cricket)
library(dplyr)
cricket <- tbl_df(cricket)
class(cricket)
cricket


# review and observations on the data set
head(cricket)
tail(cricket)
str(cricket)

# check the summary of the data
summary(cricket)


# men vs women odi matches
boxplot(first_innings_total ~ gender,
        data = cricket,
        names = c("Female", "Male"))


# Proportion of tosses won in home country
# Proportion of tosses won when playing away from home
homet <- with(cricket[cricket$team_a_venue == "home",], xtabs( ~ team_a_venue + toss_decision))
homet/rowSums(homet) # 53% bat, 47% bowl
awayt <- with(cricket[cricket$team_a_venue == "away",], xtabs( ~ team_a_venue + toss_decision))
awayt/rowSums(awayt) # 55% bat, 45% bowl

# Proportion of toss decision when played home and D/N match
homet.dn <- with(cricket[cricket$team_a_venue == "home" & cricket$day_night == 1,], xtabs( ~ team_a_venue + toss_decision))
homet.dn/rowSums(homet.dn) # 72% bat, 28% bowl
awayt.dn <- with(cricket[cricket$team_a_venue == "away" & cricket$day_night == 0,], xtabs( ~ team_a_venue + toss_decision))
awayt.dn/rowSums(awayt.dn) # 47% bat, 53% bowl


# Proportion of toss decision when played home and rain
homet.r <- with(cricket[cricket$team_a_venue == "home" & cricket$rain == 1,], xtabs( ~ team_a_venue + toss_decision))
homet.r/rowSums(homet.r) # 43% bat, 57% bowl
awayt.r <- with(cricket[cricket$team_a_venue == "away" & cricket$rain == 0,], xtabs( ~ team_a_venue + toss_decision))
awayt.r/rowSums(awayt.r) # 55% bat, 45% bowl

# Proportion of toss decision when played home, day night and rain
homet.dn.r <- with(cricket[cricket$team_a_venue == "home" & cricket$day_night == 1 & cricket$rain == 1,], xtabs( ~ team_a_venue + toss_decision))
homet.dn.r/rowSums(homet.dn.r) # 52% bat, 48% bowl
awayt.dn.r <- with(cricket[cricket$team_a_venue == "away" & cricket$day_night == 0 & cricket$rain == 0,], xtabs( ~ team_a_venue + toss_decision))
awayt.dn.r/rowSums(awayt.dn.r) # 47% bat, 53% bowl



# Win toss win game







# Home match better probablity of winniing
library(dplyr)
sel_cricket <-
  select(.data = cricket, team_a, team_b, winner, toss_winner, team_a_venue)
sel_cricket$team_a <- as.character(sel_cricket$team_a)
sel_cricket$team_b <- as.character(sel_cricket$team_b)
sel_cricket$winner <- as.character(sel_cricket$winner)
sel_cricket$toss_winner <- as.character(sel_cricket$toss_winner)

str(sel_cricket)

homewin <-
  (
    sel_cricket %>%
      filter(team_a_venue == "home") %>%
      group_by(team_a, team_b) %>%
      summarise(won = sum(team_a == winner), total = n()) %>%
      mutate(percent = (won / total) * 100) %>%
      arrange(team_a, team_b)
  )

awaywin <-
  (
    sel_cricket %>%
      filter(team_a_venue == "away") %>%
      group_by(team_a, team_b) %>%
      summarise(won = sum(team_a == winner), total = n()) %>%
      mutate(percent = (won / total) * 100) %>%
      arrange(team_a, team_b)
  )

splitted.data.home <- split(homewin, homewin$team_a)
splitted.data.away <- split(awaywin, awaywin$team_a)
fill <- c("#4f90c1", "#d3c8c8")
par(mfrow = c(1, 1))

library(ggplot2)
library(scales)
for (country in levels(cricket$team_a)) {
  chome <- splitted.data.home[country]
  coutside <- splitted.data.away[country]
  title <- paste(country, "vs Rest")
  
  
  dat1 <-
    data.frame(
      type = "Home",
      percent = chome[[1]]$percent,
      opponent = chome[[1]]$team_b,
      total = chome[[1]]$total
    )
  dat2 <-
    data.frame(
      type = "Away",
      percent = coutside[[1]]$percent,
      opponent = coutside[[1]]$team_b,
      total = coutside[[1]]$total
    )
  dat <- rbind(dat1, dat2)
  
  p4 <- ggplot() + theme_bw() + geom_bar(aes(y = percent, x = opponent, fill = type),
                                        data = dat,
                                        stat = "identity")
  p4 <-
    p4 + theme(axis.text.x = element_text(angle = 70, hjust = 1))
  
  p4 <-
    p4 + geom_text(data = dat,
                   aes(
                     x = opponent,
                     y = percent,
                     label = paste0(round(percent,
                                          2), "%")
                   ),
                   size = 3, 
                   position = position_stack(vjust = 0.5))
  
  p4 <-
    p4 + theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_blank()
    )
  
  p4 <-
    p4 + ggtitle(title) + labs(y = "Win percent") + scale_y_continuous(labels = dollar_format(suffix = "%",
                                                                                              prefix = ""))
  p4 <- p4 + scale_fill_manual(values = fill)
  print(p4)
  
}

# Percentage of Team vs Opponent MEAN HOME win
mean(homewin$percent[homewin$team_a == "Australia"]) # 73.89%
mean(homewin$percent[homewin$team_a == "Bangladesh"]) # 13.64%
mean(homewin$percent[homewin$team_a == "England"]) # 64.71%
mean(homewin$percent[homewin$team_a == "India"]) # 63.13%
mean(homewin$percent[homewin$team_a == "New Zealand"]) # 66.94%
mean(homewin$percent[homewin$team_a == "Pakistan"]) # 62.65%
mean(homewin$percent[homewin$team_a == "South Africa"]) # 75.02%
mean(homewin$percent[homewin$team_a == "Sri Lanka"]) # 69.14%
mean(homewin$percent[homewin$team_a == "West Indies"]) # 66.16%
mean(homewin$percent[homewin$team_a == "Zimbabwe"]) # 21.33 %


# Percentage of Team vs Opponent MEAN AWAY win
mean(awaywin$percent[awaywin$team_a == "Australia"]) # 78.62%
mean(awaywin$percent[awaywin$team_a == "Bangladesh"]) # 14.48%
mean(awaywin$percent[awaywin$team_a == "England"]) # 54.93%
mean(awaywin$percent[awaywin$team_a == "India"]) # 55.55%
mean(awaywin$percent[awaywin$team_a == "New Zealand"]) # 55.69%
mean(awaywin$percent[awaywin$team_a == "Pakistan"]) # 56.40%
mean(awaywin$percent[awaywin$team_a == "South Africa"]) # 63.29%
mean(awaywin$percent[awaywin$team_a == "Sri Lanka"]) # 55.07%
mean(awaywin$percent[awaywin$team_a == "West Indies"]) # 68.75%
mean(awaywin$percent[awaywin$team_a == "Zimbabwe"]) # NaN




# 300 or more runs, more wins
score300 <- with(cricket[cricket$first_innings_total >= 300,], xtabs( ~ first_innings_total, winner))
score300/rowSums(score300) # 
score300

plot(score300)




###### Data Mining - Transformed & Pre-processed data #####
