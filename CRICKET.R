
# Set working directory and import raw data
setwd("~/Desktop/kdd-cricket")

# Source
source("PREPROCESSING.R")

###### Data Mining - Transformed & Pre-processed data #####
cricket <- read.csv("3-transformed-data/cric_transformed_odi.csv")
str(cricket)

cricket$outcome <- as.character(cricket$outcome)
cricket$day_night <- as.factor(cricket$day_night)
cricket$rain <- as.factor(cricket$rain)
cricket$duckworth_lewis <- as.factor(cricket$duckworth_lewis)

str(cricket)
summary(cricket)
