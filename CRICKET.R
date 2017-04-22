# Created by: nazmul
# Created on: 21/04/17

install.packages("jsonlite")
library(jsonlite)

setwd("~/Desktop/kdd-cricket")
raw.data <- read.csv("final_output.csv")

# Find the missing odi matches
json <- fromJSON("matches-odi.json")
json <- as.vector(as.integer(json))
ids <- raw.data$id
ids <- as.vector(ids)

setdiff(json, ids)

# find the number of duplicated ids
dim(raw.data[duplicated(raw.data$id),])[1]


# Create a new column for gender
raw.data$gender <- "male"

raw.data$gender <- mapply(grepl, pattern="Women", x= raw.data$team_a)


