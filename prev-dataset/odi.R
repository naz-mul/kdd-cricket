# https://github.com/jillrobinson/Cricket/blob/master/Cricket%20data%20exploration.ipynb
# https://rpubs.com/sinhavis/cricket
# http://www.cricketabstract.com/?dwl

# Read rows as columns - first column as header, second column as rows
# Only first 19 rows
# Ignore header
# Skip first row

setwd("~/Desktop/learn-r/proj")

import.mult.csv <- function(path, pattern, ...) {
  tmp.files.list <- list.files(path, pattern, full.names = TRUE)
  tmp.list.data <- list(length = length(tmp.files.list))
  for (i in 1:length(tmp.files.list))
  {
    cur.file <- tmp.files.list[i]
    tmp.list.data[[i]] <- read.csv(cur.file, ...)
  }
  names(tmp.list.data) <- tmp.files.list
  tmp.list.data
}

sbind <- function(x, y, fill = NA) {
  sbind.fill <- function(d, cols) {
    for (c in cols)
      d[[c]] <- fill
    d
  }
  
  x <- sbind.fill(x, setdiff(names(y), names(x)))
  y <- sbind.fill(y, setdiff(names(x), names(y)))
  
  rbind(x, y)
}

bind_data <- function(data.t) {
  for (i in 2:length(data)) {
    tmp.df <- as.data.frame(data[i])
    tmp.df.t <- as.data.frame(t(tmp.df))
    row.names(tmp.df.t) <- NULL
    data.t <- sbind(data.t, tmp.df.t, fill = TRUE)
  }
  data.t
}


# Import all the csv files as list
this.path <- "odi_csv_male"
this.pattern <- ".csv$"

data <-
  import.mult.csv(
    this.path,
    this.pattern,
    nrows = 19,
    skip = 1,
    row.names = NULL,
    header = F,
    colClasses = c("NULL", "NULL", NA)
  )

# Convert to dataframe
library(plyr)
cric.data <- as.data.frame(data[1])
cric.data.t <- as.data.frame(t(cric.data))
row.names(cric.data.t) <- NULL


# Extract column names
names.data <-
  import.mult.csv(
    this.path,
    this.pattern,
    nrows = 19,
    skip = 1,
    row.names = NULL,
    header = F,
    colClasses = c("NULL", NA, "NULL")
  )

library(plyr)
names <- as.data.frame(names.data[1])
names.char <- as.character(names$V2)


# Bind list of dataframes to a single dataframe
# First create a single data-frame
library(plyr)
cric.data <- as.data.frame(data[1])
cric.data.t <- as.data.frame(t(cric.data))
row.names(cric.data.t) <- NULL

# Then, merge all the other data frames to the single data frame
df <- bind_data(cric.data.t)


# Set column names
names(df) <- names.char

# Get a subset of the data - remove last 3 invalid columns
df.sub <- df[c(-20, -21,-22)]

# Write to csv file
write.csv(x = df.sub,
          file = "mens_odi_data.csv",
          row.names = FALSE)


# Convert date data to Date format
library(lubridate)
df.sub$date <- ymd(as.character(df.sub$date))
head(df.sub$date)

