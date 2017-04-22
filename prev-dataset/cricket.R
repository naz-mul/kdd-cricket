# https://decisionstats.com/2013/04/25/using-r-for-cricket-analysis-rstats-ipl/
# http://stats.espncricinfo.com/ci/engine/records/team/match_results.html?class=2;id=2014;type=year
# http://stackoverflow.com/a/37199673/4946884

list.of.packages <- c("rvest", "XML")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(rvest)
url <- "http://stats.espncricinfo.com/ci/engine/records/team/match_results.html?class=2;id=2016;type=year"
webpage <- read_html(url)
table.nodes <- html_nodes(webpage, "table")
table <- html_table(table.nodes[1], fill = TRUE)

df <- do.call(rbind.data.frame, table)
write.csv(file = "data/2016.csv", df)
