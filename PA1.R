
# Project Assignment 1 #

# Loading data

zip.name <- "activity.zip"
file.name <- "activity.csv"

unzip(zip.name)
activity <- read.csv(file=file.name, header = TRUE, stringAsFactors = FALSE, na.strings = "NA")
