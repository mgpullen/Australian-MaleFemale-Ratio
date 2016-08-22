# Clear workspace
rm(list=ls())

# Load required libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)

# File name of data to import
fileName <- "1981Q3-2015Q4.csv"

# Load data into APdata (Australian Population data)
APdata <- read.csv(fileName, stringsAsFactors = FALSE)

# Put the time column into an R compatible date-time format
date_start <- "01"
APdata$Date <- dmy(paste(date_start, APdata$Time, sep = "-"))

# Put the data into a dplyr format using tbl_df function
APdata <- tbl_df(APdata)

# Select the rows and columns of interest
# Currently only interested in the population estimates
APdata <- filter(APdata, Measure == "Estimated Resident Population")
APdata <- select(APdata, State, Date, Sex, Age, Value)