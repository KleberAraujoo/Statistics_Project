library(quantmod)
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(hrbrthemes)


master_df <- read.csv("google.csv")
column_list <- c("Open","High","Low","Close","Adj Close", "Volume")

master_df$X <- NULL

master_df <- master_df %>% drop_na()
master_df$Date <- strptime(master_df$Date, format="%Y-%m-%d")