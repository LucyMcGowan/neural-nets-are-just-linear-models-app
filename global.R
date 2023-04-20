library(visNetwork)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(plotly)
library(glue)
library(igraph)
library(rintrojs)

load("dataset.rds")

nodes <- data.frame(id = 1:5, 
                    label = c("x", "A1", "A2", "A3", "y"))

edges <- data.frame(from = c(1, 1, 1, 2, 3, 4),
                    to = c(2, 3, 4, 5, 5, 5))
coords <- matrix(c(1, 2, 
                   2, 0, 
                   2, 2, 
                   2, 4, 
                   3, 2), ncol = 2, byrow = TRUE)

fade_color <- function(color, i, selected) {
  if (i == 0) {
    color
  } else if (i == selected) {
    color
  } else {
    "lightgrey"
  }
}

bold_line <- function(i, selected) {
  if (i == selected) {
    5
  } else {
    2
  }
}
