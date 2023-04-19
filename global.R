library(patchwork)
library(DiagrammeR)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(plotly)

load("dataset.rds")
load("dataset2.rds")

graph <- create_graph()

# add nodes
graph %>%
  add_node(label = "x") %>%
  add_node(label = "A3") %>%
  add_node(label = "A2") %>%
  add_node(label = "A1") %>%
  add_node(label = "y") %>%
  set_node_position(node = 1, x = 1, y = 2) %>%
  set_node_position(node = 2, x = 2, y = 1) %>%
  set_node_position(node = 3, x = 2, y = 2) %>%
  set_node_position(node = 4, x = 2, y = 3) %>%
  set_node_position(node = 5, x = 3, y = 2) %>%
  
  add_edge(1, 2) %>%
  add_edge(1, 3) %>%
  add_edge(1, 4) %>%
  add_edge(2, 5) %>%
  add_edge(3, 5) %>%
  add_edge(4, 5) -> graph



create_graph() %>%
  add_node(label = "x") %>%
  add_node(label = "A13") %>%
  add_node(label = "A12") %>%
  add_node(label = "A11") %>%
  add_node(label = "A22") %>%
  add_node(label = "A21") %>%
  add_node(label = "y") %>%
  set_node_position(node = 1, x = 1, y = 2) %>%
  set_node_position(node = 2, x = 2, y = 1) %>%
  set_node_position(node = 3, x = 2, y = 2) %>%
  set_node_position(node = 4, x = 2, y = 3) %>%
  set_node_position(node = 5, x = 3, y = 1.5) %>%
  set_node_position(node = 6, x = 3, y = 2.5) %>%
  set_node_position(node = 7, x = 4, y = 2) %>%
  
  add_edge(1, 2) %>%
  add_edge(1, 3) %>%
  add_edge(1, 4) %>%
  add_edge(2, 5) %>%
  add_edge(3, 5) %>%
  add_edge(4, 5) %>%
  add_edge(2, 6) %>%
  add_edge(3, 6) %>%
  add_edge(4, 6) %>%
  add_edge(5, 7) %>%
  add_edge(6, 7)  -> graph2

