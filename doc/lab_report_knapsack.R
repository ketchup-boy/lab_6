## ----include = FALSE----------------------------------------------------------
library(lab6)  # Assuming your package is named 'lab6'
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
# Generate random items
set.seed(42)
n <- 2000
knapsack_objects <- data.frame(
  w = sample(1:4000, size = n, replace = TRUE),
  v = runif(n = n, 0, 10000)
)

## -----------------------------------------------------------------------------

result <- brute_force_knapsack(knapsack_objects[1:8,], W = 3500)
print(result$max_value)
print(result$selected_items)

## -----------------------------------------------------------------------------

result <- knapsack_dynamic_programming(knapsack_objects[1:800,], W = 3500)
print(result$max_value)
print(result$selected_items)

## -----------------------------------------------------------------------------

result <- greedy_knapsack(knapsack_objects[1:1200,], W = 3500)
print(result$value)
print(result$elements)

## ----setup--------------------------------------------------------------------
library(lab6)

