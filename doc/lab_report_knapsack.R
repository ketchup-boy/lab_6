## ----include = FALSE----------------------------------------------------------
library(lab6)  
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
# Generate random items
set.seed(42)
n <- 2000
knapsack_objects <- data.frame(
  w = sample(1:4000, size = n, replace = TRUE),
  v = runif(n = n, 0, 10000)
)

## ----brute force--------------------------------------------------------------

result <- brute_force_knapsack(knapsack_objects[1:8,], W = 3500)
print(result$max_value)
print(result$selected_items)

## ----dynamic------------------------------------------------------------------

result <- knapsack_dynamic_programming(knapsack_objects[1:500,], W = 3500)
print(result$max_value)
print(result$selected_items)

## ----greedy-------------------------------------------------------------------

result <- greedy_knapsack(knapsack_objects[1:1200,], W = 3500)
print(result$value)
print(result$elements)

## ----brute force opt----------------------------------------------------------

result <- brute_force_knapsack_optimized(knapsack_objects[1:8,], W = 3500)
print(result$max_value)
print(result$selected_items)

## ----dynamic opt--------------------------------------------------------------

result <- knapsack_dynamic_programming_optimized(knapsack_objects[1:500,], W = 3500)
print(result$max_value)
print(result$selected_items)

## ----greedy opt---------------------------------------------------------------

result <- greedy_knapsack_optimized(knapsack_objects[1:1200,], W = 3500)
print(result$value)
print(result$elements)

## ----brute porce par----------------------------------------------------------

result <- brute_force_knapsack_general(knapsack_objects[1:8,], W = 3500, TRUE)
print(result$max_value)
print(result$selected_items)

## ----brute force time---------------------------------------------------------
#system.time({
 # result <- brute_force_knapsack(knapsack_objects[1:16,], W = 3500)
#})

## ----dynamic time-------------------------------------------------------------
#system.time({
#  result <- knapsack_dynamic_programming(knapsack_objects[1:500,], W = 3500)
#})

## ----setup brute force opt vs regular-----------------------------------------
#system.time({
#brute_force_knapsack(knapsack_objects[1:16,], 3300)
#})

#system.time({
#brute_force_knapsack_optimized(knapsack_objects[1:16,], 3000)
#})

## ----dynamic opt vs regular---------------------------------------------------
#system.time({
#knapsack_dynamic_programming(knapsack_objects[1:600,], 3300)
#})

#system.time({
#knapsack_dynamic_programming_optimized(knapsack_objects[1:600,], 3000)
#})

## ----prute force par vs regular-----------------------------------------------
#system.time({
# result <- brute_force_knapsack_general(knapsack_objects[1:17,], 3300, FALSE)
#})

#system.time({
#result_parallel <- brute_force_knapsack_general(knapsack_objects[1:17,], 3000, TRUE)
#})

## ----library------------------------------------------------------------------
library(lab6)

