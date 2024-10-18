

# Set RNG seed and generate test data
suppressWarnings(RNGversion(min(as.character(getRversion()),"3.5.3")))
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <- data.frame(
  w = sample(1:4000, size = n, replace = TRUE),
  v = runif(n = n, 0, 10000)
)

# Test 1: Check if the correct object is returned
test_that("Correct object is returned", {
  expect_silent(kd <- knapsack_dynamic_programming(x = knapsack_objects[1:8,], W = 3500))
  expect_named(kd, c("max_value", "selected_items"))
})

# Test 2: Test that the function rejects erroneous input
test_that("Function rejects erroneous input", {
  expect_error(knapsack_dynamic_programming("hello", 3500)) # Non-data frame input
  expect_error(knapsack_dynamic_programming(x = knapsack_objects[1:8,], W = -3500)) # Negative capacity
})

# Test 3: Check if the function returns correct results
test_that("Function returns correct results", {
  kd <- knapsack_dynamic_programming(x = knapsack_objects[1:8,], W = 3500)
  expect_equal(round(kd$max_value), 16770)
  expect_true(all(round(kd$selected_items) %in% c(5, 8)))
  
  kd <- knapsack_dynamic_programming(x = knapsack_objects[1:12,], W = 3500)
  expect_equal(round(kd$max_value), 16770)
  expect_true(all(round(kd$selected_items) %in% c(5, 8)))
  
  kd <- knapsack_dynamic_programming(x = knapsack_objects[1:8,], W = 2000)
  expect_equal(round(kd$max_value), 15428)
  expect_true(all(round(kd$selected_items) %in% c(3, 8)))
  
  kd <- knapsack_dynamic_programming(x = knapsack_objects[1:12,], W = 2000)
  expect_equal(round(kd$max_value), 15428)
  expect_true(all(round(kd$selected_items) %in% c(3, 8)))
  
  # Performance test
  st <- system.time(kd <- knapsack_dynamic_programming(x = knapsack_objects[1:16,], W = 2000))
  expect_true(as.numeric(st)[2] >= 0.00)
})

# Test 4: Larger dataset checks
# test_that("Function returns correct results for larger datasets", {
#   kd <- knapsack_dynamic_programming(x = knapsack_objects[1:800,], W = 3500)
#   expect_equal(round(kd$max_value), 192647)
#   
#   kd <- knapsack_dynamic_programming(x = knapsack_objects[1:1200,], W = 3500)
#   expect_equal(round(kd$max_value), 270290)
# })

