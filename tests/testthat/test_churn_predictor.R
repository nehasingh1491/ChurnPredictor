
context("Correct churn predictor")

#loading data
customer <- fread("/Users/nehasingh/Library/CloudStorage/OneDrive-Personal/UZH/2023/Spring_2023/R/day5/data_customer.csv")
personal <- fread("/Users/nehasingh/Library/CloudStorage/OneDrive-Personal/UZH/2023/Spring_2023/R/day5/data_personal.csv")

#merge two data tables
df <- merge(customer, personal, by="CustomerId")

#set columns to factors
df$Exited <- as.factor(df$Exited)
df$Gender <- as.factor(df$Gender)

test_that("churn predictor for customer",{
  expect_equal(churn_pred(df, 15653251),0.94059748)
  expect_error(churn_pred(df, 9999999))
})
