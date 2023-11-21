#LOAD IN DATAFRAMES
predictions <- read.csv("Original Data/predictions.csv")
products <- read.csv("Original Data/products.csv")
sales_yr1 <- read.csv("Original Data/sales_yr1.csv")
sales_yr2 <- read.csv("Original Data/sales_yr2.csv")

unique(sales_yr2[, "pid"])

sales_yr1_aggregated 