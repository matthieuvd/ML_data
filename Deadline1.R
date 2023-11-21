# Read the CSV file into an R data frame
sales_train <- read.csv("Original Data/sales_yr1.csv")
sales_train$date_id <- as.Date(as.character(sales_train$date_id), format = "%Y%m%d")

impute <- function(x, method = mean, val = NULL) {
  if (is.null(val)) {
    val <- method(x, na.rm = TRUE)
  }
  x[is.na(x)] <- val
  return(x)
}
modus <- function(x, na.rm = FALSE) {
  if (na.rm) x <- x[!is.na(x)]
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

sales_train$date_id <- impute(sales_train$date_id, method = mean) 
sales_train$channel <- impute(sales_train$channel, method = modus) 
sales_train$net_sales_amount <- impute(sales_train$net_sales_amount, method = mean) 

# Perform the calculations
summary_table_train <- sales_train %>%
  mutate(net_sales_amount = as.numeric(net_sales_amount)) %>%  # Ensure 'amount' is numeric
  group_by(pid) %>%  # Group data by product ID ('pid')
  summarize(
    total_returned = sum(net_sales_amount[net_sales_amount < 0], na.rm = TRUE),  # Sum negative amounts
    total_sold = sum(net_sales_amount[net_sales_amount > 0], na.rm = TRUE),  # Sum positive amounts
    return_rate = -total_returned / total_sold  # Calculate return rate
  ) %>%
  mutate(return_rate = ifelse(is.nan(return_rate) | is.infinite(return_rate), 0, return_rate)) %>%  # Handle NaN and Inf
  ungroup()  # Ungroup the data frame

# View the summary table
print(summary_table_train)
unique_products <- unique(products[, c("pid", "price")])
unique_products <- unique_products %>%
  group_by(pid, price) %>%
  summarise(count = n()) %>%
  slice(which.max(count)) %>%
  select(pid, price)
unique_products
sales_train_aggregated <- merge(summary_table_train, unique_products, by = "pid")
sales_train_aggregated
sales_train_aggregated <- sales_train_aggregated %>%
  mutate(price = as.numeric(sub("€", "", price)))
sales_train_aggregated
predictions


train.index <- sample(1:nrow(sales_train_aggregated), nrow(sales_train_aggregated)/2)
lm.fit <- lm(return_rate ~ price, data = sales_train_aggregated[train.index, ])

preds <- predict(lm.fit, new_data = sales_train_aggregated[-train.index, ])
preds
