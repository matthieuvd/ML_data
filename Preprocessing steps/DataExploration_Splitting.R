##DATA EXPLORATION
#Read in the necessary data 
original_train <- read.csv('Original Data/sales_yr1.csv')
products_data <- read.csv('Original Data/products.csv')

original_train_summary <- summary(original_train)
original_train_summary 

products_summary <- summary(products_data)
products_summary

total_train_data <- merge(original_train, products_data, on = 'pid')
summary(total_train_data)

total_train_data$date_id <- as.Date(as.character(total_train_data$date_id), format = "%Y%m%d")

counts_channel <- table(total_train_data$channel)
counts_channel <- sort(counts_channel, decr = TRUE)
counts_channel
counts_physical <- sum(counts_channel) - counts_channel['CH//Online']
names(counts_physical) <- "CH//PhysicalStore"
counts_channel <- append(counts_channel, counts_physical)
counts_channel <- counts_channel[c("CH//Online", "CH//PhysicalStore")]
counts_channel

online_purchases_index <- which(total_train_data$channel == "CH//Online")
total_train_data[-c(online_purchases_index), 'channel'] <- 0
total_train_data[c(online_purchases_index), 'channel'] <- 1

