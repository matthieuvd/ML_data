#first the necessary libraries are loaded
install.packages("dplyr")
install.packages("glmnet")
install.packages("boot")
install.packages("dummy")
install.packages("corrplot")
library(dplyr)
library(glmnet)
library(boot)
library(dummy)
library(corrplot)

#We then load in the csv files with the original data 
products <- read.csv("Original Data/products.csv")


#DATA PREPROCESSING
##We change the date type of price from char to a numeric type and omit the € sign
##Afterwards we omit the NAs as these cannot be used to fit our model
products <- products %>%
  mutate(price = as.numeric(sub("€", "", price)))
#products$price <- na.omit(products$price)

##We will create a table that has all unique product ids and as features the ones that appear the most often
### Function to calculate mode
calculate_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

### Use dplyr to create the new table
unique_products <- products %>%
  group_by(pid) %>%
  summarise(
    brand = calculate_mode(brand),
    category = calculate_mode(category),
    price = mean(price),
    season = calculate_mode(season),
    sku_size = calculate_mode(sku_size),
    subcategory = calculate_mode(subcategory),
    subsubcategory = calculate_mode(subsubcategory)
  ) %>%
  ungroup()

#Setting the sku_size column on values 1 (has a size) and 0 (has no size)
#Remove leading and trailing whitespaces
unique_products$sku_size <- trimws(unique_products$sku_size)
#Mutate the sku_size column depending on their value
unique_products <- unique_products %>%
  mutate(sku_size = ifelse(sku_size != '0', '1', '0'))


#Creating dummies for season, sku_size, category
cats <- categories(unique_products[, c("season", "sku_size","category")], p = "all")
cats2 <- categories(unique_products[, c("brand", "subcategory","subsubcategory")], p = 10)

dummies_products <- dummy(unique_products[, c("season", "sku_size","category")],
                       object = cats)
dummies_products2 <- dummy(unique_products[, c("brand", "subcategory","subsubcategory")],
                           object = cats2)

unique_products <- subset(unique_products, select = -c(season, sku_size, category,brand,subcategory,subsubcategory))
unique_products <- cbind(unique_products, dummies_products,dummies_products2)


#Checking correlation btwn dummy variables WEET NIET OF HET NUTTIG IS 
dummy_data <- subset(unique_products, select = -c(price,pid))
column_pairs <- combn(names(dummy_data), 2, simplify = TRUE)

# Calculate phi coefficient for each pair
phi_coefficients <- apply(column_pairs, 2, function(pair) {
  table_data <- table(dummy_data[, pair])
  phi_coefficient <- sqrt(chisq.test(table_data)$statistic / sum(table_data))
  return(phi_coefficient)
})

# Create a dataframe to store the results
association_results <- data.frame(
  Variable1 = column_pairs[1, ],
  Variable2 = column_pairs[2, ],
  Phi_Coefficient = phi_coefficients
)


#CREATION OF TRAINING SET 
## We'll load the table that has for each product id of year 1 the return rate 
return_rate_yr1 <- read.csv('New Data/return_rate_yr1.csv')

train <- merge(unique_products, return_rate_yr1, by = "pid", all = FALSE)
train <- na.omit(train)

write.csv(train, file='New Data/train.csv',row.names=F)

#CREATION OF TEST SET 
## We'll load the table that has for each product id of year 1 the return rate 
return_rate_yr2 <- read.csv('New Data/return_rate_yr2.csv')

test <- merge(unique_products, return_rate_yr2, by = "pid", all = FALSE)
test <- test %>%
  mutate_all(~ifelse(is.na(.), calculate_mode(.), .))


write.csv(test, file='New Data/test.csv',row.names=F)
