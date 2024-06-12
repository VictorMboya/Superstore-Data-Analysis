#Loading libraries
library(janitor)
library(lubridate)
library(tidyverse)
library(webr)
library(ggplot2)
library(scales)
library(maps)

#Loading the data
Data <- read.csv("D:\\Data Analysis Projects\\006_Supestore_sales\\train.csv")
class(Data)
View(Data)

#DATA PREPARATION
#Changing column names                 
Data <- clean_names(Data)
colnames(Data)
#Checking data types
str(Data)
#Changing data types for order_date and ship_date columns
Data$order_date <- as.Date(Data$order_date, '%d/%m/%Y')
Data$ship_date <- as.Date(Data$ship_date, '%d/%m/%Y')
str(Data)

View(Data)

#Checking for null values
sum(is.na(Data))
#Where are the null values
sapply(Data, function(x)which(is.na(x)))
#Highlight the rows with null values
null_values <- Data[c(2235, 5275, 8799, 9147, 9148, 9149, 9387, 9388, 9389, 9390, 9742),]
View(null_values)
#The null values are the Postal Code for Vermont, Burlington, United States.
#Replacing the null with the correct Postal Code of Burlington city in Vermont state ; 05401-05408  
Data$postal_code <- replace_na(Data$postal_code,05401)

#Checking for duplicate rows
sum(duplicated(Data))

View(Data)

#Attach the data
attach(Data)

#DATA ANALYSIS
#Orders Analysis
#Total number of orders:
length(order_id)
#Total number of unique orders:
length(unique(order_id))
#Order sales
sales_by_order_id <- aggregate(sales~order_id, data = Data, sum) %>%
  arrange(desc(sales))
#Top 5 orders by sum
top_orders <- head(sales_by_order_id, 5)
View(top_orders)
#Bottom 5 orders by  sum
bottom_orders <- tail(sales_by_order_id, 5)
View(bottom_orders)


#Category and Sub-category analysis
#Main category sales
category_sum <- aggregate(sales~category, data = Data, sum) %>%
  arrange(desc(sales))
View(category_sum)
#Sub-category sales
sub_category_sum <- aggregate(sales~sub_category, data = Data, sum) %>%
  arrange(desc(sales))
View(sub_category_sum)
#Mean of sales by category
round(sum(sales)/length(unique(category)),2)
#Mean of sales by sub-category
round(sum(sales)/length(unique(sub_category)),2)
#Grouped data frame of sales for category and sub-category
table_1 <- Data %>%
  group_by(category, sub_category) %>%
  summarise(sales = sum(sales))
#Pie-donut plot
PieDonut(table_1,
         aes(category, sub_category, count = sales),
         title = "Sales of Main and Sub-category")
#Sub-category bar plot
ggplot(data = Data, aes(x = reorder(sub_category, -sales), y = sales)) +
  geom_bar(stat = "identity", fill = "grey") +
  geom_hline(yintercept = sum(sales)/length(unique(sub_category)), color = "blue", lwd = 0.75) +
  scale_y_continuous(labels = comma) +
  labs(title = "Sales by Sub-Category", x = "Sub-Category", y = "Sales") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, vjust = 0.5))


#Product Analysis
#Product rankings
product_ranking <- Data %>%
  group_by(product_id) %>%
  summarise(total_orders = length(product_id), average_price = mean(sales)) %>%
  arrange(desc(total_orders))
#Top 3 products
top_products <- head(product_ranking, 10)
View(top_products)


#Analysis by Date
#Total sales by year
year_sales <- Data %>%
  group_by(year = year(order_date)) %>%
  summarize(total_sales = sum(sales)) %>%
  mutate(rate  = (total_sales - dplyr::lag(total_sales))/dplyr::lag(total_sales)*100)
View(year_sales)
#Average sales by month and total orders over the years
month_sales <- Data %>%
  group_by(month = month(order_date)) %>%
  summarize(total_orders = length(order_id), average_sales = mean(sales)) %>%
  mutate(average_sales_rate  = (average_sales - dplyr::lag(average_sales))/dplyr::lag(average_sales)*100,
         orders_rate  = (total_orders - dplyr::lag(total_orders))/dplyr::lag(total_orders)*100)
View(month_sales)
#Time with highest sales
time_sales <- Data %>%
  summarize(total_sales = sum(sales)) %>%
  arrange(desc(total_sales))
head(time_sales,3)
#Time with highest orders
time_orders <- Data %>%
  group_by(order_date) %>%
  summarize(total_orders = length(order_date)) %>%
  arrange(desc(total_orders))
head(time_orders,3)
#Plot of sales by ordered date
ggplot(Data, aes(x = order_date, y = sales)) +
  geom_line() +
  scale_y_continuous(labels = comma) +
  scale_x_date(labels = date_format("%b-%Y")) +
  labs(title = "Sales by Ordered Date", x = "Date", y = "Sales") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, vjust = 0.5))
#Plot of sales by shipped date
ggplot(Data, aes(x = ship_date, y = sales)) +
  geom_line() +
  scale_y_continuous(labels = comma) +
  scale_x_date(labels = date_format("%b-%Y")) +
  labs(title = "Sales by Shipped Date", x = "Date", y = "Sales") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, vjust = 0.5))


#Shipping Analysis
#Shipping rankings
shipping_ranking <- Data %>%
  group_by(ship_mode) %>%
  summarise(orders_shipped = length(ship_mode)) %>%
  arrange(desc(orders_shipped))
View(shipping_ranking)
#Plot for ship mode rankings
PieDonut(shipping_ranking,
         aes(ship_mode, count = orders_shipped),
         title = "Shipping modes")


#Customer Analysis
#Segment sales
segment_sales <- Data %>%
  group_by(segment) %>%
  summarise(total_ordered = length(segment),
            total_sales = sum(sales),
            average_sales = mean(sales)) %>%
  arrange(desc(total_ordered))
View(segment_sales)
#Plot of segment by sales
PieDonut(segment_sales,
         aes(segment, count = total_sales),
         title = "Sales by Segment")
#Customer rankings
customer_ranking <- Data %>%
  group_by(customer_id) %>%
  summarise(total_ordered = length(customer_id)) %>%
  arrange(desc(total_ordered))
#Top 3 customers
top_customers <- head(customer_ranking, 3)
View(top_customers)


#Geographical Analysis
#Region sales
region_sales <- Data %>%
  group_by(region) %>%
  summarise(total_ordered = length(region),
            total_sales = sum(sales),
            average_sales = mean(sales)) %>%
  arrange(desc(total_sales))
View(region_sales)
#Plot of region by sales
PieDonut(region_sales,
         aes(region, count = total_sales),
         title = "Sales by Region")
#State sales
state_sales <- Data %>%
  group_by(state) %>%
  summarise(total_ordered = length(state),
            total_sales = sum(sales),
            average_sales = mean(sales)) %>%
  arrange(desc(total_sales))
View(state_sales)
#City sales
city_sales <- Data %>%
  group_by(city) %>%
  summarise(total_ordered = length(city),
            total_sales = sum(sales),
            average_sales = mean(sales)) %>%
  arrange(desc(total_sales))
View(city_sales)

#Mapping by state
state_sales_2 <- Data %>%
  group_by(state) %>%
  summarise(total_sales = sum(sales))
colnames(state_sales) <- c("region", "sales")
state_sales$region <- tolower(state_sales$region)
us_state <- map_data("state")
statemap <- merge(us_state, state_sales, by = "region")
ggplot(statemap, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = sales), color = "white") +
  theme_void() +
  scale_fill_continuous(name = "Sales", low = 'lightblue', high = 'darkblue', label = comma) +
  labs(title = "Sales by State") +
  theme(plot.title = element_text(hjust = 0.5))
  