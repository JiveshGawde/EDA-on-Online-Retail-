library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(plotly)

setwd("C:/Users/hp/Desktop/Lr_Tiwar_college/Datasets")
data = read.csv("OnlineRetail(ABA).csv")
View(data)

dim(data)
summary(data)
str(data)
glimpse(data)
sum(is.na(data))

View(head(data))
data$Description

names(data)
length(data)
class(data$Description)

data$CustomerID <- NULL

sum(is.na(data))

min(data$InvoiceDate)
max(data$InvoiceDate)

data = data %>%
  mutate(InvoiceDate = mdy_hm(InvoiceDate),  # Convert to date-time
         Month = month(InvoiceDate),
         Year = year(InvoiceDate))

data_cleaned = data %>% 
  mutate(Total_Price = Quantity * UnitPrice)

data_cleaned

glimpse(data_cleaned)

revenue_by_country = data_cleaned %>% 
  group_by(Country) %>% 
  summarise(Total_Price_New = sum(Total_Price)) %>% 
  arrange(desc(Total_Price_New))

revenue_by_country

monthly_revenue = data_cleaned %>% 
  group_by(Month) %>% 
  summarise(Total_Price = sum(Total_Price))


Mrt = ggplot(data = monthly_revenue, aes(x = factor(Month), y = Total_Price)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Monthly Revenue Trend",
    x = "Month",
    y = "Revenue"
  )

Monthly_Revenue_Trend = ggplotly(Mrt)
Monthly_Revenue_Trend


top_products = data_cleaned %>% 
  group_by(Description) %>% 
  summarise(Total_Price = sum(Total_Price)) %>% 
  arrange(desc(Total_Price)) %>% 
  head(10)

top_products

tp10 = ggplot(data = top_products, aes(x = reorder(Description, Total_Price), y = Total_Price)) +
                geom_col(fill = "SeaGreen") +
                labs(
                  title = "Top 10 Products",
                  x = "Description",
                  y = "Total_Price"
                ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels by 45 degrees
  )

Top_10_products = ggplotly(tp10)
Top_10_products              

# There seems a correlation between top 10 products which generated highest revenue with
# the reason for sales being highest in months of October, November and December
# that is the top 10 products are mostly bought in those three months more than other

# to clarify this first we can create a table which shows the distribution of these 10 products in each month
orders_by_month = data_cleaned %>% 
  group_by(Month, Description) %>% 
  summarise(Order_Count = n()) %>% 
  filter(Order_Count > 250) %>% 
  arrange(desc(Order_Count))
print(orders_by_month, n = Inf)
# secondly if it is true that these products are mostly sold in months of october, november and december check which country is buying them the most
count_by_country_product <- function(product_name) {
  data_cleaned %>%
    filter(Description == product_name) %>%   # Filter based on the passed product name
    group_by(Country) %>%
    summarise(Count = n(), .groups = "drop") %>%
    arrange(desc(Count))
}

rnl = count_by_country_product("RABBIT NIGHT LIGHT")
rnl
rct = count_by_country_product("REGENCY CAKESTAND 3 TIER")
rct

#Hypothesis: "Products that protect from the cold and are festive in nature 
#(e.g., Christmas-related products) experience higher demand in the months of November 
#and December, especially in the United Kingdom."


