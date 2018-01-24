library(tidyr)
library(dplyr)
library(ggplot2)

raw_listings <- read.csv('data/listings.csv')

clean_price <- function(price) as.numeric(gsub('\\$|,', '', price))

raw_listings %>%
  mutate(nprice = clean_price(price)) %>%
  select(name, price, nprice)

listings <- raw_listings %>%
  filter(!is.na(bedrooms), !is.na(bathrooms)) %>%
  mutate(price = clean_price(price),
         weekly_price = clean_price(weekly_price),
         monthly_price = clean_price(monthly_price))%>%
    
to.plot %>%
  ggplot(aes(mean_value_rating, mean_location_rating, size=num_listings, color = neighbourhood_cleansed)) +
  geom_point(alpha = 0.4) +
  theme_bw()
