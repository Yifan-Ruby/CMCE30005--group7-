library(lubridate)
library(dplyr)     # Data manipulation
library(tibble)    # Enhanced data frames
library(ggplot2)   # Visualization

calendar <- read.csv("calendar_airbnb.csv")
listings <- read.csv("listings_airbnb.csv")



# keep columns that are not full of NA values
listings_clean <- listings[, colSums(is.na(listings)) < nrow(listings)] 

# changing type of date from chr to ymd date format
listings_clean <- listings_clean |>
  mutate(across(c(price_quote_checkin_date,
                  price_quote_checkout_date,
                  calendar_last_scraped,
                  first_review,
                  last_review), ymd))
#checking if the ids and prices all match
listings_new %>% group_by(id) %>% summarise(n_url = n_distinct(listing_url), n_scrape = n_distinct(scrape_id)) %>% filter(n_url != 1 | n_scrape != 1)
listings_new %>% group_by(host_id) %>% summarise(n_url = n_distinct(host_url)) %>% filter(n_url != 1)
listings_new %>% group_by(price) %>% summarise(n_url = n_distinct(price_quote_price_per_night)) %>% filter(n_url != 1)

listings_new <- listings[, colSums(is.na(listings)) < nrow(listings)] 

listings_new <- listings_new |>
  mutate(across(c(price_quote_checkin_date,
                  price_quote_checkout_date,
                  calendar_last_scraped,
                  first_review,
                  last_review), ymd))

#listings without an epty price entity, could be seen as inactive listings
empty_price <- listings_new %>%
  filter(is.na(price) & is.na(price_quote_price_per_night) & is.na(price_quote_total_price))

View(empty_price)

#long min stay: 
long_minimum_stay <- listings_new %>%
  filter(minimum_nights > 360)

View(long_minimum_stay)

#hotel free listings:
library(dplyr)
library(stringr)

# Listings that mention "hotel" in either name or description
hotel_listings <- listings_clean %>%
  filter(
    str_detect(
      coalesce(room_type, ""),
      regex("hotel", ignore_case = TRUE)
    ) |
      str_detect(
        coalesce(property_type, ""),
        regex("hotel", ignore_case = TRUE)
      )
  )
# Listings that do NOT mention "hotel"
hotel_free_listings <- listings_clean %>%
  filter(!id %in% hotel_listings$id)

#final listings, hotel free, no inactive listings, rid of unnessasary entities
final_listings <- listings_clean %>%
  filter(
    !id %in% hotel_listings$id,
    !id %in% long_minimum_stay$id
  ) %>%
  select(
    -minimum_minimum_nights,
    -maximum_minimum_nights, 
    -host_profile_id,
    -host_profile_url,
    -minimum_maximum_nights,
    -maximum_maximum_nights,
  )

cat("Original listings:", nrow(listings_clean), "\n")
cat("Hotels removed:", nrow(hotel_listings), "\n")
cat("Long minimum stays removed:", nrow(long_minimum_stay), "\n")
cat("Final listings:", nrow(final_listings), "\n")

#remove empty rows, no empty rows
empty_rows <- final_listings %>%
  filter(if_all(everything(), is.na))

#empty location entities
missing_coordinates <- final_listings %>%
  filter(
    is.na(latitude) | is.na(longitude)
  )
View(empty_rows)

#price entries are all characters, changing them to numeric
library(readr)

location_data <- location_data %>%
  mutate(
    price = parse_number(as.character(price))
  )

library(dplyr)
library(readr)

final_listings <- final_listings %>%
  mutate(
    price = parse_number(as.character(price))
  )

class(location_data$price)
summary(location_data$price)
final_listings <- final_listings %>%
  filter(
    !(
      coalesce(availability_30, -1) == 0 &
        coalesce(availability_60, -1) == 0 &
        coalesce(availability_90, -1) == 0 &
        coalesce(availability_365, -1) == 0
    )
  )
#everything above should produce a final_listings dataset with 24423 obs and 71 variables
# Basic overview
str(final_listings)

# Summary statistics for every column
summary(final_listings)

# Column names
names(final_listings)