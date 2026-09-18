# CMCE30005--group7-
<<<<<<< HEAD
# CMCE30005 Business Analytics Challenge
## [Group7] - [Airbnb]

**Subject:** CMCE30005 Business Analytics Challenge, Semester 2 2026
**University:** University of Melbourne
**Team Members:** [Han Ton], [Ruby WANG], [Dina Lian], [Qi Zhang]

---

## Business Problem

[Write your one-paragraph problem statement here. Include: who is the stakeholder,
what question you are answering, why it matters, and what methods you plan to use.]

---

## Dataset

**Dataset name:** [e.g., Airbnb Melbourne - June 2026 Snapshot]
**Source:** [e.g., Inside Airbnb - http://insideairbnb.com/]
**Coverage:** [e.g., All active Airbnb listings in Melbourne as of 16 June 2026]

### Data Files

| File | Description | Size |
|------|-------------|------|
| `listings_all.csv` | Full listing details (~75 variables) | ~50 MB |
| `reviews_all.csv` | Guest review text | ~200 MB |
| `calendar_all.csv` | Daily availability and pricing | ~1 GB |

> **Note:** Data files are not committed to this repository due to size.
> Download from: [insert download URL or instructions]

---

*Last updated: [Date]*

=======
Problem statement

Air bnb
Business Problem 

test-dina lian

# IMPORTING LIBRARIES
library(tidyverse)
library(stargazer)
library(lubridate)
library(jsonlite)
library(stringr)
library(dplyr)
library(readr)
library(skimr)

# IMPORTING DATASETS 
calendar <- read.csv("calendar_airbnb.csv")
listings <- read.csv("listings_airbnb.csv")
reviews <- read.csv("reviews_airbnb.csv")

# DATA PRE-PROCESSING AND CLEANING 
## keep columns that are not full of NA values 
listings_clean <- listings[, colSums(is.na(listings)) < nrow(listings)]  

## changing type of date from chr to ymd date format 
listings_clean <- listings_new |> 
  mutate(across(c(price_quote_checkin_date, 
                  price_quote_checkout_date, 
                  calendar_last_scraped, 
                  first_review, 
                  last_review), ymd)) 

## checking if the ids and prices all match 

listings_clean %>% group_by(id) %>% summarise(n_url = n_distinct(listing_url), n_scrape = n_distinct(scrape_id)) %>% filter(n_url != 1 | n_scrape != 1) 
listings_clean %>% group_by(host_id) %>% summarise(n_url = n_distinct(host_url)) %>% filter(n_url != 1) 
listings_clean %>% group_by(price) %>% summarise(n_url = n_distinct(price_quote_price_per_night)) %>% filter(n_url != 1) 


listings_clean <- listings[, colSums(is.na(listings)) < nrow(listings)]  

listings_clean <- listings_clean |> 
  
  mutate(across(c(price_quote_checkin_date,
                  price_quote_checkout_date, 
                  calendar_last_scraped, 
                  first_review, 
                  last_review), ymd)) |>
## amenities is not empty
  filter(amenities != "[]")


## listings without an empty price entity, could be seen as inactive listings 
empty_price <- listings_clean %>% 
  filter(is.na(price) & is.na(price_quote_price_per_night) & is.na(price_quote_total_price)) 

View(empty_price) 

## long min stay:  

long_minimum_stay <- listings_clean %>% 
  filter(minimum_nights > 360) 

View(long_minimum_stay) 


#hotel free listings: 

## Listings that mention "hotel" in room description 

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

## Listings that do NOT mention "hotel" 

hotel_free_listings <- listings_clean %>% 
  filter(!id %in% hotel_listings$id) 



## final listings, hotel free, no inactive listings, rid of unnessasary entities 

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

## remove empty rows, no empty rows 

empty_rows <- final_listings %>% 
  filter(if_all(everything(), is.na)) 

View(empty_rows) 


## remove the rows with 0 availabilities 

class(location_data$price) 

summary(location_data$price) 


final_listings <- final_listings %>% 
  filter( 
    !(availability_30 == 0 & 
        availability_60 == 0 & 
        availability_90 == 0 & 
        availability_365 == 0) 
    
  ) 


# QUESTION 1 
## classifying function for amenities
normalize_amenity <- function(x) {
  x <- tolower(x)
  x <- str_squish(x)
  
  case_when(
    
    # Technology
    str_detect(x, "\\btv\\b|hdtv|television") ~ "tv",
    str_detect(x, "wifi|wi-fi|pocket wifi") ~ "wifi",
    str_detect(x, "ethernet") ~ "ethernet",
    str_detect(x, "sound system|bose|sonos|google home") ~ "sound_system",
    str_detect(x, "record player") ~ "record_player",
    str_detect(x, "game console") ~ "game_console",
    
    # Kitchen
    str_detect(x, "^kitchen$") ~ "kitchen",
    str_detect(x, "refrigerator|fridge") ~ "refrigerator",
    str_detect(x, "freezer") ~ "freezer",
    str_detect(x, "microwave") ~ "microwave",
    str_detect(x, "dishwasher") ~ "dishwasher",
    str_detect(x, "oven") ~ "oven",
    str_detect(x, "stove") ~ "stove",
    str_detect(x, "blender") ~ "blender",
    str_detect(x, "toaster") ~ "toaster",
    str_detect(x, "rice maker") ~ "rice_maker",
    str_detect(x, "bread maker") ~ "bread_maker",
    str_detect(x, "coffee maker") ~ "coffee_maker",
    str_detect(x, "hot water kettle|kettle") ~ "kettle",
    str_detect(x, "wine glasses") ~ "wine_glasses",
    str_detect(x, "dishes and silverware") ~ "dishes_silverware",
    str_detect(x, "cooking basics") ~ "cooking_basics",
    str_detect(x, "baking sheet") ~ "baking_sheet",
    str_detect(x, "breakfast bar") ~ "breakfast_bar",
    str_detect(x, "dining table") ~ "dining_table",
    str_detect(x, "kitchenette") ~ "kitchenette",
    str_detect(x, "outdoor kitchen") ~ "outdoor_kitchen",
    str_detect(x, "shared outdoor kitchen") ~ "shared_outdoor_kitchen",
    str_detect(x, "private outdoor kitchen") ~ "private_outdoor_kitchen",
    str_detect(x, "\\bcoffee\\b") ~ "coffee",
    
    # Workspace
    str_detect(x, "dedicated workspace") ~ "dedicated_workspace",
    
    # Laundry
    str_detect(x, "washer") ~ "washer",
    str_detect(x, "dryer") ~ "dryer",
    str_detect(x, "drying rack") ~ "drying_rack",
    str_detect(x, "laundromat") ~ "laundromat_nearby",
    str_detect(x, "iron") ~ "iron",
    str_detect(x, "ironing board") ~ "ironing board",
    
    # Air Conditioning/Heating
    str_detect(x, "air conditioning|\\bac\\b|ductless") ~ "air_conditioning",
    str_detect(x, "heating|heater|radiant heating") ~ "heating",
    str_detect(x, "ceiling fan|portable fans") ~ "fan",
    str_detect(x, "indoor fireplace") ~ "indoor_fireplace",
    str_detect(x, "outdoor fireplace") ~ "outdoor_fireplace",
    str_detect(x, "fireplace guards") ~ "fireplace_guards",
    
    # Bathroom
    str_detect(x, "shower gel") ~ "shower_gel",
    str_detect(x, "shampoo") ~ "shampoo",
    str_detect(x, "conditioner") ~ "conditioner",
    str_detect(x, "body soap|body wash|liquid soaps") ~ "body_soap",
    str_detect(x, "hair dryer") ~ "hair_dryer",
    str_detect(x, "bathtub") ~ "bathtub",
    str_detect(x, "hot water") ~ "hot_water",
    str_detect(x, "bidet") ~ "bidet",
    str_detect(x, "outdoor shower") ~ "outdoor_shower",
    
    # Bedroom
    str_detect(x, "bed linens") ~ "bed_linens",
    str_detect(x, "extra pillows") ~ "extra_pillows_blankets",
    str_detect(x, "hangers") ~ "hangers",
    str_detect(x, "clothing storage") ~ "clothing_storage",
    str_detect(x, "room-darkening") ~ "room_darkening_shades",
    str_detect(x, "mosquito net") ~ "mosquito_net",
    
    # Security / access
    str_detect(x, "self check-in") ~ "self_check_in",
    str_detect(x, "host greets you") ~ "host_greets_you",
    str_detect(x, "lockbox|keypad|smart lock") ~ "smart_lock",
    str_detect(x, "lock on bedroom door") ~ "bedroom_lock",
    str_detect(x, "exterior security cameras") ~ "security_cameras",
    str_detect(x, "smoke alarm") ~ "smoke_alarm",
    str_detect(x, "carbon monoxide alarm") ~ "carbon_monoxide_alarm",
    str_detect(x, "fire extinguisher") ~ "fire_extinguisher",
    str_detect(x, "first aid kit") ~ "first_aid_kit",
    str_detect(x, "\\bsafe\\b") ~ "safe",
    str_detect(x, "window guards") ~ "window_guards",
    str_detect(x, "outlet covers") ~ "outlet_covers",
    str_detect(x, "baby safety gates") ~ "baby_safety_gates",
    str_detect(x, "table corner guards") ~ "table_corner_guards",
    str_detect(x, "gated community") ~ "gated_community",
    str_detect(x, "noise decibel monitors") ~ "noise_decibel_monitors",
    str_detect(x, "doorman") ~ "doorman",
    str_detect(x, "building staff") ~ "building_staff",
    
    # Outdoor
    str_detect(x, "patio|balcony") ~ "patio_balcony",
    str_detect(x, "backyard") ~ "backyard",
    str_detect(x, "outdoor dining") ~ "outdoor_dining",
    str_detect(x, "outdoor furniture") ~ "outdoor_furniture",
    str_detect(x, "bbq|barbecue") ~ "bbq",
    str_detect(x, "fire pit") ~ "fire_pit",
    str_detect(x, "sun loungers") ~ "sun_loungers",
    str_detect(x, "hammock") ~ "hammock",
    str_detect(x, "outdoor playground") ~ "outdoor_playground",
    str_detect(x, "climbing wall") ~ "climbing_wall",
    str_detect(x, "skate ramp") ~ "skate_ramp",
    
    # Parking / EV
    str_detect(x, "ev charger") ~ "ev_charger",
    str_detect(x, "free parking") ~ "free_parking",
    str_detect(x, "paid parking") ~ "paid_parking",
    str_detect(x, "paid valet parking") ~ "paid_valet_parking",
    str_detect(x, "garage") ~ "garage",
    str_detect(x, "carport") ~ "carport",
    str_detect(x, "driveway parking") ~ "driveway_parking",
    str_detect(x, "street parking") ~ "street_parking",
    
    # Recreation
    str_detect(x, "piano") ~ "piano",
    str_detect(x, "\\bgym\\b|exercise equipment") ~ "gym",
    str_detect(x, "pool") ~ "pool",
    str_detect(x, "sauna") ~ "sauna",
    str_detect(x, "hot tub") ~ "hot_tub",
    str_detect(x, "board games") ~ "board_games",
    str_detect(x, "books and reading") ~ "books",
    str_detect(x, "bikes") ~ "bikes",
    str_detect(x, "ping pong table") ~ "ping_pong",
    str_detect(x, "mini golf") ~ "mini_golf",
    str_detect(x, "bowling alley") ~ "bowling_alley",
    str_detect(x, "laser tag") ~ "laser_tag",
    str_detect(x, "batting cage") ~ "batting_cage",
    str_detect(x, "tennis court") ~ "tennis_court",
    str_detect(x, "hockey rink") ~ "hockey_rink",
    str_detect(x, "life size games") ~ "life_size_games",
    str_detect(x, "arcade games") ~ "arcade_games",
    str_detect(x, "game console") ~ "game_console",
    str_detect(x, "kayak") ~ "kayak",
    str_detect(x, "boat slip") ~ "boat_slip",
    str_detect(x, "resort access") ~ "resort_access",
    str_detect(x, "ski-in/ski-out") ~ "ski_in_ski_out",
    
    # Family / children
    str_detect(x, "crib|travel crib|pack.*play") ~ "crib",
    str_detect(x, "high chair") ~ "high_chair",
    str_detect(x, "children.*books|children.*toys") ~ "children_toys_books",
    str_detect(x, "children.*dinnerware") ~ "children_dinnerware",
    str_detect(x, "baby bath") ~ "baby_bath",
    str_detect(x, "babysitter recommendations") ~ "babysitter_recommendations",
    str_detect(x, "baby monitor") ~ "baby_monitor",
    str_detect(x, "changing table") ~ "changing_table",
    str_detect(x, "children's playroom") ~ "childrens_playroom",
    str_detect(x, "theme room") ~ "theme_room",
    
    # Cleaning / housekeeping
    str_detect(x, "cleaning available during stay") ~ "cleaning_during_stay",
    str_detect(x, "housekeeping") ~ "housekeeping",
    str_detect(x, "cleaning products") ~ "cleaning_products",
    str_detect(x, "essentials") ~ "essentials",
    str_detect(x, "trash compactor") ~ "trash_compactor",
    
    # Food / services
    str_detect(x, "\\bbreakfast\\b") ~ "breakfast",
    
    # Beach / water
    str_detect(x, "private beach access") ~ "private_beach_access",
    str_detect(x, "shared beach access") ~ "shared_beach_access",
    str_detect(x, "beach access") ~ "beach_access",
    str_detect(x, "waterfront") ~ "waterfront",
    str_detect(x, "lake access") ~ "lake_access",
    
    # Property / accessibility
    str_detect(x, "single level home") ~ "single_level_home",
    str_detect(x, "private entrance") ~ "private_entrance",
    str_detect(x, "private living room") ~ "private_living_room",
    str_detect(x, "elevator") ~ "elevator",
    str_detect(x, "long term stays") ~ "long_term_stays",
    str_detect(x, "luggage dropoff") ~ "luggage_dropoff",
    
    # Pets / smoking
    str_detect(x, "pets allowed") ~ "pets_allowed",
    str_detect(x, "smoking allowed") ~ "smoking_allowed",
    
    # Views
    str_detect(x, "garden view") ~ "garden_view",
    str_detect(x, "bay view") ~ "bay_view",
    str_detect(x, "harbor view") ~ "harbor_view",
    str_detect(x, "ocean view|sea view") ~ "ocean_view",
    str_detect(x, "city skyline") ~ "city_view",
    str_detect(x, "courtyard view") ~ "courtyard_view",
    str_detect(x, "park view") ~ "park_view",
    str_detect(x, "mountain view") ~ "mountain_view",
    str_detect(x, "river view") ~ "river_view",
    str_detect(x, "valley view") ~ "valley_view",
    str_detect(x, "lake view") ~ "lake_view",
    str_detect(x, "beach view") ~ "beach_view",
    str_detect(x, "marina view") ~ "marina_view",
    str_detect(x, "resort view") ~ "resort_view",
    str_detect(x, "vineyard view") ~ "vineyard_view",
    str_detect(x, "golf course view") ~ "golf_course_view",
    str_detect(x, "canal view") ~ "canal_view",
    str_detect(x, "desert view") ~ "desert_view",
    
    # Anything not yet classified
    TRUE ~ NA_character_
  )
}


## identified items not matched by the classifying function
unmatched <- final_listings %>%
  select(id, amenities) %>%
  mutate(
    amenities = gsub("\\\\u2013", "-", amenities),
    amenities = lapply(amenities, function(x) {
      parsed <- jsonlite::fromJSON(x)
      if (length(parsed) == 0) NA_character_ else parsed
    })
  ) %>%
  unnest(amenities) %>%
  filter(!is.na(amenities)) %>%
  mutate(
    amenity = normalize_amenity(amenities)
  ) %>%
  filter(is.na(amenity)) %>%
  count(amenities, sort = TRUE)

unmatched

## export the unmatched amenities into a csv for ease of reading and sorting
write_csv(unmatched, "unmatched.csv")

## amenities long
amenities_long <- final_listings %>%
  select(id, amenities) %>%
  mutate(
    amenities = gsub("\\\\u2013", "-", amenities),
    
    amenities = map(amenities, function(x) {
      parsed <- fromJSON(x)
      
      if (length(parsed) == 0) {
        return(character(0))
      } else {
        return(parsed)
      }
    })
  ) %>%
  unnest_longer(amenities) %>%
  filter(!is.na(amenities)) %>%
  mutate(
    amenity_raw = amenities,
    amenity_clean = str_to_lower(str_squish(amenities))
  )

## binary processing for all amenities based on id
amenity_binary <- amenities_long %>%
  mutate(
    amenity = normalize_amenity(amenity_clean)
  ) %>%
  filter(!is.na(amenity)) %>%
  distinct(id, amenity) %>%
  mutate(present = 1L) %>%
  pivot_wider(
    names_from = amenity,
    values_from = present,
    values_fill = 0
  )

## df for streaming services on TV offered
streaming_df <- amenities_long %>%
  group_by(id) %>%
  summarise(
    streaming = as.integer(
      any(
        str_detect(
          amenity_clean,
          "netflix|hulu|amazon prime|prime video|disney\\+|apple tv|paramount\\+|max|hbo|peacock"
        )
      )
    ),
    
    netflix = as.integer(
      any(str_detect(amenity_clean, "netflix"))
    ),
    
    amazon_prime = as.integer(
      any(str_detect(amenity_clean, "amazon prime|prime video"))
    ),
    
    apple_tv = as.integer(
      any(str_detect(amenity_clean, "apple tv"))
    ),
    
    hulu = as.integer(
      any(str_detect(amenity_clean, "hulu"))
    ),
    
    disney_plus = as.integer(
      any(str_detect(amenity_clean, "disney\\+"))
    ),
    
    .groups = "drop"
  )

## df for parking services offered
parking_df <- amenities_long %>%
  group_by(id) %>%
  summarise(
    free_parking = as.integer(
      any(str_detect(amenity_clean, "free parking"))
    ),
    
    paid_parking = as.integer(
      any(str_detect(amenity_clean, "paid parking"))
    ),
    
    garage = as.integer(
      any(str_detect(amenity_clean, "garage"))
    ),
    
    street_parking = as.integer(
      any(str_detect(amenity_clean, "street parking"))
    ),
    parking_spaces = {
      spaces <- str_extract_all(
        amenity_clean,
        "\\d+\\s*(?:space|spaces)"
      )[[1]]
      
      if (length(spaces) == 0) {
        NA_real_
      } else {
        as.numeric(str_extract(spaces[1], "\\d+"))
      }
    },
    
    .groups = "drop"
  )

## counting individual group and total amenities
amenity_counts <- amenities_long %>%
  group_by(id) %>%
  summarise(
    # Technology
    technology_amenities = sum(
      str_detect(
        amenity_clean,
        "tv|hdtv|television|wifi|wi-fi|pocket wifi|ethernet|sound system|bose|sonos|google home|record player|game console"
      )
    ),
    
    # Kitchen
    kitchen_amenities = sum(
      str_detect(
        amenity_clean,
        "^kitchen$|refrigerator|fridge|freezer|microwave|dishwasher|oven|stove|blender|toaster|rice maker|bread maker|coffee maker|hot water kettle|kettle|wine glasses|dishes and silverware|cooking basics|baking sheet|breakfast bar|dining table|kitchenette|outdoor kitchen|shared outdoor kitchen|private outdoor kitchen|\\bcoffee\\b"
      )
    ),
    
    # Workspace
    workspace_amenities = sum(
      str_detect(
        amenity_clean,
        "dedicated workspace"
      )
    ),
    
    # Laundry
    laundry_amenities = sum(
      str_detect(
        amenity_clean,
        "washer|dryer|drying rack|laundromat|iron|ironing board"
      )
    ),
    
    # Climate
    climate_amenities = sum(
      str_detect(
        amenity_clean,
        "air conditioning|\\bac\\b|ductless|heating|heater|radiant heating|ceiling fan|portable fans|indoor fireplace|outdoor fireplace|fireplace guards"
      )
    ),
    
    # Bathroom
    bathroom_amenities = sum(
      str_detect(
        amenity_clean,
        "shower gel|shampoo|conditioner|body soap|body wash|liquid soaps|hair dryer|bathtub|hot water|bidet|outdoor shower"
      )
    ),
    
    # Bedroom
    bedroom_amenities = sum(
      str_detect(
        amenity_clean,
        "bed linens|extra pillows|hangers|clothing storage|room-darkening|mosquito net"
      )
    ),
    
    # Security / access
    security_access_amenities = sum(
      str_detect(
        amenity_clean,
        "self check-in|host greets you|lockbox|keypad|smart lock|lock on bedroom door|exterior security cameras|smoke alarm|carbon monoxide alarm|fire extinguisher|first aid kit|\\bsafe\\b|window guards|outlet covers|baby safety gates|table corner guards|gated community|noise decibel monitors|doorman|building staff"
      )
    ),
    
    # Outdoor
    outdoor_amenities = sum(
      str_detect(
        amenity_clean,
        "patio|balcony|backyard|outdoor dining|outdoor furniture|bbq|barbecue|fire pit|sun loungers|hammock|outdoor playground|climbing wall|skate ramp"
      )
    ),
    
    # Parking / EV
    parking_ev_amenities = sum(
      str_detect(
        amenity_clean,
        "ev charger|free parking|paid parking|paid valet parking|garage|carport|driveway parking|street parking"
      )
    ),
    
    # Recreation
    recreation_amenities = sum(
      str_detect(
        amenity_clean,
        "piano|\\bgym\\b|exercise equipment|pool|sauna|hot tub|board games|books and reading|bikes|ping pong table|mini golf|bowling alley|laser tag|batting cage|tennis court|hockey rink|life size games|arcade games|game console|kayak|boat slip|resort access|ski-in/ski-out"
      )
    ),
    
    # Family / children
    family_children_amenities = sum(
      str_detect(
        amenity_clean,
        "crib|travel crib|pack.*play|high chair|children.*books|children.*toys|children.*dinnerware|baby bath|babysitter recommendations|baby monitor|changing table|children's playroom|theme room"
      )
    ),
    
    # Cleaning / housekeeping
    cleaning_housekeeping_amenities = sum(
      str_detect(
        amenity_clean,
        "cleaning available during stay|housekeeping|cleaning products|essentials|trash compactor"
      )
    ),
    
    # Food / services
    food_services_amenities = sum(
      str_detect(
        amenity_clean,
        "\\bbreakfast\\b"
      )
    ),
    
    # Beach / water
    beach_water_amenities = sum(
      str_detect(
        amenity_clean,
        "private beach access|shared beach access|beach access|waterfront|lake access"
      )
    ),
    
    # Property / accessibility
    property_accessibility_amenities = sum(
      str_detect(
        amenity_clean,
        "single level home|private entrance|private living room|elevator|long term stays|luggage dropoff"
      )
    ),
    
    # Pets / smoking
    pets_smoking_amenities = sum(
      str_detect(
        amenity_clean,
        "pets allowed|smoking allowed"
      )
    ),
    
    # Views
    view_amenities = sum(
      str_detect(
        amenity_clean,
        "garden view|bay view|harbor view|ocean view|sea view|city skyline|courtyard view|park view|mountain view|river view|valley view|lake view|beach view|marina view|resort view|vineyard view|golf course view|canal view|desert view"
      )
    ),
    
    # Total unique amenities
    total_amenities = sum(technology_amenities, kitchen_amenities, workspace_amenities,
                          climate_amenities, laundry_amenities, security_access_amenities,
                          pets_smoking_amenities, view_amenities, property_accessibility_amenities,
                          beach_water_amenities, food_services_amenities, cleaning_housekeeping_amenities,
                          family_children_amenities, recreation_amenities, parking_ev_amenities,
                          outdoor_amenities),
    
    .groups = "drop"
  )

### checking characteristic and distribution of amenities count
skim(amenity_counts)

## creating amenities df
amenities_df <- amenity_binary %>%
  left_join(streaming_df, by = "id") %>%
  left_join(parking_df, by = "id") %>%
  left_join(amenity_counts, by = "id") %>%
  mutate(
    across(
      -c(id, parking_spaces),
      ~ replace_na(.x, 0)
    )
  )

write_csv(amenities_df, "amenities.csv")


## creating data frame for logistic regression
lreg_df <- listings_clean %>%
  select(id, estimated_occupancy_l365d) %>%
  left_join(amenity_counts, by = "id") %>%
  mutate(
    occupancy_rate = factor(
      ifelse((estimated_occupancy_l365d)/365>=0.75, 1, 0),
      levels = c(1, 0),
      labels = c("high occupancy", "low occupancy")
    )

## logistic regression attempt
q3_reg <- glm(
  occupancy_rate ~ . - id,
  data = lreg_df,
  family = "binomial"
)

summary(q3_reg)

# QUESTION 2 
#actual location analysis

airbnb_analysis <- listings_new %>%
  filter(
    !is.na(latitude),
    !is.na(longitude),
    !is.na(estimated_occupancy_l365d),
    minimum_nights <= 360
  ) %>%
  mutate(
    price = readr::parse_number(price),
    occupancy_rate = estimated_occupancy_l365d / 365
  )

summary(airbnb_analysis$occupancy_rate)

#location level analysis, non-detailed, which locations have high occupancy,
#and how many competing listings they contain (issues, grouping by neighbourhood_cleansed, we might miss certain locations, issues like including outliers still exist because occumency rate is not filtered)
location_analysis <- airbnb_analysis %>%
  group_by(neighbourhood_cleansed) %>%
  summarise(
    listings = n(),
    median_price = median(price, na.rm = TRUE),
    median_occupancy = median(occupancy_rate, na.rm = TRUE),
    median_revenue = median(estimated_revenue_l365d, na.rm = TRUE),
    total_occupied_nights = sum(estimated_occupancy_l365d, na.rm = TRUE),
    avg_location_rating = mean(review_scores_location, na.rm = TRUE)
  ) %>%
  arrange(desc(median_occupancy))

View(location_analysis)

#demand to supply measure: nights per listing: 
location_analysis <- location_analysis %>%
  mutate(
    demand_per_listing = total_occupied_nights / listings
  )
#how much booking activity does the average airbnb in this location receive(again, very generalised still, because the locations are only separated by the cleansed suburbs)
#ranking the 
location_analysis %>%
  arrange(desc(demand_per_listing)) %>%
  View()

#looking for high demand + low supply
location_analysis <- location_analysis %>%
  mutate(
    demand_level = if_else(
      median_occupancy >= median(median_occupancy, na.rm = TRUE),
      "High Demand",
      "Low Demand"
    ),
    
    supply_level = if_else(
      listings >= median(listings, na.rm = TRUE),
      "High Supply",
      "Low Supply"
    ),
    
    market_type = paste(demand_level, supply_level, sep = " / ")
  )
#making quadrands for better understanding 
location_analysis %>%
  select(
    neighbourhood_cleansed,
    listings,
    median_occupancy,
    median_price,
    median_revenue,
    market_type
  ) %>%
  arrange(desc(median_occupancy)) %>%
  View()
#isolate the high demand/low suply quadrant
opportunity_locations <- location_analysis %>%
  filter(market_type == "High Demand / Low Supply") %>%
  arrange(desc(median_revenue))

View(opportunity_locations)

#supply vs demand scatterplot
library(ggplot2)

ggplot(location_analysis,
       aes(x = listings,
           y = median_occupancy,
           size = median_revenue,
           label = neighbourhood_cleansed)) +
  geom_point(alpha = 0.7) +
  geom_text(
    aes(label = neighbourhood_cleansed),
    size = 3,
    nudge_y = 0.003,
    check_overlap = TRUE
  ) +
  geom_vline(
    xintercept = median(location_analysis$listings, na.rm = TRUE),
    linetype = "dashed"
  ) +
  geom_hline(
    yintercept = median(location_analysis$median_occupancy, na.rm = TRUE),
    linetype = "dashed"
  ) +
  labs(
    title = "Melbourne Airbnb Supply vs Demand",
    subtitle = "Bubble size represents median estimated annual revenue",
    x = "Number of Airbnb Listings (Supply)",
    y = "Median Estimated Occupancy Rate",
    size = "Median Revenue"
  ) +
  theme_minimal()

#visualise locations

class(final_listings$longitude)
class(final_listings$latitude)

summary(final_listings$longitude)
summary(final_listings$latitude)

final_listings %>%
  ggplot(aes(x = longitude, y = latitude)) +
  geom_point(alpha = 0.20, size = 0.6) +
  coord_fixed() +
  labs(
    title = "Spatial Distribution of Airbnb Listings",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()

##alternate way, not very visually pleasing
final_listings %>%
  ggplot(aes(x = longitude, y = latitude)) +
  stat_bin_2d(bins = 40) +
  coord_fixed() +
  labs(
    title = "Airbnb Listing Density Grid",
    subtitle = "Each grid cell shows the number of listings",
    x = "Longitude",
    y = "Latitude",
    fill = "Listings"
  ) +
  theme_minimal()

##data validation: making sure the neighbourhood_cleansed mathces the longitude and latitude

library(ggplot2)
library(dplyr)

final_listings %>%
  filter(
    !is.na(latitude),
    !is.na(longitude),
    !is.na(neighbourhood_cleansed)
  ) %>%
  ggplot(
    aes(
      x = longitude,
      y = latitude,
      colour = neighbourhood_cleansed
    )
  ) +
  geom_point(
    alpha = 0.35,
    size = 0.4
  ) +
  coord_fixed() +
  labs(
    title = "Airbnb Listings by Neighbourhood",
    subtitle = "Checking consistency of neighbourhood classifications",
    x = "Longitude",
    y = "Latitude",
    colour = "Neighbourhood"
  ) +
  theme_minimal()


#Faceted version: 
  final_listings %>%
  filter(
    !is.na(latitude),
    !is.na(longitude),
    !is.na(neighbourhood_cleansed)
  ) %>%
  ggplot(aes(x = longitude, y = latitude)) +
  geom_point(alpha = 0.5, size = 0.5) +
  coord_fixed() +
  facet_wrap(~ neighbourhood_cleansed) +
  labs(
    title = "Geographic Distribution by Neighbourhood",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()
  
  final_listings <- final_listings %>%
    mutate(
      estimated_occupancy_rate =
        estimated_occupancy_l365d / 365
    )

#occupency rate
  location_data <- final_listings %>%
    filter(
      !is.na(neighbourhood_cleansed),
      !is.na(latitude),
      !is.na(longitude)
    ) %>%
    mutate(
      price = parse_number(as.character(price)),
      occupancy_rate = estimated_occupancy_l365d / 365,
      
      pressure_30 = 1 - availability_30 / 30,
      pressure_60 = 1 - availability_60 / 60,
      pressure_90 = 1 - availability_90 / 90,
      
      current_pressure =
        0.5 * pressure_30 +
        0.3 * pressure_60 +
        0.2 * pressure_90
    )
  region_summary <- final_listings %>%
    group_by(neighbourhood_cleansed) %>%
    summarise(
      
      # SUPPLY
      supply = n(),
      
      # DEMAND
      median_occupancy_rate =
        median(estimated_occupancy_rate, na.rm = TRUE),
      
      # COMMERCIAL PERFORMANCE
      median_nightly_price =
        median(price, na.rm = TRUE),
      
      median_annual_revenue =
        median(estimated_revenue_l365d, na.rm = TRUE),
      
      # FORWARD AVAILABILITY
      median_availability_30 =
        median(availability_30, na.rm = TRUE),
      
      median_availability_60 =
        median(availability_60, na.rm = TRUE),
      
      median_availability_90 =
        median(availability_90, na.rm = TRUE),
      
      .groups = "drop"
    )
  
# visualising
  regional_results <- region_summary %>%
    mutate(
      occupancy_pct = round(median_occupancy_rate * 100, 1),
      median_nightly_price = round(median_nightly_price, 0),
      median_annual_revenue = round(median_annual_revenue, 0),
      
      demand_rank = rank(
        -median_occupancy_rate,
        ties.method = "min"
      ),
      
      supply_rank = rank(
        -supply,
        ties.method = "min"
      )
    ) %>%
    select(
      neighbourhood_cleansed,
      supply,
      supply_rank,
      occupancy_pct,
      demand_rank,
      median_nightly_price,
      median_annual_revenue
    ) %>%
    arrange(demand_rank)
  
  # Calculate median reference points
  supply_threshold <- median(
    region_summary$supply,
    na.rm = TRUE
  )
  
  demand_threshold <- median(
    region_summary$median_occupancy_rate,
    na.rm = TRUE
  )
  
  # Supply-demand scatterplot
  ggplot(
    region_summary,
    aes(
      x = supply,
      y = median_occupancy_rate,
      size = median_annual_revenue
    )
  ) +
    geom_point(alpha = 0.7) +
    
    geom_text(
      aes(label = neighbourhood_cleansed),
      size = 2.5,
      vjust = -0.8,
      check_overlap = TRUE
    ) +
    
    geom_vline(
      xintercept = supply_threshold,
      linetype = "dashed"
    ) +
    
    geom_hline(
      yintercept = demand_threshold,
      linetype = "dashed"
    ) +
    
    scale_x_log10() +
    
    scale_y_continuous(
      labels = scales::percent
    ) +
    
    scale_size_continuous(
      labels = scales::dollar
    ) +
    
    labs(
      title = "Airbnb Supply vs Estimated Demand",
      subtitle = "Bubble size represents median estimated annual revenue",
      x = "Number of Active Listings (log scale)",
      y = "Median Estimated Occupancy Rate",
      size = "Median Annual Revenue"
    ) +
    
    theme_minimal()

# QUESTION 3 

#check unsual bedroom values
summary(listings$bedrooms)

listings |>
  count(bedrooms) |>
  arrange(bedrooms)


# View listings with the largest bedroom counts

listings |>
  arrange(desc(bedrooms)) |>
  select(
    id,
    bedrooms,
    accommodates,
    room_type,
    minimum_nights
  ) |>
  head(20)

summary(listings$minimum_nights)

# check common minimum-night requirements

listings |>
  count(minimum_nights) |>
  arrange(desc(n)) |>
  head(20)


# Largest minimum-night requirements

listings |>
  arrange(desc(minimum_nights)) |>
  select(
    id,
    minimum_nights,
    accommodates,
    bedrooms,
    room_type
  ) |>
  head(20)

#check id unique and duplicated id

nrow(listings)
n_distinct(listings$id)

listings |>
  count(id) |>
  filter(n > 1)

#check whether calendar listing id exist in listings
calendar |>
  anti_join(
    listings,
    by = c("listing_id" = "id")
  ) |>
  distinct(listing_id)


#----EDA analysis----#

#Bedroom value

ggplot(
  listings |>
    filter(
      !is.na(bedrooms),
      bedrooms <= 10
    ),
  aes(x = factor(bedrooms))
) +
  geom_bar() +
  labs(
    title = "Distribution of Bedrooms",
    x = "Number of Bedrooms",
    y = "Number of Listings"
  ) +
  theme_minimal()

# Minimum night requiremnets
ggplot(
  listings |>
    filter(
      !is.na(minimum_nights),
      minimum_nights <= 30
    ),
  aes(x = minimum_nights)
) +
  geom_histogram(binwidth = 1) +
  labs(
    title = "Distribution of Minimum-Night Requirements",
    x = "Minimum Nights",
    y = "Number of Listings"
  ) +
  theme_minimal()

#room typoe

ggplot(
  listings,
  aes(x = room_type)
) +
  geom_bar() +
  labs(
    title = "Distribution of Airbnb Room Types",
    x = "Room Type",
    y = "Number of Listings"
  ) +
  theme_minimal() +
  theme(
    axis.text.x =
      element_text(angle = 30, hjust = 1)
  )


#Accommodation capacity

ggplot(
  listings,
  aes(x = accommodates)
) +
  geom_bar() +
  labs(
    title = "Distribution of Accommodation Capacity",
    x = "Number of Guests Accommodated",
    y = "Number of Listings"
  ) +
  theme_minimal()


#create guest segment 
listings_seg <- listings |>
  filter(!is.na(accommodates)) |>
  mutate(
    guest_segment = case_when(
      accommodates <= 2 ~ "Solo / Couple",
      accommodates <= 4 ~ "Small Group / Family",
      accommodates <= 6 ~ "Large Group",
      accommodates >= 7 ~ "Very Large Group"
    )
  )

#check how many listings belong to each segment
listings_seg |>
  count(guest_segment) |>
  arrange(desc(n))

#Calculate estimated occupancy

occupancy <- calendar |>
  group_by(listing_id) |>
  summarise(
    total_days = n(),
    unavailable_days = sum(available == "f", na.rm = TRUE),
    estimated_occupancy = unavailable_days / total_days
  )

#Check the estimated occupancy with listing information

analysis_q3 <- listings_seg |>
  left_join(occupancy, by = c("id" = "listing_id"))

#check the result
head(analysis_q3) 



# QUESTION 4 

## Q4 - Data Preparation 

library(tidyverse)

## 1. Load data
listings <- read_csv("listings_airbnb.csv", guess_max = 30000)

## 2. Confirm no duplicate IDs
sum(duplicated(listings$id))

## 3. Check missing data for key variables
listings %>%
  select(price, bedrooms, bathrooms, review_scores_rating) %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100))

## 4. Clean and log-transform price
listings <- listings %>%
  mutate(
    price_num = parse_number(price),
    log_price = log(price_num)
  )

## 5.  Median imputation grouped by property_type
listings <- listings %>%
  group_by(property_type) %>%
  mutate(
    bedrooms = ifelse(is.na(bedrooms), median(bedrooms, na.rm = TRUE), bedrooms),
    bathrooms = ifelse(is.na(bathrooms), median(bathrooms, na.rm = TRUE), bathrooms),
    review_scores_rating = ifelse(is.na(review_scores_rating),
                                   median(review_scores_rating, na.rm = TRUE),
                                   review_scores_rating)
  ) %>%
  ungroup()


>>>>>>> 28829d2c7af39da62eff5a2762d15e7c5ab03c6c
