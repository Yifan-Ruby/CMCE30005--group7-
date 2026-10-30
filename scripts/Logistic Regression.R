library(writexl)
library(broom)
library(knitr)


# classifying function for amenities
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


# identified items not matched by the classifying function
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


# export the unmatched amenities into a csv for ease of reading and sorting
write_csv(unmatched, "unmatched.csv")
#########################################

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

###

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
####

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
      # Extract digits from any matching "X space(s)" string in the group
      matches <- unlist(str_extract_all(amenity_clean, "\\d+\\s*(?:space|spaces)"))
      if (length(matches) == 0) {
        NA_real_
      } else {
        as.numeric(str_extract(matches[1], "\\d+"))
      }
    },
    .groups = "drop"
  )

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

skim(amenity_counts)
summary(amenity_counts$total_amenities)

# DATA FRAME

# Create all amenities and occupancy data frame for logistic regression
cat_reg_df <- amenity_counts %>%
  select(-total_amenities) %>%
  left_join(listings_clean %>% select(id, 
                                      estimated_occupancy_l365d, 
                                      price_quote_price_per_night,
                                      property_type,
                                      neighbourhood_cleansed), by = "id") %>%
  filter(!is.na(price_quote_price_per_night)) %>%
  mutate(
    high_occupancy_rate = as.integer(
      ifelse((estimated_occupancy_l365d)/365>=0.60, 1, 0)
    ),
    log_price_pnight = log(price_quote_price_per_night)) %>%
  select(-estimated_occupancy_l365d, -price_quote_price_per_night)




# REGRESSION FORMULA
## Create regression formula for categorical amentity regression model
### Reorder the factor levels from most frequent to least frequent 

cat_reg_df <- cat_reg_df %>%
  mutate(
    # fct_infreq() automatically puts the most common category first
    property_type = fct_infreq(as.factor(property_type)),
    neighbourhood_cleansed = fct_infreq(as.factor(neighbourhood_cleansed))
  )

### Identify predictor variables
cat_predictor_names <- setdiff(names(cat_reg_df), c("id", "high_occupancy_rate"))

### Stitch them together into a standard formula string
cat_formula_string <- paste("high_occupancy_rate ~", paste(cat_predictor_names, collapse = " + "))

### Convert to a formal formula object
cat_regression_formula <- as.formula(cat_formula_string)

print(cat_regression_formula) # "+" need to be manually replaced with "|" in front of "neighbourhood_cleansed"

# REGRESSION #######################

set.seed(123) # Set seed for reproducibility

### Create the 80/20 split (Stratified by suburb so all suburbs are represented)
cat_data_split <- initial_split(cat_reg_df, prop = 0.8, strata = neighbourhood_cleansed)

cat_train_data <- training(cat_data_split)
cat_test_data  <- testing(cat_data_split)

### Train the model using the 80% split
cat_fe_model <- feglm(high_occupancy_rate ~ technology_amenities + kitchen_amenities + 
                        workspace_amenities + laundry_amenities + climate_amenities + 
                        bathroom_amenities + bedroom_amenities + security_access_amenities + 
                        outdoor_amenities + parking_ev_amenities + recreation_amenities + 
                        family_children_amenities + cleaning_housekeeping_amenities + 
                        food_services_amenities + beach_water_amenities + property_accessibility_amenities + 
                        pets_smoking_amenities + view_amenities +
                        + i(property_type, ref = "Entire rental unit")  + log_price_pnight | 
                        neighbourhood_cleansed,
                      data = cat_train_data, 
                      family = "binomial")

summary(cat_fe_model)


# TABLE FOR ODD RATIOS IN EXCEL
# Extract summary table into a clean dataframe
cat_odds_ratios <- tidy(cat_fe_model) %>%
  mutate(
    odds_ratio = exp(estimate), # e^beta = Odds Ratio
    # Calculate 95% Confidence Intervals for Odds Ratios
    or_low  = exp(estimate - 1.96 * std.error),
    or_high = exp(estimate + 1.96 * std.error)
  ) %>%
  select(term, estimate, odds_ratio, or_low, or_high, std.error, p.value) %>%
  mutate(
    # Format p-value to non-scientific notation (e.g., "0.000000...")
    p.value = format(p.value, scientific = FALSE, digits = 3),
    estimate = round(estimate, 3),
    std.error = round(std.error, 3)
  )


# Save directly to Excel
write_xlsx(cat_odds_ratios, "cat_regression_results.xlsx")


# GRAPH FOR SIGNIFICANT AMENITIES TYPES
plot_data <- modelplot(cat_fe_model, exponentiate = TRUE, draw = FALSE)

significant_amenities <- plot_data %>% 
  filter(
    !grepl("property_type", term),        # Omit all property type rows
    p.value < 0.05,                       # Keep only statistically significant rows (p < 0.05)
    term != "(Intercept)"                 # Omit the intercept if it's there
  ) %>% 
  # Clean up messy variable names (removes the "_amenities" suffix for a cleaner axis)
  mutate(term = gsub("_amenities", "", term))


ggplot(significant_amenities, aes(y = reorder(term, estimate), x = estimate)) +
  # Add the 'no effect' reference line at 1.0 for odds ratios
  geom_vline(xintercept = 1, linetype = "dashed", color = "red", size = 0.8) +
  
  # Add the confidence intervals
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), width = 0.2, color = "black") +
  
  # Add the calculated odds ratio center points
  geom_point(size = 3, color = "darkblue") +
  
  # Displays exact Odds Ratio values rounded to 2 decimal places
  geom_text(aes(label = round(estimate, 2)), 
            vjust = -0.5,                   # Positions text safely above the marker
            color = "darkblue", 
            fontface = "bold",
            size = 5) +
  
  theme_minimal() +
  
  theme(
    axis.text.y = element_text(
      size = 14,        
    ),
      axis.title = element_text(
      size = 14,         
      face = "bold"
    ),
    axis.text.x = element_blank(),        
    axis.ticks.x = element_blank(),       
    panel.grid.minor.x = element_blank(),  
    panel.grid.major.x = element_blank()
  ) +
  labs(
    title = "Impact of Statitically Significant Amenities on Occupancy (Odds Ratios)",
    x = "Odds Ratio (Relative to Baseline)",
    y = "Amenity Type"
  )

# GRAPH FOR NON SIGNIFICANT AMENITIES TYPES

non_sig_amenities <- plot_data %>% 
  filter(
    !grepl("property_type", term),        # Omit all property type rows
    p.value >= 0.05,                       # Keep only non statistically significant rows (p < 0.05)
    term != "(Intercept)"                 # Omit the intercept if it's there
  ) %>% 
  # Clean up messy variable names (removes the "_amenities" suffix for a cleaner axis)
  mutate(term = gsub("_amenities", "", term))


ggplot(non_sig_amenities, aes(y = reorder(term, estimate), x = estimate)) +
  # Add the 'no effect' reference line at 1.0 for Odds Ratios
  geom_vline(xintercept = 1, linetype = "dashed", color = "red", size = 0.8) +
  
  # Add the confidence intervals 
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), width = 0.2, color = "black") +
  
  # Add the calculated odds ratio center points
  geom_point(size = 3, color = "darkblue") +
  
  # Displays exact Odds Ratio values rounded to 2 decimal places
  geom_text(aes(label = round(estimate, 2)), 
            vjust = -0.5,                   # Positions text safely above the marker
            color = "darkblue", 
            fontface = "bold",
            size = 5) +
  
  theme_minimal() +
  
  theme(
    axis.text.y = element_text(
      size = 14,          
    ),
    
    axis.title = element_text(
      size = 14,          
      face = "bold"
    ),
    axis.text.x = element_blank(),        
    axis.ticks.x = element_blank(),       
    panel.grid.minor.x = element_blank(), 
    panel.grid.major.x = element_blank()
  ) +
  labs(
    title = "Impact of Non Statitically Significant Amenities on Occupancy (Odds Ratios)",
    x = "Odds Ratio (Relative to Baseline)",
    y = "Amenity Type"
  )



library(modelsummary)

# GRAPHS FOR PEROPERTY TYPES
# Keep only 'property_type' where p-value is LESS than 0.05
significant_properties <- plot_data %>% 
  filter(
    grepl("property_type", term),  # Retain only property type categories
    p.value < 0.05                 # Retain only statistically significant outcomes
  ) %>% 
  # Clean up the messy "property_type" prefix from the labels
  mutate(term = gsub("property_type::", "", term))

# Graph the filtered significant subset with text labels
ggplot(significant_properties, aes(y = reorder(term, estimate), x = estimate)) +
  # Add the 'no effect' reference line at 1.0 for Odds Ratios
  geom_vline(xintercept = 1, linetype = "dashed", color = "red", size = 0.8) +
  
  # Add the confidence intervals 
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, color = "black") +
  
  # Add the calculated Odds Ratio center points
  geom_point(size = 3, color = "darkblue") +
  
  # Displays the rounded odds ratio right above each dot
  geom_text(aes(label = round(estimate, 2)), 
            vjust = -0.5,             # Pushes the text slightly above the dot
            color = "darkblue", 
            fontface = "bold",
            size = 5) +
  
  theme_minimal() +
  
  # Remove x-axis number and ticks
  theme(
    axis.text.y = element_text(
      size = 14,        
    ),
  
    axis.title = element_text(
      size = 14,        
      face = "bold"
    ),
    axis.text.x = element_blank(),   # Removes the numbers on the xaxis
    axis.ticks.x = element_blank(),  # Removes the little tick marks on the x axis
    panel.grid.minor.x = element_blank(), # Cleans up background grid lines
    panel.grid.major.x = element_blank()
  ) +
  labs(
    title = "Statistically Significant Property Types Impacting Occupancy (p < 0.05)",
    subtitle = "Odds Ratio > 1 increase occupancy probability relative to baseline",
    x = "Odds Ratio (Relative to Baseline)",
    y = "Property Type"
  )


# GRAPH FOR PROPERTY TYPE BASED ON THEIR CHARACTERISITCS
## Extract the coefficients
coeff_data <- data.frame(
  raw_name = names(coef(cat_fe_model)),
  raw_estimate = coef(cat_fe_model)
)

# Filter, categorize, and convert to Odds Ratios
coeff_clean <- coeff_data %>%
  filter(grepl("property_type::", raw_name)) %>%
  mutate(
    property_type_clean = gsub("property_type::", "", raw_name),
    clean_type = tolower(property_type_clean),
    
    odds_ratio = exp(raw_estimate),
    
    category = case_when(
      grepl("shared room", clean_type) ~ "Shared Room",
      grepl("private room|entire guest suite", clean_type) ~ "Room",
      grepl("entire house|entire guesthouse|entire place|entire serviced apartment|entire home|entire vacation home|entire townhouse|entire villa", clean_type) ~ "Individual place",
      TRUE ~ "Novelty"
    )
  )

# Create the strictly uniform odds ratio histograms
ggplot(coeff_clean, aes(x = odds_ratio, fill = category)) +
  geom_histogram(bins = 15, color = "white", alpha = 0.85) +
  
  # Reference line at 1.0 (Baseline level)
  geom_vline(xintercept = 1, linetype = "dashed", color = "red", size = 0.8) +
  
  # REMOVED scales = "free_y" to enforce uniform heights and y-axis limits across all groups
  facet_wrap(~category) + 
  
  # Log scaling balances out extreme right-side skews
  scale_x_log10(labels = function(x) sprintf("%g", x)) +
  scale_fill_brewer(palette = "Set1") +
  
  labs(
    title = "Distribution of Property Type Odds Ratios",
    x = "Odds Ratio (Values > 1 mean higher probability of high occupancy)",
    y = "Count of Property Subcategories"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 12),
    plot.title = element_text(face = "bold", size = 14),
    
    # Makes every individual facet panel a uniform square shape
    aspect.ratio = 1 
  )


# GRAPH FOR ALL PROPERTY TYPES
# Keep all 'property_type's
all_properties <- plot_data %>% 
  filter(
    grepl("property_type", term),  # Retain only property type categories
  ) %>% 
  # Clean up the messy "property_type" prefix from the labels
  mutate(term = gsub("property_type::", "", term))

# Graph the filtered significant subset with text labels
cat_all_property_plot <- ggplot(all_properties, aes(y = reorder(term, estimate), x = estimate)) +
  # Add the 'no effect' reference line at 1.0 for Odds Ratios
  geom_vline(xintercept = 1, linetype = "dashed", color = "red", size = 0.8) +
  
  # Add the confidence intervals
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, color = "black") +
  
  # Add the calculated odds ratio center points
  geom_point(size = 3, color = "darkblue") +
  
  # Displays the rounded odds ratio right above each dot
  geom_text(aes(label = round(estimate, 2)), 
            vjust = -0.5,             # Pushes the text slightly above the dot
            color = "darkblue", 
            fontface = "bold",
            size = 5) +
  
  theme_minimal() +
  
  # Remove axis numberand ticks
  theme(
    axis.text.y = element_text(
      size = 14,    
    ),
    axis.title = element_text(
      size = 14,
      face = "bold"
    ),
    axis.text.x = element_blank(),  
    axis.ticks.x = element_blank(), 
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  labs(
    title = "Statistically Significant Property Types Impacting Occupancy (p < 0.05)",
    subtitle = "Odds Ratio > 1 increase occupancy probability relative to baseline",
    x = "Odds Ratio (Relative to Baseline)",
    y = "Property Type"
  )


print(cat_all_property_plot)


# MODEL EVALUATION

# 1. Dynamically identify all property types used during training
trained_properties <- unique(cat_train_data$property_type)

# 2. Filter the test data to include ONLY those trained property types
# This safely drops the rare un-modeled categories causing the crash
cat_test_data_clean <- cat_test_data %>% 
  filter(property_type %in% trained_properties)

# 3. Generate your predicted probabilities!
cat_test_data_clean$pred_prob <- predict(
  cat_fe_model, 
  newdata = cat_test_data_clean, 
  type = "response"
)

summary(cat_test_data_clean$pred_prob)

test_preds <- predict(cat_fe_model, newdata = cat_test_data_clean, type = "response")


actual_y <- cat_test_data_clean$high_occupancy_rate

# 3. EVALUATE & CLASSIFY USING OPTIMAL THRESHOLD

# A. ROC-AUC
roc_obj <- roc(actual_y, test_preds)
test_auc <- pROC::auc(roc_obj)
cat("Test ROC-AUC:", round(test_auc, 4), "\n")

# Plot ROC Curve (with reduced margins to prevent display errors)
par(mar = c(4, 4, 2, 1))
plot(roc_obj, main = paste("Test Set ROC Curve (AUC =", round(test_auc, 3), ")"))

# B. Brier Score
brier_score <- mean((test_preds - actual_y)^2)
cat("Test Brier Score:", round(brier_score, 4), "\n")

# C. Find Optimal Threshold (Youden's J)
best_coords <- coords(roc_obj, "best", best.method = "youden")
optimal_threshold <- as.numeric(best_coords$threshold)
cat("Optimal Decision Threshold:", round(optimal_threshold, 4), "\n")

# D. Classification Matrix at Optimal Threshold
binary_preds_opt <- ifelse(test_preds >= optimal_threshold, 1, 0)
conf_matrix_opt <- table(Predicted = binary_preds_opt, Actual = actual_y)

cat("\n--- Balanced Confusion Matrix ---\n")
print(conf_matrix_opt)

# Balanced Accuracy Calculation
sensitivity <- conf_matrix_opt[2, 2] / sum(conf_matrix_opt[, 2]) # Recall (1s)
specificity <- conf_matrix_opt[1, 1] / sum(conf_matrix_opt[, 1]) # True Negative Rate (0s)
cat("\nSensitivity (Recall for 1s):", round(sensitivity, 4))
cat("\nSpecificity (Rate for 0s):", round(specificity, 4), "\n")


# TESTING DIFFERENT THRESHOLDS
# Define a vector of thresholds to test
threshold_list <- c(0.05, round(optimal_threshold, 4), 0.10, 0.15, 0.20, 0.25, 0.30)

# Function to compute metrics at a given threshold
eval_threshold <- function(thresh, preds, actual) {
  binary_preds <- ifelse(preds >= thresh, 1, 0)
  
  # Confusion matrix components
  tp <- sum(binary_preds == 1 & actual == 1)
  fp <- sum(binary_preds == 1 & actual == 0)
  tn <- sum(binary_preds == 0 & actual == 0)
  fn <- sum(binary_preds == 0 & actual == 1)
  
  # Metrics
  accuracy    <- (tp + tn) / (tp + fp + tn + fn)
  sensitivity <- ifelse((tp + fn) > 0, tp / (tp + fn), 0) # Recall
  specificity <- ifelse((tn + fp) > 0, tn / (tn + fp), 0) # True Negative Rate
  precision   <- ifelse((tp + fp) > 0, tp / (tp + fp), 0) # Positive Predictive Value
  f1_score    <- ifelse((precision + sensitivity) > 0, 2 * (precision * sensitivity) / (precision + sensitivity), 0)
  
  return(data.frame(
    Threshold   = thresh,
    Accuracy    = round(accuracy, 4),
    Sensitivity = round(sensitivity, 4),
    Specificity = round(specificity, 4),
    Precision   = round(precision, 4),
    F1_Score    = round(f1_score, 4),
    True_Pos    = tp,
    False_Pos   = fp
  ))
}

# Iterate over all thresholds and bind into a clean data frame
metrics_table <- do.call(rbind, lapply(threshold_list, eval_threshold, preds = test_preds, actual = actual_y))

# Print clean formatted table
cat("\n=================== THRESHOLD EVALUATION TABLE ===================\n\n")
print(kable(metrics_table, format = "simple", col.names = c("Threshold", "Accuracy", "Sensitivity (Recall)", "Specificity", "Precision", "F1 Score", "True Pos (TP)", "False Pos (FP)")))





