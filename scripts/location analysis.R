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