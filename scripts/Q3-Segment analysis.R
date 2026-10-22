#Data import
library(dplyr)
library(tidyverse)
library(lubridate)

listings = read_csv("listings_airbnb.csv")
calendar = read_csv("calendar_airbnb.csv")
reviews = read_csv("reviews_airbnb.csv")

#check initial data strucures
glimpse(listings)
glimpse(calendar)

# Summary of important variables
summary(listings$bedrooms)
summary(listings$accommodates)
summary(listings$minimum_nights)
summary(listings$room_type)

#check missing values
missing_summary <- listings |>
  summarise(
    total_listings = n(),
    
    missing_bedrooms =
      sum(is.na(bedrooms)),
    
    missing_accommodates =
      sum(is.na(accommodates)),
    
    missing_minimum_nights =
      sum(is.na(minimum_nights)),
    
    missing_room_type =
      sum(is.na(room_type)),
    
    bedroom_missing_pct =
      mean(is.na(bedrooms)) * 100,
    
    accommodates_missing_pct =
      mean(is.na(accommodates)) * 100,
    
    minimum_nights_missing_pct =
      mean(is.na(minimum_nights)) * 100,
    
    room_type_missing_pct =
      mean(is.na(room_type)) * 100
  )

missing_summary

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
    unavailable_days = sum(available == FALSE, na.rm = TRUE),
    estimated_occupancy = unavailable_days / total_days
  )

# Check occupancy calculation
head(occupancy)

summary(occupancy$estimated_occupancy)

#Check the estimated occupancy with listing information

analysis_q3 <- listings_seg |>
  left_join(occupancy, by = c("id" = "listing_id"))

#check the result
head(analysis_q3) 
dim(analysis_q3)
summary(analysis_q3$estimated_occupancy)

#----EDA od estimated occupancy----#

#Distribution of estimated occupancy
ggplot(
  analysis_q3 |>
    filter(!is.na(estimated_occupancy)),
  aes(x = estimated_occupancy)
) +
  geom_histogram(
    binwidth = 0.05
  ) +
  labs(
    title = "Distribution of Estimated Occupancy",
    x = "Estimated Occupancy",
    y = "Number of Listings"
  ) +
  theme_minimal()

#Occupancy by bedrooms
ggplot(
  analysis_q3 |>
    filter(
      !is.na(bedrooms),
      bedrooms >= 1,
      bedrooms <= 5,
      !is.na(estimated_occupancy)
    ),
  aes(
    x = factor(bedrooms),
    y = estimated_occupancy
  )
) +
  geom_boxplot() +
  labs(
    title = "Estimated Occupancy by Number of Bedrooms",
    x = "Number of Bedrooms",
    y = "Estimated Occupancy"
  ) +
  theme_minimal()

#Occupancy by accommodation capacity

ggplot(
  analysis_q3 |>
    filter(
      !is.na(accommodates),
      accommodates <= 10,
      !is.na(estimated_occupancy)
    ),
  aes(
    x = factor(accommodates),
    y = estimated_occupancy
  )
) +
  geom_boxplot() +
  labs(
    title = "Estimated Occupancy by Accommodation Capacity",
    x = "Number of Guests Accommodated",
    y = "Estimated Occupancy"
  ) +
  theme_minimal()

#Occupancy by room type

ggplot(
  analysis_q3 |>
    filter(
      !is.na(room_type),
      !is.na(estimated_occupancy)
    ),
  aes(
    x = room_type,
    y = estimated_occupancy
  )
) +
  geom_boxplot() +
  labs(
    title = "Estimated Occupancy by Room Type",
    x = "Room Type",
    y = "Estimated Occupancy"
  ) +
  theme_minimal() +
  theme(
    axis.text.x =
      element_text(
        angle = 30,
        hjust = 1
      )
  )

#create minimum-stay segment

analysis_q3 <- analysis_q3 |>
  mutate(
    stay_type = case_when(
      
      minimum_nights <= 2 ~
        "Short stay (1-2 nights)",
      
      minimum_nights <= 6 ~
        "Medium stay (3-6 nights)",
      
      minimum_nights <= 28 ~
        "Longer stay (7-28 nights)",
      
      minimum_nights >= 29 ~
        "Long-term (29+ nights)",
      
      TRUE ~ NA_character_
    )
  )
# Check number of listings in each stay category
analysis_q3 |>
  count(
    stay_type,
    sort = TRUE
  )

#guest segment performence

segment_summary <- analysis_q3 |>
  filter(
    !is.na(guest_segment),
    !is.na(estimated_occupancy)
  ) |>
  group_by(guest_segment) |>
  summarise(
    
    number_of_listings = n(),
    
    median_occupancy =
      median(
        estimated_occupancy,
        na.rm = TRUE
      ),
    
    mean_occupancy =
      mean(
        estimated_occupancy,
        na.rm = TRUE
      ),
    
    median_bedrooms =
      median(
        bedrooms,
        na.rm = TRUE
      ),
    
    median_minimum_nights =
      median(
        minimum_nights,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  ) |>
  arrange(desc(median_occupancy))


segment_summary

#Visualise guest segment performance
ggplot(
  analysis_q3 |>
    filter(
      !is.na(guest_segment),
      !is.na(estimated_occupancy)
    ),
  aes(
    x = guest_segment,
    y = estimated_occupancy
  )
) +
  geom_boxplot() +
  labs(
    title = "Estimated Occupancy by Target Market Segment",
    x = "Target Market Segment",
    y = "Estimated Occupancy"
  ) +
  theme_minimal() +
  theme(
    axis.text.x =
      element_text(
        angle = 25,
        hjust = 1
      )
  )


# Median occupancy bar chart
ggplot(
  segment_summary,
  aes(
    x = reorder(
      guest_segment,
      median_occupancy
    ),
    y = median_occupancy
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Median Estimated Occupancy by Target Segment",
    x = "Target Market Segment",
    y = "Median Estimated Occupancy"
  ) +
  theme_minimal()

#bedroom performence
bedroom_summary <- analysis_q3 |>
  filter(
    !is.na(bedrooms),
    bedrooms >= 1,
    bedrooms <= 5,
    !is.na(estimated_occupancy)
  ) |>
  group_by(bedrooms) |>
  summarise(
    
    number_of_listings = n(),
    
    median_occupancy =
      median(
        estimated_occupancy,
        na.rm = TRUE
      ),
    
    mean_occupancy =
      mean(
        estimated_occupancy,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  ) |>
  arrange(desc(median_occupancy))


bedroom_summary


# Visualise bedroom performance
ggplot(
  bedroom_summary,
  aes(
    x = factor(bedrooms),
    y = median_occupancy
  )
) +
  geom_col() +
  labs(
    title = "Median Estimated Occupancy by Bedrooms",
    x = "Number of Bedrooms",
    y = "Median Estimated Occupancy"
  ) +
  theme_minimal()

#minimun stay perfomence

stay_summary <- analysis_q3 |>
  filter(
    !is.na(stay_type),
    !is.na(estimated_occupancy)
  ) |>
  group_by(stay_type) |>
  summarise(
    
    number_of_listings = n(),
    
    median_occupancy =
      median(
        estimated_occupancy,
        na.rm = TRUE
      ),
    
    mean_occupancy =
      mean(
        estimated_occupancy,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  ) |>
  arrange(desc(median_occupancy))


stay_summary


# Visualise minimum-stay performance
ggplot(
  analysis_q3 |>
    filter(
      !is.na(stay_type),
      !is.na(estimated_occupancy)
    ),
  aes(
    x = stay_type,
    y = estimated_occupancy
  )
) +
  geom_boxplot() +
  labs(
    title = "Estimated Occupancy by Minimum-Stay Requirement",
    x = "Minimum-Stay Requirement",
    y = "Estimated Occupancy"
  ) +
  theme_minimal() +
  theme(
    axis.text.x =
      element_text(
        angle = 30,
        hjust = 1
      )
  )

#guest segmnet and bedrooms combination

combo_summary <- analysis_q3 |>
  filter(
    !is.na(guest_segment),
    !is.na(bedrooms),
    bedrooms >= 1,
    bedrooms <= 5,
    !is.na(estimated_occupancy)
  ) |>
  group_by(
    guest_segment,
    bedrooms
  ) |>
  summarise(
    
    number_of_listings = n(),
    
    median_occupancy =
      median(
        estimated_occupancy,
        na.rm = TRUE
      ),
    
    mean_occupancy =
      mean(
        estimated_occupancy,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  ) |>
  
  # Avoid interpreting very small groups
  filter(number_of_listings >= 20) |>
  
  arrange(desc(median_occupancy))


combo_summary


# Top 10 combinations
top_combo <- combo_summary |>
  slice_max(
    order_by = median_occupancy,
    n = 10
  )

top_combo


# Visualise guest segment + bedrooms
ggplot(
  combo_summary,
  aes(
    x = factor(bedrooms),
    y = median_occupancy,
    fill = guest_segment
  )
) +
  geom_col(
    position = "dodge"
  ) +
  labs(
    title = "Estimated Occupancy by Guest Segment and Bedrooms",
    x = "Number of Bedrooms",
    y = "Median Estimated Occupancy",
    fill = "Guest Segment"
  ) +
  theme_minimal()

# Compare occupancy distributions by guest segment + bedrooms

# Keep the same combinations with at least 20 listings
combo_hist_data <- analysis_q3 |>
  semi_join(
    combo_summary,
    by = c("guest_segment", "bedrooms")
  ) |>
  filter(!is.na(estimated_occupancy))

ggplot(
  combo_hist_data,
  aes(x = estimated_occupancy)
) +
  geom_histogram(
    aes(y = after_stat(density)),
    binwidth = 0.05,
    boundary = 0,
    fill = "steelblue",
    colour = "white"
  ) +
  facet_wrap(
    vars(guest_segment, bedrooms),
    labeller = label_both,
    ncol = 3
  ) +
  scale_x_continuous(
    labels = scales::label_percent(accuracy = 1),
    breaks = seq(0, 1, 0.25)
  ) +
  coord_cartesian(xlim = c(0, 1)) +
  labs(
    title = "Estimated Occupancy by Guest Segment and Bedrooms",
    subtitle = "Only combinations with at least 20 listings",
    x = "Estimated Occupancy",
    y = "Density"
  ) +
  theme_minimal()


# Select the two combinations shown in Figure Q3.1
combo_hist_data <- analysis_q3 |>
  filter(
    !is.na(estimated_occupancy),
    (guest_segment == "Small Group / Family" & bedrooms == 2) |
      (guest_segment == "Solo / Couple" & bedrooms == 1)
  ) |>
  mutate(
    profile = case_when(
      guest_segment == "Small Group / Family" ~
        "Small Group / Family\n2 bedrooms",
      guest_segment == "Solo / Couple" ~
        "Solo / Couple\n1 bedroom"
    )
  )

ggplot(
  combo_hist_data,
  aes(x = estimated_occupancy)
) +
  geom_histogram(
    aes(y = after_stat(density)),
    binwidth = 0.05,
    boundary = 0,
    closed = "left",
    fill = "#306B8C",
    colour = "white"
  ) +
  facet_wrap(~profile, ncol = 2) +
  scale_x_continuous(
    labels = scales::label_percent(accuracy = 1),
    breaks = seq(0, 1, 0.25)
  ) +
  coord_cartesian(xlim = c(0, 1)) +
  labs(
    x = "Calendar unavailability",
    y = "Density"
  ) +
  theme_minimal(base_size = 15)

#gust degmnet and minimun stay combination

segment_stay_summary <- analysis_q3 |>
  filter(
    !is.na(guest_segment),
    !is.na(stay_type),
    !is.na(estimated_occupancy)
  ) |>
  group_by(
    guest_segment,
    stay_type
  ) |>
  summarise(
    
    number_of_listings = n(),
    
    median_occupancy =
      median(
        estimated_occupancy,
        na.rm = TRUE
      ),
    
    mean_occupancy =
      mean(
        estimated_occupancy,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  ) |>
  
  filter(number_of_listings >= 20) |>
  
  arrange(desc(median_occupancy))


segment_stay_summary

# Figure Q3.2 — Capacity and minimum-night heatmap

heatmap_data <- segment_stay_summary |>
  mutate(
    guest_segment = factor(
      guest_segment,
      levels = c(
        "Solo / Couple",
        "Small Group / Family",
        "Large Group",
        "Very Large Group"
      )
    ),
    stay_type = factor(
      stay_type,
      levels = c(
        "Short stay (1-2 nights)",
        "Medium stay (3-6 nights)",
        "Longer stay (7-28 nights)",
        "Long-term (29+ nights)"
      ),
      labels = c(
        "1-2 nights",
        "3-6 nights",
        "7-28 nights",
        "29+ nights"
      )
    )
  )

ggplot(
  heatmap_data,
  aes(
    x = stay_type,
    y = guest_segment,
    fill = median_occupancy
  )
) +
  geom_tile(colour = "white", linewidth = 1) +
  geom_text(
    aes(
      label = paste0(
        scales::percent(median_occupancy, accuracy = 0.1),
        "\nn=",
        scales::comma(number_of_listings)
      )
    ),
    size = 4.1
  ) +
  scale_fill_gradient(
    low = "#EDF4F7",
    high = "#367A9D",
    limits = c(0, 1),
    guide = "none"
  ) +
  labs(
    x = "Host minimum-night requirement",
    y = "Capacity-based segment"
  ) +
  theme_minimal(base_size = 15) +
  theme(panel.grid = element_blank())


# Figure Q3.3 — Forward versus historical measures

# Calculate historical modelled occupancy by guest segment
historical_summary <- analysis_q3 |>
  filter(!is.na(estimated_occupancy_l365d)) |>
  group_by(guest_segment) |>
  summarise(
    historical_rate =
      median(estimated_occupancy_l365d) / 365,
    .groups = "drop"
  )

# Combine the two measures
comparison_data <- segment_summary |>
  select(
    guest_segment,
    forward_rate = median_occupancy
  ) |>
  left_join(
    historical_summary,
    by = "guest_segment"
  ) |>
  pivot_longer(
    cols = c(forward_rate, historical_rate),
    names_to = "measure",
    values_to = "rate"
  ) |>
  mutate(
    guest_segment = factor(
      guest_segment,
      levels = c(
        "Solo / Couple",
        "Small Group / Family",
        "Large Group",
        "Very Large Group"
      )
    ),
    measure = factor(
      measure,
      levels = c("forward_rate", "historical_rate"),
      labels = c(
        "Forward calendar unavailability",
        "Historical modelled occupancy"
      )
    )
  )

ggplot(
  comparison_data,
  aes(x = guest_segment, y = rate)
) +
  geom_col(fill = "#306B8C", width = 0.6) +
  geom_text(
    aes(
      label = scales::percent(rate, accuracy = 0.1)
    ),
    vjust = -0.5,
    size = 4.1
  ) +
  facet_wrap(~measure, ncol = 2) +
  scale_x_discrete(
    labels = c(
      "Solo / Couple" = "1-2",
      "Small Group / Family" = "3-4",
      "Large Group" = "5-6",
      "Very Large Group" = "7+"
    )
  ) +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 1),
    limits = c(0, 1),
    breaks = seq(0, 1, 0.25)
  ) +
  labs(
    x = "Advertised guest capacity",
    y = "Median rate"
  ) +
  theme_minimal(base_size = 15) +
  theme(panel.grid.minor = element_blank())

#Full taeget marjet segmnet performence

full_segment_summary <- analysis_q3 |>
  filter(
    !is.na(guest_segment),
    !is.na(bedrooms),
    !is.na(stay_type),
    !is.na(estimated_occupancy),
    bedrooms >= 1,
    bedrooms <= 5
  ) |>
  
  group_by(
    guest_segment,
    bedrooms,
    stay_type
  ) |>
  
  summarise(
    
    number_of_listings = n(),
    
    median_occupancy =
      median(
        estimated_occupancy,
        na.rm = TRUE
      ),
    
    mean_occupancy =
      mean(
        estimated_occupancy,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  ) |>
  
  # Only retain segments with enough observations
  filter(number_of_listings >= 20) |>
  
  arrange(desc(median_occupancy))


full_segment_summary


 # ------------------------------------------------------------
# Top 10 inferred target-market profiles
# ------------------------------------------------------------

top_segments <- full_segment_summary |>
  slice_max(
    order_by = median_occupancy,
    n = 10
  )

top_segments

#Kruskal_wallis test-guest segment

kruskal_guest <- kruskal.test(
  estimated_occupancy ~ guest_segment,
  data = analysis_q3
)

kruskal_guest

#pairwise test-guest segment
pairwise_guest <- pairwise.wilcox.test(
  analysis_q3$estimated_occupancy,
  analysis_q3$guest_segment,
  p.adjust.method = "BH"
)

pairwise_guest


#Kruskal_wallis test-guest segment-number of bedrooms
bedroom_test_data <- analysis_q3 |>
  filter(
    !is.na(bedrooms),
    bedrooms >= 1,
    bedrooms <= 5,
    !is.na(estimated_occupancy)
  ) |>
  mutate(
    bedroom_group =
      factor(bedrooms)
  )


kruskal_bedroom <- kruskal.test(
  estimated_occupancy ~ bedroom_group,
  data = bedroom_test_data
)

kruskal_bedroom

#Kruskal_wallis test-guest segment-minium stay
stay_test_data <- analysis_q3 |>
  filter(
    !is.na(stay_type),
    !is.na(estimated_occupancy)
  )


kruskal_stay <- kruskal.test(
  estimated_occupancy ~ stay_type,
  data = stay_test_data
)

kruskal_stay

#final result table
final_q3_table <- full_segment_summary |>
  select(
    guest_segment,
    bedrooms,
    stay_type,
    number_of_listings,
    median_occupancy,
    mean_occupancy
  ) |>
  arrange(desc(median_occupancy))


final_q3_table

#final key output
# Overall guest segment performance
segment_summary

# Bedroom performance
bedroom_summary

# Minimum-stay performance
stay_summary

# Guest segment + bedroom performance
combo_summary

# Guest segment + stay requirement
segment_stay_summary

# Full inferred target-market profiles
full_segment_summary

# Top target-market profiles
top_segments

# Statistical tests
kruskal_guest
pairwise_guest
kruskal_bedroom
kruskal_stay


