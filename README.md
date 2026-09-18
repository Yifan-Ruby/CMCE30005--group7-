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


# Q4 - Data Preparation 

library(tidyverse)

# 1. Load data
listings <- read_csv("listings_airbnb.csv", guess_max = 30000)

# 2. Confirm no duplicate IDs
sum(duplicated(listings$id))

# 3. Check missing data for key variables
listings %>%
  select(price, bedrooms, bathrooms, review_scores_rating) %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100))

# 4. Clean and log-transform price
listings <- listings %>%
  mutate(
    price_num = parse_number(price),
    log_price = log(price_num)
  )

# 5.  Median imputation grouped by property_type
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
