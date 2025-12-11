
### Monsoon & Dry Month Exposure for Giardia Study
### Author: Pearl Ante-Testard
### Description: Computes, for each child, the number of monsoon and dry months from birth to measurement.

### ------------------------------------------------------------------
### Clear Environment & Load Packages
### ------------------------------------------------------------------

# Clear all existing objects
rm(list = ls(all = TRUE))

# Load project configuration (e.g. packages, paths)
source(here::here("R/0-config.R"))


### ------------------------------------------------------------------
### Load & Prepare Data
### ------------------------------------------------------------------

# Load Protozoa Data (Giardia) 
# Wide format
# Restrict to index children with non-missing age and valid treatment arms
washb_protozoa <- read.csv(here("data/untouched/washb-bangladesh-protozoa-public.csv"),
                           stringsAsFactors = TRUE) %>%
  filter(indexchild == "Index child",
         !is.na(agedays),
         tr %in% c("WSH", "Nutrition", "Nutrition + WSH", "Control")) %>%
  mutate(tr = factor(tr)) %>%
  select(dataid, clusterid, block, collectweek, collectyear, labmonth,
         indexchild, posgi, delta_prot, sex, agedays, tr)

# Load Anthropometry Data (contains DOB) 
# Long format but we only need the DOB so we can discard the duplicates
# Keep only unique target children with non-missing age
washb_anthro <- read.csv(here("data/untouched/washb-bangladesh-anthro-public.csv"),
                         stringsAsFactors = TRUE) %>%
  filter(tchild == "Target child", !is.na(aged)) %>%
  distinct(dataid, .keep_all = TRUE) %>%
  select(dataid, childid, clusterid, block, tchild, dob)

# Merge Protozoa, Anthropometry and Diarrhea Data
df_protozoa_anthro <- left_join(washb_protozoa, washb_anthro,
                                by = c("dataid", "clusterid", "block")) %>%
  mutate(dob = as.Date(dob, format = "%d%b%Y"))  # Convert DOB to Date format


### ------------------------------------------------------------------
### Calculate Time Variables (Collection Date, Weeks, and Months)
### ------------------------------------------------------------------

df_protozoa_anthro <- df_protozoa_anthro %>%
  mutate(
    collection_date = dob + days(agedays),
    days_from_birth = time_length(interval(dob, collection_date), unit = "days"),
    weeks_from_birth = time_length(interval(dob, collection_date), unit = "weeks"),
    months_from_birth = time_length(interval(dob, collection_date), unit = "months")
  )

# Estimate collection_date only where it's missing
df_protozoa_anthro <- df_protozoa_anthro %>%
  mutate(
    collection_date = if_else(
      is.na(collection_date) & !is.na(collectweek) & !is.na(collectyear),
      as.Date(paste0(collectyear, "-01-01")) + weeks(collectweek - 1),
      collection_date
    )
  )

# Estimate Date of Birth for rows with missing DOBs based on Age and Collection Date
# 5 children had missing DOBs
df_protozoa_anthro <- df_protozoa_anthro %>%
  mutate(
    dob = if_else(is.na(dob),
                  collection_date - days(agedays),
                  dob)
  )


### ------------------------------------------------------------------
### Define Monsoon Periods (Known Rainy Season Windows)
### ------------------------------------------------------------------

monsoon_periods <- data.frame(
  #start = as.Date(c("2012-08-08", "2013-04-27", "2014-05-27", "2015-04-01", "2016-05-12")),
  start = as.Date(c("2012-05-12", "2013-04-27", "2014-05-27", "2015-04-01", "2016-05-12")),
  #end   = as.Date(c("2012-09-30", "2013-10-06", "2014-09-27", "2015-09-26", "2016-06-26"))
  end   = as.Date(c("2012-10-09", "2013-10-06", "2014-09-27", "2015-09-26", "2016-06-26"))
)

# The monsoon periods are based on Grembi et al., 2024: determined the rainy season empirically,
# defined as the continuous period during which the 5-day rolling average of daily precipitation was >= 10mm/day
# Average precipitation data for Bangladesh: in Box (untouched/ppt-daily-averages)

### ------------------------------------------------------------------
### Count Monsoon Months (Months that fall fully or partially in monsoon periods)
### ------------------------------------------------------------------

# Function to count number of weeks overlapping monsoon periods
# Counted the weeks of monsoon months first and then converted to months so that 
# we will have a continuous variable for monsoon 
count_monsoon_months <- function(start_date, end_date) {
  # Return 0 if either start_date or end_date is missing
  if (is.na(start_date) || is.na(end_date)) return(0)
  
  # Create a sequence of daily dates between start_date and end_date
  day_seq <- seq(
    from = as.Date(start_date),  # start date
    to   = as.Date(end_date),    # end date
    by   = "day"                 # increment by days
  )
  
  # Initialize a vector to store overlapping days
  overlapping_days <- c()
  
  # Loop over each monsoon period to find overlaps
  for (i in seq_len(nrow(monsoon_periods))) {
    monsoon_start <- as.Date(monsoon_periods$start[i])  # start of monsoon period
    monsoon_end   <- as.Date(monsoon_periods$end[i])    # end of monsoon period
    
    # Identify days that fall within this monsoon period
    overlap <- day_seq[day_seq >= monsoon_start & day_seq <= monsoon_end]
    
    # Add them to the list of overlapping days
    overlapping_days <- c(overlapping_days, overlap)
  }
  
  # Calculate the total number of unique overlapping days
  total_overlapping_days <- length(unique(overlapping_days))
  
  # Convert days to weeks (assuming 7 days per week)
  #overlapping_weeks <- total_overlapping_days / 7
  
  # Convert weeks to months (assuming 4.345 weeks per month on average)
  #overlapping_months <- overlapping_weeks / 4.345
  
  # Convert to months
  overlapping_months <- total_overlapping_days / 30.44
  
  # Return the number of overlapping months
  overlapping_months
}

df_protozoa_anthro <- df_protozoa_anthro %>%
  mutate(
    # Apply monsoon week counter for each child using dob and collection_date
    monsoon_months = map2_dbl(dob, collection_date, count_monsoon_months),
    
  )


### ------------------------------------------------------------------
### Binary Monsoon 
### ------------------------------------------------------------------
# Define Monsoon Periods (specific to each year)
monsoon_periods <- data.frame(
  year = 2012:2016,  # Yearly mapping
  #start = as.Date(c("2012-08-08", "2013-04-27", "2014-05-27", "2015-04-01", "2016-05-12")),
  start = as.Date(c("2012-05-12", "2013-04-27", "2014-05-27", "2015-04-01", "2016-05-12")),
  #end   = as.Date(c("2012-09-30", "2013-10-06", "2014-09-27", "2015-09-26", "2016-06-26"))
  end   = as.Date(c("2012-10-09", "2013-10-06", "2014-09-27", "2015-09-26", "2016-06-26"))
)

df_protozoa_anthro <- df_protozoa_anthro %>%
  mutate(
    collection_date = as.Date(collection_date),        # Ensure collection_date is in Date format
    collection_year = year(collection_date)            # Extract the year from collection_date
  ) %>%
   #collection_year is used to join the child’s data with the correct monsoon window for that year.
   left_join(monsoon_periods, by = c("collection_year" = "year")) %>%
  # This joins each row to the appropriate monsoon period for the matching year using a left join.
  mutate(
    monsoon = if_else(
      !is.na(collection_date) &                     # Make sure collection_date is available
        collection_date >= start & collection_date <= end,  # Check if it falls within the monsoon range
      1, 0                                           # Assign 1 = monsoon, 0 = non-monsoon
    ),
    monsoon = factor(monsoon, levels = c(0, 1))      # Convert to factor 
  ) %>%
  select(-start, -end, -collection_year)  # Optional cleanup: remove helper columns

# Check the monsoon months using the collectweek and collectyear
df_check_monsoon <- df_protozoa_anthro %>%
                    mutate(monsoon = ifelse(collectyear == "2016" & 
                            collectweek >= 19 & # May 12-June 26, 2016
                            collectweek <= 25, 1,
                          ifelse(collectyear == "2015" & 
                                   collectweek >= 14 & # April 1-September 26, 2015
                                   collectweek <= 39, 1, 0)),
                    monsoon = factor(monsoon, levels = c(0, 1)))

# Check the distribution of the binary monsoon variable
table(df_protozoa_anthro$monsoon)
#  0    1 
#1630  643 
table(df_check_monsoon$monsoon)
#   0    1 
#1609  664 
# slight difference: the above df_protozoa_anthro has 21 less children in monsoon than the df_check_monsoon
# this is probably because the df_check_monsoon is based on collectweek and collectyear, while the df_protozoa_anthro is based on more precise 
# estimation of collection_date


### ------------------------------------------------------------------
### Count Dry Months (Months that do NOT overlap any monsoon period)
### ------------------------------------------------------------------

# Counted the weeks of monsoon months first and then converted to months so that 
# we will have a continuous variable for monsoon 
count_dry_months <- function(start_date, end_date) {
  # Return NA if either start_date or end_date is missing
  if (is.na(start_date) || is.na(end_date)) return(NA_integer_)
  
  # Create a sequence of daily dates between start_date and end_date
  day_seq <- seq(
    from = as.Date(start_date),  # start date
    to   = as.Date(end_date),    # end date
    by   = "day"                 # increment by days
  )
  
  # Identify days that do NOT overlap with any monsoon period
  dry_days <- day_seq[!sapply(day_seq, function(day) {
    # Check if this day overlaps with any monsoon period
    any(mapply(function(ms, me) {
      day >= as.Date(ms) && day <= as.Date(me)  # overlaps if within monsoon period
    }, monsoon_periods$start, monsoon_periods$end))
  })]
  
  # Calculate the total number of dry days
  total_dry_days <- length(unique(dry_days))
  
  # Convert days to weeks (assuming 7 days per week)
  #dry_weeks <- total_dry_days / 7
  
  # Convert weeks to months (assuming 4.345 weeks per month on average)
  #dry_months <- dry_weeks / 4.345
  
  # Convert days to months
  dry_months <- total_dry_days / 30.44
  
  # Return the number of dry weeks
  #dry_weeks
  
  # Return the number of dry months
  dry_months
}

df_protozoa_anthro <- df_protozoa_anthro %>%
  mutate(
    # Count dry weeks between dob and collection_date
    dry_months = map2_dbl(dob, collection_date, count_dry_months),
  )


# Check the dry months using the difference between months_from_birth and monsoon_months
# The values are similar with negligible differences
df_protozoa_anthro <- df_protozoa_anthro %>%
  mutate(
    dry_months_check = months_from_birth - monsoon_months
  )

### ------------------------------------------------------------------
### Check the correlations between monsoon and dry months
### ------------------------------------------------------------------

# Check the correlation between dry_months and dry_months_check
cor(df_protozoa_anthro$dry_months, df_protozoa_anthro$dry_months_check, use = "complete.obs")
plot(df_protozoa_anthro$dry_months, df_protozoa_anthro$dry_months_check)
# 0.999932
# Check the correlation between monsoon_months and dry_months
cor(df_protozoa_anthro$monsoon_months, df_protozoa_anthro$dry_months, use = "complete.obs")
plot(df_protozoa_anthro$monsoon_months, df_protozoa_anthro$dry_months)
# -0.5294356
cor(df_protozoa_anthro$monsoon_months, df_protozoa_anthro$dry_months_check, use = "complete.obs")
plot(df_protozoa_anthro$monsoon_months, df_protozoa_anthro$dry_months_check)
# -0.5295355


### ------------------------------------------------------------------
### Final Output
### ------------------------------------------------------------------

# View summary of computed columns
df_protozoa_anthro %>%
  select(dataid, dob, collection_date, months_from_birth, monsoon_months, dry_months_check, monsoon, dry_months) %>%
  head()

# Save the final dataset
saveRDS(df_protozoa_anthro, here("data/final/exposure-monsoon-protozoa-2015-16-anthrosvy2-latest-2.rds"))
write_csv2(df_protozoa_anthro, here("data/final/exposure-monsoon-protozoa-2015-16-anthrosvy2-latest-2.csv"))

           