# Load Libraries ------------------------

# Install specific versions
install.packages("data.table")  # Will install latest version (1.16.4)
install.packages("rlang")       # Will install latest version (1.1.4)

# Load these packages first
library(data.table)
library(rlang)
packageVersion("data.table")
packageVersion("rlang")


# Function to install and load packages 
package_install = function(package_list) {
  for (pkg in package_list) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      tryCatch({
        install.packages(pkg)
      }, error = function(e) {
        message(paste0("Could not install ", pkg, ": ", e))
      })
    }
    # Load the library
    library(pkg, character.only = TRUE)
  }
}

# List of packages
packages = c('tidyverse', 'haven', 'qgcomp', 'dplyr', 'ggplot2', 'moments', 
             'naniar', 'VIM', 'missForest', 'tidymodels', 'purrr', 'gridExtra', 
             'knitr', 'infotheo', 'igraph', 'ggraph', 'lubridate', 'zoo', 
             'ggridges', 'slider', 'stats', 'reshape2', 'broom', 'gridExtra',
             'modelsummary', 'sandwich', 'igraph', 'ggraph', 
             'arrow', 'patchwork', 'lme4', 'performance')

# Call the package_install function 
package_install(packages)

# This snippet assumes that you run setup first

# This code copies a file from your Google Bucket into a dataframe

# replace 'test.csv' with the name of the file in your google bucket (don't delete the quotation marks)
name_of_file_in_bucket = 'mvp_2d_0725.csv'


# DON'T CHANGE FROM HERE

# Get the bucket name
my_bucket = Sys.getenv('WORKSPACE_BUCKET')

# Copy the file from current workspace to the bucket
system(paste0("gsutil cp ", my_bucket, "/data/", name_of_file_in_bucket, " ."), intern=T)

# Load the file into a dataframe
cope_auo  = read_csv(name_of_file_in_bucket)
head(cope_auo)

# Load the parquet file
parquet_filename = 'df_consensus_with_distributions_wald_final.parquet'
system(paste0("gsutil cp ", my_bucket, "/data/", parquet_filename, " ."), intern=T)
conditions_df = read_parquet(parquet_filename)
head(conditions_df)
str(conditions_df) # 413,360 in df_consensus_with_distributions_wald_final.parquet

# Loading stringency index 
stringency_filename = "2d_stringency_by_exposure.RData"

# Get the bucket name from the environment
my_bucket = Sys.getenv('WORKSPACE_BUCKET')

# Copy the .RData file from the bucket to your local working directory
system(paste0("gsutil cp ", my_bucket, "/data/", stringency_filename, " ."), intern = TRUE)

load("2d_stringency_by_exposure.RData")

# Changing name of file and removing empty string value in state
stringency_averages = my_file[state != ""]


# Creating stringency objects
stringency_march_15_june_30 = stringency_averages[, .(state, stringency_march_15_june_30)]
stringency_march_22_may_23  = stringency_averages[, .(state, stringency_march_22_may_23)]
stringency_april_12_may_23 = stringency_averages[, .(state, stringency_april_12_may_23)]

# Converting character columns to factors
cope_auo$RegionName           = as.factor(cope_auo$RegionName)
cope_auo$gender               = as.factor(cope_auo$gender)
cope_auo$has_health_insurance = as.factor(cope_auo$has_health_insurance)
cope_auo$have_job             = as.factor(cope_auo$have_job)
cope_auo$college_graduated    = as.factor(cope_auo$college_graduated)
cope_auo$has_health_insurance = as.factor(cope_auo$has_health_insurance)
cope_auo$age_bin              = as.factor(cope_auo$age_bin)
cope_auo$household_income_cat = as.factor(cope_auo$household_income_cat)
cope_auo$race                 = as.factor(cope_auo$race)
cope_auo$zip_code             = as.factor(cope_auo$zip_code)
cope_auo$home_ownership       = as.factor(cope_auo$home_ownership)

# Change column names to use underscores instead of spaces
colnames(cope_auo) = gsub(" ", "_", colnames(cope_auo))

# --- NEW CODE START ---
# Define the filename for the combined exposure data (an .RData file)
exposure_filename = "combined_exposure_data_final_average.RData"

# Get the bucket name from the environment
my_bucket = Sys.getenv('WORKSPACE_BUCKET')

# Copy the .RData file from the bucket to your local working directory
system(paste0("gsutil cp ", my_bucket, "/data/", exposure_filename, " ."), intern = TRUE)

# Check that the file was copied correctly (optional)
print(list.files())

# Load the RData file into your environment.
# The load() function returns a character vector with the names of the objects loaded.
load(exposure_filename)

combined_exposure_data = combined_exposure_data_clean

# Preprocessing ----------------------------------------------------------------

# Function to process and rename exposure data
process_exposure_data = function(period_key, suffix) {
  # Find keys that start with the given period_key
  key_match = grep(paste0("^", period_key), names(combined_exposure_data), value = TRUE)
  
  # Check if a matching key was found
  if (length(key_match) == 0) {
    stop("No matching key for ", period_key, " found in combined_exposure_data")
  }
  
  # Use the first matching key (assuming there is only one that fits)
  combined_exposure_data[[ key_match[1] ]]$data %>%
    select(person_id, 
           C1M_School_closing,
           C2M_Workplace_closing,
           C3M_Cancel_public_events,
           C4M_Restrictions_on_gatherings,
           C5M_Close_public_transport,
           C6M_Stay_at_home_requirements,
           C7M_Restrictions_on_internal_movement) %>%
    rename_with(~ paste0(., suffix), -person_id)
}

# Simple function to check policy ranges 
check_policy_ranges = function(data) {
  # Identify policy columns
  policy_cols = grep("C[1-7]M", names(data), value = TRUE)
  
  # Print header
  cat("\n=== COVID Policy Range Check ===\n")
  
  # Check each policy column
  all_integer = TRUE
  
  for (col in policy_cols) {
    values = data[[col]]
    min_val = min(values, na.rm = TRUE)
    max_val = max(values, na.rm = TRUE)
    is_integer = all(values == round(values), na.rm = TRUE)
    
    # Get value counts
    value_counts = table(values)
    count_text = paste(paste0(names(value_counts), ":", value_counts), collapse=", ")
    
    # If any non-integer values are found, update the flag
    if (!is_integer) {
      all_integer = FALSE
    }
    
    # Print the results
    cat(sprintf("%-50s: Range [%g-%g], Integer-only: %s\n", 
                col, min_val, max_val, 
                ifelse(is_integer, "Yes", "No")))
  }
  
  # Overall result
  cat("\nAll policies have integer-only values: ", 
      ifelse(all_integer, "YES", "NO"), "\n\n")
}

# Process exposure window 
cope_auo_20200412_20200523 = process_exposure_data(
  "20200412_20200523", 
  "_April_12_May_23"
)

# Checking policy ranges 
check_policy_ranges(cope_auo_20200412_20200523)

# Merge exposure data into main dataframe
cope_auo_windows = cope_auo %>%
  left_join(cope_auo_20200412_20200523, by = "person_id") 

# Verify new columns
message("New columns added:")
print(colnames(cope_auo_windows))

# Checking structure of cope_auo data with windows added
str(cope_auo_windows)

# Ensure each row is a unique individual
cope_auo_unique = cope_auo_windows %>%
  distinct(person_id, .keep_all = TRUE)

# Creating a merged dataset using columns of interest from cope_auo_unique and conditions_df
# Note that conditions_df contains the propensity score values and some covariates of interest
cope_auo_ps = cope_auo_unique %>%
  select(
    person_id,
    RegionName,
    StringencyIndex_Average_202005,
    gad_score_avg_july,
    date_of_birth,
    age_2020,
    C1M_School_closing,
    C2M_Workplace_closing,
    C3M_Cancel_public_events,
    C4M_Restrictions_on_gatherings,
    C5M_Close_public_transport,
    C6M_Stay_at_home_requirements,
    C7M_Restrictions_on_internal_movement,
    C1M_School_closing_April_12_May_23,
    C2M_Workplace_closing_April_12_May_23, 
    C3M_Cancel_public_events_April_12_May_23,
    C4M_Restrictions_on_gatherings_April_12_May_23, 
    C5M_Close_public_transport_April_12_May_23, 
    C6M_Stay_at_home_requirements_April_12_May_23, 
    C7M_Restrictions_on_internal_movement_April_12_May_23,
    gad_score_july,
    gad_score_avg_may,
    gad_score_may,
    gad_score_avg_june,
    gad_score_june
  ) %>%
  inner_join(
    conditions_df %>% 
      select(
        person_id,
        anxiety_disorder_before_covid_lifetime,
        generalized_anxiety_disorder_before_covid_lifetime,
        anxiety_disorder_1yr_before_covid,
        generalized_anxiety_disorder_1yr_before_covid,
        response_prob,
        observation_wt,
        observation_wt_trim,
        sex,
        education,
        employment,
        insurance,
        home,
        income,
        maritalstatus,
        race,
        birthplace
      ),
    by = "person_id"
  ) %>%
  mutate(
    log_observation_wt = log(observation_wt)
  )


# Check uniqueness in cope_auo_unique (should be TRUE because of distinct())
n_unique_cope = n_distinct(cope_auo_unique$person_id)
n_total_cope = nrow(cope_auo_unique)
all_unique_cope = n_unique_cope == n_total_cope

cat("cope_auo_unique:\n",
    "- Total rows:", n_total_cope, "\n",
    "- Unique person_ids:", n_unique_cope, "\n",
    "- All person_ids unique?:", ifelse(all_unique_cope, "Yes", "No"), "\n\n")

# Check uniqueness in conditions_df
n_unique_conditions = n_distinct(conditions_df$person_id)
n_total_conditions = nrow(conditions_df)
all_unique_conditions = n_unique_conditions == n_total_conditions

cat("conditions_df:\n",
    "- Total rows:", n_total_conditions, "\n",
    "- Unique person_ids:", n_unique_conditions, "\n",
    "- All person_ids unique?:", ifelse(all_unique_conditions, "Yes", "No"), "\n\n")

# Determine overlap between the two datasets:
ids_cope = unique(cope_auo_unique$person_id)
ids_conditions = unique(conditions_df$person_id)

common_ids = intersect(ids_cope, ids_conditions)
n_common = length(common_ids)

only_in_cope = setdiff(ids_cope, ids_conditions)
n_only_in_cope =length(only_in_cope)

only_in_conditions = setdiff(ids_conditions, ids_cope)
n_only_in_conditions = length(only_in_conditions)

cat("Overlap between cope_auo_unique and conditions_df:\n",
    "- Common person_ids:", n_common, "\n",
    "- IDs only in cope_auo_unique:", n_only_in_cope, "\n",
    "- IDs only in conditions_df:", n_only_in_conditions, "\n")


# Count unique participants across geographic locations
participant_counts = cope_auo_ps %>%
  count(RegionName) %>%
  filter(!is.na(RegionName))  # Remove NA values from RegionName

print(participant_counts, n = 50)

# Determining completed and missing values for each outcome window (May, June, July)
print(
  cope_auo_ps %>%
    summarise(
      total_rows = n(),
      complete_may = sum(!is.na(gad_score_avg_may)),
      missing_may = sum(is.na(gad_score_avg_may)),
      percent_complete_may = mean(!is.na(gad_score_avg_may)) * 100,
      percent_missing_may = mean(is.na(gad_score_avg_may)) * 100,
      complete_june = sum(!is.na(gad_score_avg_june)),
      missing_june = sum(is.na(gad_score_avg_june)),
      percent_complete_june = mean(!is.na(gad_score_avg_june)) * 100,
      percent_missing_june = mean(is.na(gad_score_avg_june)) * 100,
      complete_july = sum(!is.na(gad_score_avg_july)),
      missing_july = sum(is.na(gad_score_avg_july)),
      percent_complete_july = mean(!is.na(gad_score_avg_july)) * 100,
      percent_missing_july = mean(is.na(gad_score_avg_july)) * 100
    ),
  width = Inf
)

# Removing any rows with missing values in exposures 
cope_auo_ps = cope_auo_ps %>%
  filter(
    !is.na(C1M_School_closing) &
      !is.na(C2M_Workplace_closing) &
      !is.na(C3M_Cancel_public_events) &
      !is.na(C4M_Restrictions_on_gatherings) &
      !is.na(C5M_Close_public_transport) &
      !is.na(C6M_Stay_at_home_requirements) &
      !is.na(C7M_Restrictions_on_internal_movement)
  )

# Subset for May
cope_auo_may = cope_auo_ps %>%
  filter(!is.na(gad_score_avg_may))


# Subset for June
cope_auo_june = cope_auo_ps %>%
  filter(!is.na(gad_score_avg_june))

# Subset for July
cope_auo_july = cope_auo_ps %>%
  filter(!is.na(gad_score_avg_july))

# Create a function to analyze missing values and complete cases
analyze_missing_values = function(data) {
  # Calculate complete cases
  complete_rows = data %>%
    select(insurance, income, employment, 
           education, sex, age_2020) %>%
    complete.cases() %>%
    sum()
  
  # Calculate percentage of complete cases
  total_rows = nrow(data)
  percent_complete = (complete_rows / total_rows) * 100
  
  # Original missing values analysis
  missing_summary = data %>%
    summarise(
      total_rows = n(),
      complete_rows = complete_rows,
      percent_complete_rows = percent_complete,
      
      # Health Insurance
      missing_health_insurance = sum(is.na(insurance)),
      percent_missing_health_insurance = mean(is.na(insurance)) * 100,
      
      # Household Income
      missing_household_income = sum(is.na(income)),
      percent_missing_household_income = mean(is.na(income)) * 100,
      
      # Employment
      missing_employment = sum(is.na(employment)),
      percent_missing_employment = mean(is.na(employment)) * 100,
      
      # Education
      missing_education = sum(is.na(education)),
      percent_missing_education = mean(is.na(education)) * 100,
      
      # Gender
      missing_gender = sum(is.na(sex)),
      percent_missing_gender = mean(is.na(sex)) * 100,
      
      # Age
      missing_age = sum(is.na(age_2020)),
      percent_missing_age = mean(is.na(age_2020)) * 100
    )
  
  return(missing_summary)
}

# Analyze missing values for each time period
cat("\nMissing values analysis for full unique dataset:\n")
analyze_missing_values(cope_auo_ps)

cat("\nMissing values analysis for May subset:\n")
analyze_missing_values(cope_auo_may)

cat("\nMissing values analysis for June subset:\n")
analyze_missing_values(cope_auo_june)

cat("\nMissing values analysis for July subset:\n")
analyze_missing_values(cope_auo_july)

# Additional detailed analysis for July subset
july_complete_cases = cope_auo_july %>%
  select(insurance, income, employment, 
         education, sex, age_2020) %>%
  complete.cases()

cat("Total rows in July subset:", nrow(cope_auo_july), "\n")
cat("Complete rows (no missing values):", sum(july_complete_cases), "\n")
cat("Incomplete rows (at least one missing value):", sum(!july_complete_cases), "\n")
cat("Percentage complete:", round(mean(july_complete_cases) * 100, 2), "%\n")

# Create a visualization of missing data patterns
#missing_patterns = cope_auo_unique %>%
#select(insurance, income, employment, 
#education, sex, age_2020) %>%
#miss_var_summary()

# Plot missing value patterns
#ggplot(missing_patterns, 
#aes(x = reorder(variable, n_miss), 
#y = pct_miss)) +
#geom_bar(stat = "identity", fill = "steelblue") +
#coord_flip() +
#theme_minimal() +
#labs(title = "Percentage of Missing Values by Variable",
#x = "Variable",
#y = "Percent Missing (%)")

# Histogram plot of the distribution of GAD Average Scores across outcome windows 

# Outcome window #1 - GAD Score (July 2020)
ggplot(cope_auo_ps, aes(x = gad_score_avg_july)) +
  geom_histogram(binwidth = 0.2, fill = "#2ba2ce", color = "black") +
  theme_classic() +
  labs(title = "Distribution of GAD Score Average - July 2020",
       x = "GAD Score Average",
       y = "Frequency") +
  geom_vline(aes(xintercept = median(gad_score_avg_july)), 
             color = "#ce572b", linetype = "dashed", linewidth = 1) +
  annotate("text", x = median(cope_auo_unique$gad_score_avg_july), 
           y = 1000, label = "Median", color = "#ce572b", angle = 90, vjust = -0.5) +
  geom_vline(aes(xintercept = mean(gad_score_avg_july)), 
             color = "black", linetype = "dashed", linewidth = 1) +
  annotate("text", x = mean(cope_auo_unique$gad_score_avg_july), 
           y = 1000, label = "Mean", color = "black", angle = 90, vjust = -0.5)


# Outcome window #2 - GAD Score (May 2020)
ggplot(cope_auo_may, aes(x = gad_score_avg_may)) +
  geom_histogram(binwidth = 0.2, fill = "#2ba2ce", color = "black") +
  theme_classic() +
  labs(title = "Distribution of GAD Score Average - May 2020",
       x = "GAD Score Average",
       y = "Frequency") +
  geom_vline(aes(xintercept = median(gad_score_avg_may)), 
             color = "#ce572b", linetype = "dashed", linewidth = 1) +
  annotate("text", x = median(cope_auo_may$gad_score_avg_may), 
           y = 1000, label = "Median", color = "#ce572b", angle = 90, vjust = -0.5) +
  geom_vline(aes(xintercept = mean(gad_score_avg_may)), 
             color = "black", linetype = "dashed", linewidth = 1) +
  annotate("text", x = mean(cope_auo_may$gad_score_avg_may), 
           y = 1000, label = "Mean", color = "black", angle = 90, vjust = -0.5)

# Outcome window #3 - GAD Score (June 2020)
ggplot(cope_auo_june, aes(x = gad_score_avg_june)) +
  geom_histogram(binwidth = 0.2, fill = "#2ba2ce", color = "black") +
  theme_classic() +
  labs(title = "Distribution of GAD Score Average - June 2020",
       x = "GAD Score Average",
       y = "Frequency") +
  geom_vline(aes(xintercept = median(gad_score_avg_june)), 
             color = "black", linetype = "dashed", linewidth = 1) +
  annotate("text", x = median(cope_auo_june$gad_score_avg_june), 
           y = 1000, label = "Median", color = "black", angle = 90, vjust = -0.5) +
  geom_vline(aes(xintercept = mean(gad_score_avg_june)), 
             color = "red", linetype = "dashed", linewidth = 1) +
  annotate("text", x = mean(cope_auo_june$gad_score_avg_june), 
           y = 1000, label = "Mean", color = "red", angle = 90, vjust = -0.5)

# Checking number of cases of no anxiety (0-4), mild (5-9), moderate (10-14), and severe (15-21) anxiety
# Using the threshold established within the GAD-7 

gad_summary_june = cope_auo_june %>%
  mutate(gad_range = case_when(
    gad_score_june >= 0 & gad_score_june <= 4 ~ "0-4",
    gad_score_june >= 5 & gad_score_june <= 9 ~ "5-9",
    gad_score_june >= 10 & gad_score_june <= 14 ~ "10-14",
    gad_score_june >= 15 & gad_score_june <= 21 ~ "15-21",
    TRUE ~ "Other"
  )) %>%
  group_by(gad_range) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100) %>%
  arrange(factor(gad_range, levels = c("0-4", "5-9", "10-14", "15-21", "Other")))

# Print the summary
print(gad_summary_june)

# Ensuring plot shows factors in the correct order
gad_summary_june = gad_summary_june %>% mutate(gad_range = factor(gad_range, 
                                                                  levels = c("0-4", "5-9", "10-14", "15-21")))

# Plotting distribution of GAD Scores in the sample 
ggplot(gad_summary_june, aes(x = gad_range, y = count)) +
  geom_bar(stat = "identity", fill = "#642ca6", color = "black") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5, size = 5) +
  theme_classic() +
  labs(title = "Distribution of GAD Scores - June 2020",
       x = "GAD Score Range",
       y = "Count") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),  
    axis.title.x = element_text(size = 14),  
    axis.title.y = element_text(size = 14), 
    axis.text.x  = element_text(size = 12),
    axis.text.y  = element_text(size = 12)
  )


#### Version for July 2020 outcome
gad_summary_july = cope_auo_july %>%
  mutate(gad_range = case_when(
    gad_score_july >= 0 & gad_score_july <= 4 ~ "0-4",
    gad_score_july >= 5 & gad_score_july <= 9 ~ "5-9",
    gad_score_july >= 10 & gad_score_july <= 14 ~ "10-14",
    gad_score_july >= 15 & gad_score_july <= 21 ~ "15-21",
    TRUE ~ "Other"
  )) %>%
  group_by(gad_range) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100) %>%
  arrange(factor(gad_range, levels = c("0-4", "5-9", "10-14", "15-21", "Other")))

# Print the summary
print(gad_summary_july)

# Ensuring plot shows factors in the correct order
gad_summary_july = gad_summary_july %>% mutate(gad_range = factor(gad_range, 
                                                                  levels = c("0-4", "5-9", "10-14", "15-21")))

# Plotting distribution of GAD Scores in the sample 
ggplot(gad_summary_july, aes(x = gad_range, y = count)) +
  geom_bar(stat = "identity", fill = "#642ca6", color = "black") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5, size = 5) +
  theme_classic() +
  labs(title = "Distribution of GAD Scores - July 2020",
       x = "GAD Score Range",
       y = "Count") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),  
    axis.title.x = element_text(size = 14),  
    axis.title.y = element_text(size = 14), 
    axis.text.x  = element_text(size = 12),
    axis.text.y  = element_text(size = 12)
  )



# Remove rows with missing data in specified covariates
cope_auo_july_no_miss = cope_auo_july %>% 
  filter(!is.na(insurance) & 
           !is.na(income) & 
           !is.na(employment) & 
           !is.na(education) & 
           !is.na(sex) & 
           !is.na(age_2020))

# Round ages using round() function
cope_auo_july_no_miss = cope_auo_july_no_miss %>%
  mutate(age_rounded = round(age_2020))

# Check for any individuals under 18 using rounded ages
check_lessthan18 = cope_auo_july_no_miss %>%
  summarise(
    min_age_raw = min(age_2020, na.rm = TRUE),
    min_age_rounded = min(age_rounded, na.rm = TRUE),
    n_under18 = sum(age_rounded < 18, na.rm = TRUE)
  )

cat("Age validation results:\n")
cat("Minimum age in dataset (raw):", check_lessthan18$min_age_raw, "\n")
cat("Minimum age in dataset (rounded):", check_lessthan18$min_age_rounded, "\n")
cat("Number of individuals under 18:", check_lessthan18$n_under18, "\n")

# Create age bins using rounded ages
cope_auo_july_no_miss = cope_auo_july_no_miss %>%
  # Original five-category binning
  mutate(binned_age = factor(case_when(
    age_rounded >= 18 & age_rounded <= 24 ~ "18-24",
    age_rounded >= 25 & age_rounded <= 44 ~ "25-44",
    age_rounded >= 45 & age_rounded <= 64 ~ "45-64",
    age_rounded >= 65 & age_rounded <= 89 ~ "65-89",
    age_rounded >= 90 ~ "90+",
    TRUE ~ NA_character_
  ), levels = c("18-24", "25-44", "45-64", "65-89", "90+"), ordered = TRUE)) %>%
  
  # Three-category solution
  mutate(age_three_cat = factor(case_when(
    age_rounded >= 18 & age_rounded <= 44 ~ "18-44 (Young Adults)",
    age_rounded >= 45 & age_rounded <= 64 ~ "45-64 (Middle-Aged Adults)",
    age_rounded >= 65 ~ "65+ (Older Adults)",
    TRUE ~ NA_character_
  ), levels = c("18-44 (Young Adults)", "45-64 (Middle-Aged Adults)", "65+ (Older Adults)"), ordered = TRUE)) %>%
  
  # Two-category solution
  mutate(age_two_cat = factor(case_when(
    age_rounded >= 18 & age_rounded <= 64 ~ "18-64 (Working-Age Adults)",
    age_rounded >= 65 ~ "65+ (Retirement-Age Adults)",
    TRUE ~ NA_character_
  ), levels = c("18-64 (Working-Age Adults)", "65+ (Retirement-Age Adults)"), ordered = TRUE))

# Check the distribution of the three-category solution
three_cat_distribution = cope_auo_july_no_miss %>%
  group_by(age_three_cat) %>%
  summarise(
    n = n(),
    percent = round(n/nrow(cope_auo_july_no_miss)*100, 2)
  )
cat("\nThree-category age distribution:\n")
print(three_cat_distribution)

# Check the distribution of the two-category solution
two_cat_distribution = cope_auo_july_no_miss %>%
  group_by(age_two_cat) %>%
  summarise(
    n = n(),
    percent = round(n/nrow(cope_auo_july_no_miss)*100, 2)
  )
cat("\nTwo-category age distribution:\n")
print(two_cat_distribution)

# Check for any unassigned ages in both new categorizations
unassigned_three = cope_auo_july_no_miss %>%
  filter(is.na(age_three_cat)) %>%
  summarise(
    n = n(),
    min_age = if(n() > 0) min(age_rounded, na.rm = TRUE) else NA_real_,
    max_age = if(n() > 0) max(age_rounded, na.rm = TRUE) else NA_real_
  )

unassigned_two = cope_auo_july_no_miss %>%
  filter(is.na(age_two_cat)) %>%
  summarise(
    n = n(),
    min_age = if(n() > 0) min(age_rounded, na.rm = TRUE) else NA_real_,
    max_age = if(n() > 0) max(age_rounded, na.rm = TRUE) else NA_real_
  )

cat("\nUnassigned ages check (three-category):\n")
cat("Number of unassigned:", unassigned_three$n, "\n")
if(unassigned_three$n > 0) {
  cat("Age range of unassigned:", unassigned_three$min_age, "to", unassigned_three$max_age, "\n")
}

cat("\nUnassigned ages check (two-category):\n")
cat("Number of unassigned:", unassigned_two$n, "\n")
if(unassigned_two$n > 0) {
  cat("Age range of unassigned:", unassigned_two$min_age, "to", unassigned_two$max_age, "\n")
}

# Create count tables for plotting
three_cat_counts = cope_auo_july_no_miss %>%
  count(age_three_cat)

two_cat_counts = cope_auo_july_no_miss %>%
  count(age_two_cat)

# Plot for three-category solution
plot_three_cat = ggplot(three_cat_counts, aes(x = age_three_cat, y = n)) +
  geom_bar(stat = "identity", fill = "#2ba2ce") +
  geom_text(aes(label = n), vjust = -0.5) +
  labs(x = "Age Range", y = "Number of unique participants", 
       title = "July 2020 Anxiety Cohort - Three Age Categories") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))

# Plot for two-category solution
plot_two_cat = ggplot(two_cat_counts, aes(x = age_two_cat, y = n)) +
  geom_bar(stat = "identity", fill = "#ce572b") +
  geom_text(aes(label = n), vjust = -0.5) +
  labs(x = "Age Range", y = "Number of unique participants", 
       title = "July 2020 Anxiety Cohort - Two Age Categories") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))

# Display both plots
print(plot_three_cat)
print(plot_two_cat)

#  Display plots side by side using patchwork package
# If patchwork is not installed, uncomment and run: # install.packages("patchwork")
library(patchwork)
combined_plot = plot_three_cat + plot_two_cat + 
  plot_layout(ncol = 2) +
  plot_annotation(title = "Age Distribution Comparison",
                  theme = theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")))
print(combined_plot)

print(colnames(stringency_averages))


add_stringency_to_cope = function(cope_data, stringency_index = "march_15_june_30") {
  # Determine which stringency data and column to use based on the stringency_index parameter
  if (stringency_index == "march_15_june_30") {
    stringency_data = stringency_march_15_june_30
    stringency_column = "stringency_march_15_june_30"  # This is the column name in the data
  } else if (stringency_index == "march_22_may_23") {
    stringency_data = stringency_march_22_may_23
    stringency_column = "stringency_march_22_may_23"
  } else if (stringency_index == "april_12_may_23") {
    stringency_data = stringency_april_12_may_23
    stringency_column = "stringency_april_12_may_23"
  } else {
    stop("Invalid stringency_index. Must be one of 'march_15_june_30', 'march_22_may_23', or 'april_12_may_23'.")
  }
  
  # Create a copy of cope_data to avoid modifying the original
  cope_data_w_stringency = cope_data
  
  # Convert RegionName to character for matching
  region_names_char = as.character(cope_data_w_stringency$RegionName)
  
  # Create a lookup table from state name to stringency value
  state_to_stringency = setNames(
    stringency_data[[stringency_column]],
    stringency_data$state
  )
  
  # Check if all region names are in the stringency data
  missing_regions = setdiff(unique(region_names_char), names(state_to_stringency))
  if (length(missing_regions) > 0) {
    warning("Some regions in cope_data do not have matching stringency data: ", 
            paste(missing_regions, collapse = ", "))
  }
  
  # Add the stringency column to cope_data_w_stringency
  # Use the original stringency column name
  cope_data_w_stringency[[stringency_column]] <- 
    state_to_stringency[region_names_char]
  
  return(cope_data_w_stringency)
}

# Calling function to add stringency index to df 
cope_auo_july_no_miss_w_stringency = add_stringency_to_cope(cope_auo_july_no_miss, "april_12_may_23")

# Splitting the data into training (70%) and testing (30%) sets, ensuring the same distribution of gad_score_avg
set.seed(42)

data_split = initial_split(cope_auo_july_no_miss, prop = 0.70, strata = "gad_score_july")

# Extract training and testing sets
train_data = training(data_split)
test_data  = testing(data_split)

# Print the dimensions of the training and testing sets
print(paste0("Dimensions of full sample: ", dim(cope_auo_july_no_miss)[1], " rows and ", dim(cope_auo_july_no_miss)[2], " columns"))
print(paste0("Dimensions of train_data: ", dim(train_data)[1], " rows and ", dim(train_data)[2], " columns"))
print(paste0("Dimensions of test_data: ",  dim(test_data)[1],  " rows and ", dim(test_data) [2], " columns"))

colnames(train_data) = gsub(" ", "_", colnames(train_data))
colnames(test_data) = gsub(" ", "_", colnames(test_data))

# Check the distribution of gad_score_avg in the training set
ggplot(train_data, aes(x = gad_score_july)) +
  geom_histogram(binwidth = 2, fill = "#2ba2ce", color = "black") +
  theme_classic() +
  labs(title = "Distribution of GAD Score Average in Training Set",
       x = "GAD Score Average",
       y = "Frequency")

# Check the distribution of gad_score_avg in the testing set
ggplot(test_data, aes(x = gad_score_july)) +
  geom_histogram(binwidth = 2, fill = "#2ba2ce", color = "black") +
  theme_classic() +
  labs(title = "Distribution of GAD Score Average in Testing Set",
       x = "GAD Score Average",
       y = "Frequency")

# Function to impute missing values using missForest 
train_data_df = as.data.frame(train_data)
test_data_df  = as.data.frame(test_data)

# Generating exposure mixture models with individual exposures (essentially GLM)--------------

#qgcomp.glm.boot models (including individual exposures)

# Assumes a linear and additive relationship between the exposures and the outcome on the link function scale
# e.g. log rate for count outcomes 

# The model is fitted directly using the quantized exposures, without the two-stage approach used in qgcomp.glm.boot
# The interpretation of the model coefficients is conditional on the specific values of the covariates. 

# Counterfactual outcomes are not specifically estimated, as the model directly relates the observed outcomes 
# to the quantized exposures.

# The variance of the mixture effect parameter (psi) is estimated uing the delta method,
# which relies on the assumed linearity and additivity of the model. 

# Estimates a single parameter (psi) representing the average change in the outcome (on the link scale) per
# quantile increase in the joint exposure to all exposures specified in 'expnms'

# Provides weights or standardized coefficients for each exposure, indicating the proportion of the overall 
# mixture effect attributed to each exposure in the positive and negative directions. 
# These weights are meaningful only when the model assumptions of linearity and additivity hold.

# April 5 - May 30 (i.e. week start of May 24) “early” exposure, July 2020 outcome

# EXPOSURE 1 - SCHOOL CLOSING----------------------------------------------------------------------


qgcomp_weights_train = train_data_df$observation_wt_trim 
qgcomp_weights_test  = test_data_df$observation_wt_trim 

qgcomp_weights_total_sample = cope_auo_july_no_miss$observation_wt_trim

C1M_coefficients = qgcomp.glm.noboot(gad_score_july ~ 
                                       age_2020 +
                                       home +
                                       maritalstatus +
                                       race +
                                       education +
                                       employment +
                                       insurance +
                                       anxiety_disorder_before_covid_lifetime +
                                       anxiety_disorder_1yr_before_covid +
                                       sex +
                                       birthplace +
                                       generalized_anxiety_disorder_before_covid_lifetime +
                                       generalized_anxiety_disorder_1yr_before_covid +
                                       #including additional covariates used to generate propensity scores
                                       C1M_School_closing_April_12_May_23,
                                     expnms = c('C1M_School_closing_April_12_May_23'), 
                                     data = cope_auo_july_no_miss, 
                                     weights = qgcomp_weights_total_sample,
                                     q = NULL,
                                     bayes = FALSE) 
length(coef(C1M_coefficients$fit))

# When we print the qgcomp output, the reported standard error for psi1 (the mixture effect)
# is calculated from the default model-based variance–covariance matrix.

# These default standard errors are computed assuming that all observations are independent.
# They are obtained from the diagonal of the variance–covariance matrix produced by the GLM,
# and the delta method is used to calculate the SE for ψ (in this case, for a single coefficient,
# the delta method simply returns sqrt(Var(beta_exposure))).
print(C1M_coefficients)

# Extract the point estimate for psi1 (this remains unchanged using robust correction)
C1M_psi1_est = C1M_coefficients$psi

# Default standard error calculation from model-based variance-covariance matrix
C1M_default_var = C1M_coefficients$var.psi
C1M_default_se = sqrt(C1M_default_var)
cat("Default psi1 SE:", C1M_default_se, "\n")

# Default confidence interval calculation
C1M_default_margin = 1.96 * C1M_default_se
C1M_default_lower  = C1M_psi1_est - C1M_default_margin
C1M_default_upper = C1M_psi1_est + C1M_default_margin
cat("Default 95% CI for psi1: (", C1M_default_lower, ",", C1M_default_upper, ")\n")

# Default test statistics
C1M_default_t = C1M_psi1_est / C1M_default_se
df_model = C1M_coefficients$df  # degrees of freedom from the model
C1M_default_p = 2 * pt(abs(C1M_default_t), df = df_model, lower.tail = FALSE)
cat("Default t-value for psi1:", C1M_default_t, "\n")
cat("Default p-value for psi1:", C1M_default_p, "\n")

##  Cluster-robust error calculations ------------------------------------------------------------------------------
# Calculate cluster-robust variance-covariance matrix accounting for RegionName
C1M_robust_vcov = vcovCL(C1M_coefficients$fit, cluster = cope_auo_july_no_miss$RegionName)

# Extract the robust standard error for the exposure coefficient
C1M_robust_se = sqrt(C1M_robust_vcov["C1M_School_closing_April_12_May_23", 
                                     "C1M_School_closing_April_12_May_23"])
cat("Cluster-robust psi1 SE:", C1M_robust_se, "\n")

# Robust confidence interval calculation
C1M_robust_margin = 1.96 * C1M_robust_se
C1M_robust_lower  = C1M_psi1_est - C1M_robust_margin
C1M_robust_upper  = C1M_psi1_est + C1M_robust_margin
cat("Robust 95% CI for psi1: (", C1M_robust_lower, ",", C1M_robust_upper, ")\n")

# Robust test statistics
C1M_robust_t = C1M_psi1_est / C1M_robust_se
C1M_robust_p = 2 * pt(abs(C1M_robust_t), df = df_model, lower.tail = FALSE)
cat("Robust t-value for psi1:", C1M_robust_t, "\n")
cat("Robust p-value for psi1:", C1M_robust_p, "\n")

## Function Test: Mixture Effect -----------------

object <- copy(C1M_coefficients)
data_eval <- copy(cope_auo_july_no_miss)
observation_wt_col= c("observation_wt_trim")
exposure_names <- c(
  'C1M_School_closing_April_12_May_23'
)


object = model_qgcomp
data_eval = test_data_df
observation_wt_col= c("observation_wt_trim")
exposure_names = exposure_names

## Extract weights
wts <- c(object$pos.weights, object$neg.weights)
wts

## Multiply exposure training weights with exposure columns
data_eval$mixture_score <- rowSums(wts*data_eval[, names(wts), drop = FALSE])

print("Summary of test mixture score column")
print(summary(data_eval$mixture_score))

# Evaluate effect
## pull formula from qgcomp
lhs <- as.character(object$fit$formula)[2]
rhs <- as.character(object$fit$formula)[3]

## Clean rhs string
rhs_clean <- gsub("[\n\r]", " ", rhs)       # Replace line breaks with space
rhs_clean <- gsub("\\s+", " ", rhs_clean) # Collapse multiple spaces
rhs_clean

## Remove exposure terms: to avoid multicollinearity
rhs_terms <- strsplit(rhs_clean, " \\+ ")[[1]]

rhs_new <- if (length(rhs_terms[!rhs_terms %in% exposure_names]) > 0) {
  paste0(" + ", paste(rhs_terms[!rhs_terms %in% exposure_names], collapse = " + "))
} else {
  ""
}

formula_str <- paste0(lhs, "~", "mixture_score" , "+" , rhs_new)
formula_str

## Model glm
# model_test <- lm(data = test_data,formula = formula_str, weights = (observation_wt_col))
model_mix <- eval(substitute(glm(formula = formula_str, family = "gaussian", data = data_eval, weights = W), list(W = as.name(observation_wt_col))))
model_mix

# Return
## return object
eval_object <- list()
eval_object$model <- copy(model_mix)
eval_object$new_data <- copy(data_eval)

## Function Test: HICC ---------------------
fit.object =  model_test
data = test_data_new
cluster_col = "RegionName"


## Check class
if (inherits(fit.object, "lm")) {
  message("This is a lm object.")
} else {
  stop("Pass a valid lm object.")
}

## Check data
if (is.null(data)){
  stop("Pass va valid data object")
}

## Check if cluster_var is a non-empty string of length 1
if (length(cluster_col) == 0 || is.null(cluster_col) || cluster_col == ""){
  stop("cluster_col must be non-empty vector of length 1")
}


# Heteroskedasticity test: if pval is significant, need HC correction (Robust SE)
hc_result <- lmtest::bptest(fit.object) 

# ICC: explains cluster level effects: should be higher than 0.05 (Cluster Robust SE)
model_formula <- formula(fit.object)
lmer_model <- eval(
  substitute(
    lme4::lmer(update(model_formula, . ~ . + (1 | CLUSTER)), data = data),
    list(CLUSTER = as.name(cluster_col))
  )
)

if(lme4::isSingular(lmer_model, tol = 1e-4)){
  message("Model is singular. setting ICC scores to 0")
  
  df <- data.frame(
    hc_p_val = hc_result$p.value,
    icc = 0,
    icc_adj = 0,
    row.names = NULL
  )
  
} else {
  
  icc_result <- performance::icc(lmer_model)
  
  df <- data.frame(
    hc_p_val = hc_result$p.value,
    icc = icc_result$ICC_unadjusted,
    icc_adj = icc_result$ICC_adjusted,
    row.names = NULL
  )
  
}



## Function Test: Robust Errors -------------------------------------------

# Init
fit.object = copy(C1M_coefficients$fit)
cluster_robust = TRUE
robust_type= "CR1"
cluster_var = cope_auo_july_no_miss$RegionName
exposure_names <- c("C1M_School_closing_April_12_May_23")

## Check class
if (inherits(fit.object, "lm")) {
  message("This is a lm object.")
} else {
  stop("Pass a valid lm object.")
}

## Check if robust_type is a non-empty string of length 1
if (length(robust_type) == 0 || is.null(robust_type)) {
  stop("robust_type must be a non-empty vector")
}

## Init types
HC_types <- c("HC0", "HC1", "HC2", "HC3", "HC4", "HC4m", "HC5")
CR_types <- c("CR0", "CR1", "CR1S", "CR2", "CR3")

## Check if robust_type in HC types for cluster_robust = FALSE
if ((cluster_robust == FALSE) & (robust_type %in% CR_types)) {
  stop("Set cluster_robust to TRUE before proceeding with CR types")
}

## Check if robust_type is accurate
if (!(robust_type %in% c(HC_types, CR_types))) {
  stop("Invalid robust_type. Must be one of: ", paste(c(HC_types, CR_types), collapse = ", "))
}

## Check if cluster_var is non-empty
if ((cluster_robust == TRUE) & (length(cluster_var) == 0 || is.null(cluster_var))) {
  stop("cluster_var must be a non-empty vector when cluster_robust = TRUE")
}


### Robust Estimation --------
message("Using vcovHC + manual t-test(df = df.residual))")
hc_vcov <- sandwich::vcovHC(fit.object, type = robust_type)

## Clean: required due to high memory usage
gc()

## Extract robust beta coefficient, p and SE
new_coeff <- fit.object$coefficients[exposure_names]     # same as qgcomp object$psi
new_se <- sqrt(hc_vcov[exposure_names, exposure_names])  # square root of covariance
new_p_val <- 2 * pt(abs(new_coeff/new_se),         # get tstat by coeff/SE
                    df = fit.object$df.residual,         # degree of freedom is residual degree of freedom from model object
                    lower.tail = FALSE)
new_lower_ci <- new_coeff - qnorm(0.975) * new_se
new_upper_ci <- new_coeff + qnorm(0.975) * new_se

### Cluster Robust HC -----------

message("Using vcovCL + manual t-test (df = G - 1)")
cl_vcov <- sandwich::vcovCL(fit.object, cluster = cluster_var, type = robust_type)

## Clean: required due to high memory usage
gc()

## Extract robust beta coefficient, p and SE
new_coeff <- fit.object$coefficients[exposure_names]     # same as qgcomp object$psi
new_se <- sqrt(cl_vcov[exposure_names, exposure_names])  # square root of covariance
new_p_val <- 2 * pt(abs(new_coeff/new_se),         # get stat by coeff/SE
                    df = length(unique(cluster_var)) - 1, # degree of freedom is number of clusters - 1
                    lower.tail = FALSE)
new_lower_ci <- new_coeff - qnorm(0.975) * new_se
new_upper_ci <- new_coeff + qnorm(0.975) * new_se
    
### Cluster Robust CR ------------
message("Using vcovCR + manual t-test")
cl_vcov <- clubSandwich::vcovCR(fit.object, cluster = cluster_var, type = robust_type)

## Clean: required due to high memory usage
gc()

## Extract robust beta coefficient, p and SE
new_coeff <- sum(fit.object$coefficients[exposure_names])     # same as qgcomp object$psi
new_se <- sqrt(sum(cl_vcov[exposure_names, exposure_names, 
                           drop = FALSE]))                    # square root of covariance

## Satterthwaite calc
which_beta <- names(coef(fit.object)) %in% exposure_names
GH_obj <- clubSandwich:::get_GH(fit.object, cl_vcov)          # NOTE: using ::: to access internal function
P_array <- clubSandwich:::get_P_array(GH_obj)                 # NOTE: using ::: to access internal function

## Get projection
P_combined <- array(0, dim = c(dim(P_array)[1:2], 1))
exposure_indices <- which(names(coef(fit.object)) %in% exposure_names)
for (g in seq_len(dim(P_array)[1])) {
  for (h in seq_len(dim(P_array)[2])) {
    P_combined[g, h, 1] <- sum(P_array[g, h, exposure_indices])
  }
}

## Get Satterthwaite df for the combined effect
df_satt <- clubSandwich:::Satterthwaite_df(P_combined)  # NOTE: using ::: to access internal function

## Clean: required due to high memory usage
gc()

## Continue with p val calculations
new_p_val <- 2 * pt(abs(new_coeff/new_se),                    # get stat by coeff/SE
                    df = df_satt,                             # degree of freedom is Satterthwaite df
                    lower.tail = FALSE)
new_lower_ci <- new_coeff - qnorm(0.975) * new_se
new_upper_ci <- new_coeff + qnorm(0.975) * new_se

### Cluster Robust CR + Coef_test ------------------
message("Using vcovCR + coef_test")
cl_vcov <- clubSandwich::vcovCR(eval_object$model, cluster = cluster_var, type = robust_type)
robust_cr2 <- clubSandwich::coef_test(eval_object$model, 
                                      vcov = cl_vcov,
                                      test = "Satterthwaite", 
                                      coefs = c("mixture_score"))

### Clean: required due to high memory usage
gc()


### Extract robust beta coefficient, p and SE
new_coeff <- robust_cr2$beta
new_p_val <- robust_cr2$p_Satt
new_se <- robust_cr2$SE
new_lower_ci <- new_coeff - qnorm(0.975) * new_se
new_upper_ci <- new_coeff + qnorm(0.975) * new_se

#### check manual ---------
message("Using vcovCR + manual t-test")
cl_vcov <- clubSandwich::vcovCR(eval_object$model, cluster = cluster_var, type = robust_type)

## Clean: required due to high memory usage
gc()

## Extract robust beta coefficient, p and SE
new_coeff <- sum(eval_object$model$coefficients[c("mixture_score")])     # same as qgcomp object$psi
new_se <- sqrt(sum(cl_vcov[c("mixture_score"), c("mixture_score"), 
                           drop = FALSE]))                    # square root of covariance

## Satterthwaite calc
which_beta <- names(coef(eval_object$model)) %in% c("mixture_score")
GH_obj <- clubSandwich:::get_GH(eval_object$model, cl_vcov)          # NOTE: using ::: to access internal function
P_array <- clubSandwich:::get_P_array(GH_obj)                 # NOTE: using ::: to access internal function

## Get projection
P_combined <- array(0, dim = c(dim(P_array)[1:2], 1))
exposure_indices <- which(names(coef(eval_object$model)) %in% c("mixture_score"))
for (g in seq_len(dim(P_array)[1])) {
  for (h in seq_len(dim(P_array)[2])) {
    P_combined[g, h, 1] <- sum(P_array[g, h, exposure_indices])
  }
}

## Get Satterthwaite df for the combined effect
df_satt <- clubSandwich:::Satterthwaite_df(P_combined)  # NOTE: using ::: to access internal function

## Clean: required due to high memory usage
gc()

## Continue with p val calculations
new_p_val <- 2 * pt(abs(new_coeff/new_se),                    # get stat by coeff/SE
                    df = df_satt,                             # degree of freedom is Satterthwaite df
                    lower.tail = FALSE)
new_lower_ci <- new_coeff - qnorm(0.975) * new_se
new_upper_ci <- new_coeff + qnorm(0.975) * new_se

### Output df -------------
df <- data.frame(
  beta = new_coeff,
  p_val = new_p_val,
  std_error = new_se,
  lower_ci = new_lower_ci,
  upper_ci = new_upper_ci,
  row.names = NULL
)

df


### Check variance matices ---------
cl_vcov1 <- clubSandwich::vcovCR(eval_object$model, cluster = cluster_var, type = robust_type)
cl_vcov2 <- clubSandwich::vcovCR(fit.object, cluster = cluster_var, type = robust_type)
all.equal(cl_vcov1, cl_vcov2)


## Function Test: Heteroskedasticity and Intra-Class Correlation Coeff ------------------------

# Init
object = copy(C1M_coefficients)
cluster_var = "RegionName"
exposure_names <- c("C1M_School_closing_April_12_May_23")

# Heteroskedasticity test: if pval is significant, need HC correction (Robust SE)
hc_result <- lmtest::bptest(object$fit) 

# ICC: explains cluster level effects: should be higher than 0.05 (Cluster Robust SE)
model_formula <- formula(object$fit)
lmer_model <- eval(
  substitute(
    lme4::lmer(update(model_formula, . ~ . + (1 | CLUSTER)), data = object$fit$data),
    list(CLUSTER = as.name(cluster_var))
  )
)
icc_result <- performance::icc(lmer_model)
icc_result$ICC_adjusted
icc_result$ICC_unadjusted

df <- data.frame(
  expnms = exposure_names,
  hc_p_val = hc_result$p.value,
  icc_p_val = icc_result$ICC_unadjusted,
  icc_p_val_adj = icc_result$ICC_adjusted,
  row.names = NULL
)

df

# EXPOSURE ALL ---------------------------------------------
# Exploratory: non parametric boostrap it to 1000 iterations. Get 95% CI from distributions. Similar to this quantile(auc_stability, probs = c(0.025, 0.975))

qgcomp_all_components = qgcomp.glm.noboot(gad_score_july ~ 
                                            age_2020 +
                                            home +
                                            maritalstatus +
                                            race +
                                            education +
                                            employment +
                                            insurance +
                                            anxiety_disorder_before_covid_lifetime +
                                            anxiety_disorder_1yr_before_covid +
                                            sex +
                                            birthplace +
                                            generalized_anxiety_disorder_before_covid_lifetime +
                                            generalized_anxiety_disorder_1yr_before_covid +
                                            C1M_School_closing_April_12_May_23 +
                                            C2M_Workplace_closing_April_12_May_23 +
                                            C3M_Cancel_public_events_April_12_May_23 +
                                            C4M_Restrictions_on_gatherings_April_12_May_23 +
                                            C5M_Close_public_transport_April_12_May_23 +
                                            C6M_Stay_at_home_requirements_April_12_May_23 +
                                            C7M_Restrictions_on_internal_movement_April_12_May_23,
                                          expnms = c(
                                            'C1M_School_closing_April_12_May_23',
                                            'C2M_Workplace_closing_April_12_May_23',
                                            'C3M_Cancel_public_events_April_12_May_23',
                                            'C4M_Restrictions_on_gatherings_April_12_May_23',
                                            'C5M_Close_public_transport_April_12_May_23',
                                            'C6M_Stay_at_home_requirements_April_12_May_23',
                                            'C7M_Restrictions_on_internal_movement_April_12_May_23'), 
                                          data = cope_auo_july_no_miss, 
                                          weights = qgcomp_weights_total_sample,
                                          q = NULL,
                                          bayes = FALSE) 
print(qgcomp_all_components)

qgcomp_all_components$fit
qgcomp_all_components$psi

# Purpose: default vs cluster-robust inference for the overall mixture effect (ψ₁)
mixture_effect_se = function(qgcomp_fit, cluster = NULL, alpha = 0.05) {
  if (!requireNamespace("sandwich", quietly = TRUE)) {
    stop("Package 'sandwich' is required; install with install.packages('sandwich').")
  }
  ## ---------- basic objects ----------
  psi_hat   = as.numeric(qgcomp_fit$psi)      # overall mixture estimate
  expos     = qgcomp_fit$expnms               # names of mixture exposures
  glm_fit   = qgcomp_fit$fit                  # underlying glm
  beta_cov  = vcov(glm_fit)[expos, expos, drop = FALSE]   # model-based Σβ
  
  ## ---------- default (model-based) SE ----------
  default_var = sum(beta_cov)                 # 1ᵀ Σβ 1   (wᵢ = 1 ∀ i)
  default_se  = sqrt(default_var)
  
  ## ---------- cluster-robust SE ----------
  if (is.null(cluster)) {
    cluster_id = seq_len(nobs(glm_fit))       
  } else {
    if (length(cluster) != nobs(glm_fit))
      stop("Length of 'cluster' must equal the number of observations in qgcomp_fit$fit.")
    cluster_id = cluster
  }
  rob_cov  = sandwich::vcovCL(glm_fit, cluster = cluster_id, type = "HC1")[expos, expos]
  robust_var = sum(rob_cov)
  robust_se  = sqrt(robust_var)
  
  ## ---------- degrees of freedom ----------
  default_df = if (glm_fit$family$family == "gaussian") glm_fit$df.residual else NA
  robust_df  = length(unique(cluster_id)) - 1L
  
  ## ---------- test statistics ----------
  default_t  = psi_hat / default_se
  robust_t   = psi_hat / robust_se
  default_p  = if (!is.na(default_df)) 2*pt(-abs(default_t), df = default_df) else 2*pnorm(-abs(default_t))
  robust_p   = 2*pt(-abs(robust_t), df = robust_df)
  
  ## ---------- confidence intervals ----------
  alpha      = min(max(alpha, 0.001), 0.5)
  z_default  = if (!is.na(default_df)) qt(1 - alpha/2, df = default_df) else qnorm(1 - alpha/2)
  z_robust   = qt(1 - alpha/2, df = robust_df)
  default_ci = psi_hat + c(-1, 1) * z_default * default_se
  robust_ci  = psi_hat + c(-1, 1) * z_robust  * robust_se
  
  ## ---------- tidy output ----------
  out = data.frame(
    Method     = c("Default (model)", "Cluster-robust"),
    Estimate   = psi_hat,
    Std.Error  = c(default_se, robust_se),
    CI.lower   = c(default_ci[1], robust_ci[1]),
    CI.upper   = c(default_ci[2], robust_ci[2]),
    t.value    = c(default_t,  robust_t),
    df         = c(default_df, robust_df),
    p.value    = signif(c(default_p, robust_p), 3)
  )
  return(out)
}

## (a) Using region as the clustering unit
all_exposures_results = mixture_effect_se(
  qgcomp_fit = qgcomp_all_components,
  cluster    = cope_auo_july_no_miss$RegionName  # same vector used in vcovCL
)
print(all_exposures_results)

## Function Test: Mixture Effect 1-----------------

object <- copy(qgcomp_all_components)
data_eval <- copy(cope_auo_july_no_miss)
observation_wt_col= c("observation_wt")
exposure_names <- c(
  'C1M_School_closing_April_12_May_23',
  'C2M_Workplace_closing_April_12_May_23',
  'C3M_Cancel_public_events_April_12_May_23',
  'C4M_Restrictions_on_gatherings_April_12_May_23',
  'C5M_Close_public_transport_April_12_May_23',
  'C6M_Stay_at_home_requirements_April_12_May_23',
  'C7M_Restrictions_on_internal_movement_April_12_May_23')


## Extract weights
wts <- c(object$pos.weights, object$neg.weights)

## Multiply exposure training weights with exposure columns
data_eval$mixture_score <- rowSums(wts*data_eval[, names(wts)])

print("Summary of test mixture score column")
print(summary(data_eval$mixture_score))

# Evaluate effect
## pull formula from qgcomp
lhs <- as.character(object$fit$formula)[2]
rhs <- as.character(object$fit$formula)[3]

## Clean rhs string
rhs_clean <- gsub("[\n\r]", " ", rhs)       # Replace line breaks with space
rhs_clean <- gsub("\\s+", " ", rhs_clean) # Collapse multiple spaces
rhs_clean

## Remove exposure terms: to avoid multicollinearity
rhs_terms <- strsplit(rhs_clean, " \\+ ")[[1]]

rhs_new <- if (length(rhs_terms[!rhs_terms %in% exposure_names]) > 0) {
  paste0(" + ", paste(rhs_terms[!rhs_terms %in% exposure_names], collapse = " + "))
} else {
  ""
}

formula_str <- paste0(lhs, "~", "mixture_score" , "+" , rhs_new)
formula_str

## Model glm
# model_test <- lm(data = test_data,formula = formula_str, weights = (observation_wt_col))
model_mix <- eval(substitute(glm(formula = formula_str, family = "gaussian", data = data_eval, weights = W), list(W = as.name(observation_wt_col))))
model_mix

# Return
## return object
eval_object <- list()
eval_object$model <- copy(model_mix)
eval_object$new_data <- copy(data_eval)



## Function Test: Robust Errors -------------------------------------------

# Init
fit.object = copy(qgcomp_all_components$fit)
cluster_robust = TRUE
robust_type= "CR1"
cluster_var = cope_auo_july_no_miss$RegionName
exposure_names <- c(
  'C1M_School_closing_April_12_May_23',
  'C2M_Workplace_closing_April_12_May_23',
  'C3M_Cancel_public_events_April_12_May_23',
  'C4M_Restrictions_on_gatherings_April_12_May_23',
  'C5M_Close_public_transport_April_12_May_23',
  'C6M_Stay_at_home_requirements_April_12_May_23',
  'C7M_Restrictions_on_internal_movement_April_12_May_23')

## Check class
if (inherits(fit.object, "lm")) {
  message("This is a lm object.")
} else {
  stop("Pass a valid lm object.")
}

## Check if robust_type is a non-empty string of length 1
if (length(robust_type) == 0 || is.null(robust_type)) {
  stop("robust_type must be a non-empty vector")
}

## Init types
HC_types <- c("HC0", "HC1", "HC2", "HC3", "HC4", "HC4m", "HC5")
CR_types <- c("CR0", "CR1", "CR1S", "CR2", "CR3")

## Check if robust_type in HC types for cluster_robust = FALSE
if ((cluster_robust == FALSE) & (robust_type %in% CR_types)) {
  stop("Set cluster_robust to TRUE before proceeding with CR types")
}

## Check if robust_type is accurate
if (!(robust_type %in% c(HC_types, CR_types))) {
  stop("Invalid robust_type. Must be one of: ", paste(c(HC_types, CR_types), collapse = ", "))
}

## Check if cluster_var is non-empty
if ((cluster_robust == TRUE) & (length(cluster_var) == 0 || is.null(cluster_var))) {
  stop("cluster_var must be a non-empty vector when cluster_robust = TRUE")
}


### Robust Estimation --------
message("Using vcovHC + manual t-test(df = df.residual))")
hc_vcov <- sandwich::vcovHC(fit.object, type = robust_type)

## Clean: required due to high memory usage
gc()

## Extract robust beta coefficient, p and SE
new_coeff <- fit.object$coefficients[exposure_names]     # same as qgcomp object$psi
new_se <- sqrt(hc_vcov[exposure_names, exposure_names])  # square root of covariance
new_p_val <- 2 * pt(abs(new_coeff/new_se),         # get tstat by coeff/SE
                    df = fit.object$df.residual,         # degree of freedom is residual degree of freedom from model object
                    lower.tail = FALSE)
new_lower_ci <- new_coeff - qnorm(0.975) * new_se
new_upper_ci <- new_coeff + qnorm(0.975) * new_se

### Cluster Robust HC -----------

message("Using vcovCL + manual t-test (df = G - 1)")
cl_vcov <- sandwich::vcovCL(fit.object, cluster = cluster_var, type = robust_type)

## Clean: required due to high memory usage
gc()

## Extract robust beta coefficient, p and SE
new_coeff <- fit.object$coefficients[exposure_names]     # same as qgcomp object$psi
new_se <- sqrt(cl_vcov[exposure_names, exposure_names])  # square root of covariance
new_p_val <- 2 * pt(abs(new_coeff/new_se),         # get stat by coeff/SE
                    df = length(unique(cluster_var)) - 1, # degree of freedom is number of clusters - 1
                    lower.tail = FALSE)
new_lower_ci <- new_coeff - qnorm(0.975) * new_se
new_upper_ci <- new_coeff + qnorm(0.975) * new_se

### Cluster Robust CR ------------
message("Using vcovCR + manual t-test")
cl_vcov <- clubSandwich::vcovCR(fit.object, cluster = cluster_var, type = robust_type)

## Clean: required due to high memory usage
gc()

## Extract robust beta coefficient, p and SE
new_coeff <- sum(fit.object$coefficients[exposure_names])     # same as qgcomp object$psi
new_se <- sqrt(sum(cl_vcov[exposure_names, exposure_names, 
                           drop = FALSE]))                    # square root of covariance

## Satterthwaite calc
which_beta <- names(coef(fit.object)) %in% exposure_names
GH_obj <- clubSandwich:::get_GH(fit.object, cl_vcov)          # NOTE: using ::: to access internal function
P_array <- clubSandwich:::get_P_array(GH_obj)                 # NOTE: using ::: to access internal function

## Get projection
P_combined <- array(0, dim = c(dim(P_array)[1:2], 1))
exposure_indices <- which(names(coef(fit.object)) %in% exposure_names)
for (g in seq_len(dim(P_array)[1])) {
  for (h in seq_len(dim(P_array)[2])) {
    P_combined[g, h, 1] <- sum(P_array[g, h, exposure_indices])
  }
}

## Get Satterthwaite df for the combined effect
df_satt <- clubSandwich:::Satterthwaite_df(P_combined)  # NOTE: using ::: to access internal function

## Clean: required due to high memory usage
gc()

## Continue with p val calculations
new_p_val <- 2 * pt(abs(new_coeff/new_se),                    # get stat by coeff/SE
                    df = df_satt,                             # degree of freedom is Satterthwaite df
                    lower.tail = FALSE)
new_lower_ci <- new_coeff - qnorm(0.975) * new_se
new_upper_ci <- new_coeff + qnorm(0.975) * new_se

### Cluster Robust CR + Coef_test ------------------
message("Using vcovCR + coef_test")
cl_vcov <- clubSandwich::vcovCR(eval_object$model, cluster = cluster_var, type = robust_type)
robust_cr2 <- clubSandwich::coef_test(eval_object$model, 
                                      vcov = cl_vcov,
                                      test = "Satterthwaite", 
                                      coefs = c("mixture_score"))

### Clean: required due to high memory usage
gc()


### Extract robust beta coefficient, p and SE
new_coeff <- robust_cr2$beta
new_p_val <- robust_cr2$p_Satt
new_se <- robust_cr2$SE
new_lower_ci <- new_coeff - qnorm(0.975) * new_se
new_upper_ci <- new_coeff + qnorm(0.975) * new_se

#### check manual ---------
message("Using vcovCR + manual t-test")
cl_vcov <- clubSandwich::vcovCR(eval_object$model, cluster = cluster_var, type = robust_type)

## Clean: required due to high memory usage
gc()

## Extract robust beta coefficient, p and SE
new_coeff <- sum(eval_object$model$coefficients[c("mixture_score")])     # same as qgcomp object$psi
new_se <- sqrt(sum(cl_vcov[c("mixture_score"), c("mixture_score"), 
                           drop = FALSE]))                    # square root of covariance

## Satterthwaite calc
which_beta <- names(coef(eval_object$model)) %in% c("mixture_score")
GH_obj <- clubSandwich:::get_GH(eval_object$model, cl_vcov)          # NOTE: using ::: to access internal function
P_array <- clubSandwich:::get_P_array(GH_obj)                 # NOTE: using ::: to access internal function

## Get projection
P_combined <- array(0, dim = c(dim(P_array)[1:2], 1))
exposure_indices <- which(names(coef(eval_object$model)) %in% c("mixture_score"))
for (g in seq_len(dim(P_array)[1])) {
  for (h in seq_len(dim(P_array)[2])) {
    P_combined[g, h, 1] <- sum(P_array[g, h, exposure_indices])
  }
}

## Get Satterthwaite df for the combined effect
df_satt <- clubSandwich:::Satterthwaite_df(P_combined)  # NOTE: using ::: to access internal function

## Clean: required due to high memory usage
gc()

## Continue with p val calculations
new_p_val <- 2 * pt(abs(new_coeff/new_se),                    # get stat by coeff/SE
                    df = df_satt,                             # degree of freedom is Satterthwaite df
                    lower.tail = FALSE)
new_lower_ci <- new_coeff - qnorm(0.975) * new_se
new_upper_ci <- new_coeff + qnorm(0.975) * new_se

### Output df -------------
df <- data.frame(
  beta = new_coeff,
  p_val = new_p_val,
  std_error = new_se,
  lower_ci = new_lower_ci,
  upper_ci = new_upper_ci,
  row.names = NULL
)

df





## OLD Test Set Approximation Function Diagnostics ---------
# Mixture on Test Set
if(FALSE){
## Initialize params
object <- qgcomp_all_components
test_data <- test_data_df
observation_wt_col <- c("observation_wt")
cluster_col <- c("RegionName")

## Check class
if (inherits(object, "qgcompfit")) {
  message("This is a qgcomp object.")
} else {
  stop("Pass a valid qgcomp object.")
}

## Check if test_data is a data.frame
if (!is.data.frame(test_data)) {
  stop("test_data must be a data.frame.")
}

## Check if observation_wt_col is a non-empty string of length 1
if (!is.character(observation_wt_col) || length(observation_wt_col) != 1 || observation_wt_col == "") {
  stop("observation_wt_col must be a non-empty character vector of length 1.")
}

## Extract weights
training_model_wts <- c(object$pos.weights, object$neg.weights)

## Multiply exposure training weights with exposure columns
test_data$test_target_mix <- rowSums(training_model_wts*test_data[, names(training_model_wts)])

print("Summary of derived test mixture weight column")
summary(test_data$test_target_mix)

# Evaluate effect
## pull formula from qgcomp
lhs <- as.character(object$fit$formula)[2]
rhs <- as.character(object$fit$formula)[3]
formula_str <- paste0(lhs, "~", "test_target_mix" , "+" , rhs)
formula_str

## Model lm
# model_test <- lm(data = test_data,formula = formula_str, weights = (observation_wt_col))
model_test <- eval(substitute(lm(formula = formula_str, data = test_data, weights = W), list(W = as.name(observation_wt_col))))

# Return
## return object
eval_object <- list()
eval_object$model <- copy(model_test)
eval_object$data <- copy(test_data)


# Cluster Robust Errors
## Check counts
cluster_counts <- test_data %>%
  count(RegionName) %>%
  pull(n)

## Summary statistics
summary(cluster_counts)      
sd(cluster_counts)           
IQR(cluster_counts)         
max(cluster_counts) / min(cluster_counts)

cluster_robust_method = "vcovCL"

# Cluster Robust Estimation
if(cluster_robust_method == "vcovCR")
{
  ## Cluster Robust CR2
  ### Get estimates
  cluster_var <- train_data[[cluster_col]]
  robust_cr2 <- clubSandwich::coef_test(qgcomp_all_components$fit, 
                                        vcov = "CR2", 
                                        cluster = cluster_var,
                                        test = "Satterthwaite", 
                                        coefs = c(
                                          'C1M_School_closing_April_12_May_23',
                                          'C2M_Workplace_closing_April_12_May_23',
                                          'C3M_Cancel_public_events_April_12_May_23',
                                          'C4M_Restrictions_on_gatherings_April_12_May_23',
                                          'C5M_Close_public_transport_April_12_May_23',
                                          'C6M_Stay_at_home_requirements_April_12_May_23',
                                          'C7M_Restrictions_on_internal_movement_April_12_May_23'))
  
  ### Extract robust beta coefficient, p and SE for test_target_mix
  mix_coeff <- robust_cr2$beta
  mix_p_val <- robust_cr2$p_Satt
  mix_se <- robust_cr2$SE
  mix_lower_ci <- mix_coeff - qnorm(0.975) * mix_se
  mix_upper_ci <- mix_coeff + qnorm(0.975) * mix_se
  
} else if (cluster_robust_method == "vcovCL"){
  
  ## Cluster Robust CL
  ### Get estimates
  cluster_var <- test_data[[cluster_col]]
  cl_vcov <- sandwich::vcovCL(model_test, cluster = ~cluster_var, type = "HC1")
  robust_cl <- lmtest::coeftest(model_test, vcov. = cl_vcov)
  
  ### Extract robust beta coefficient, p and SE for test_target_mix
  idx <- which(rownames(robust_cl) == "test_target_mix")
  mix_coeff <- robust_cl["test_target_mix", "Estimate"]
  mix_p_val <- robust_cl["test_target_mix", "Pr(>|t|)"]
  mix_se <- robust_cl["test_target_mix", "Std. Error"]
  mix_lower_ci <- mix_coeff - qnorm(0.975) * mix_se
  mix_upper_ci <- mix_coeff + qnorm(0.975) * mix_se
  
} else {
  stop("Select suitable method from vcovCR or vcovCL")
}

# Output df
df <- data.frame(
  beta = mix_coeff,
  p_val = mix_p_val,
  std_error = mix_se,
  lower_ci = mix_lower_ci,
  upper_ci = mix_upper_ci
)

# Function Test
source(here::here("2d_qgcomp_custom.R"))
df_qgcomp_test <-  qgcomp.test.eval(object = qgcomp_all_components,
                                    test_data = test_data_df,
                                    observation_wt_col = "observation_wt",
                                    cluster_col = "RegionName",
                                    cluster_robust_method = "vcovCR")

df_qgcomp_test

}

