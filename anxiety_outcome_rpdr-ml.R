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

packages = c("ggplot2", "dplyr", "treemap", "usmap", "here")

# Call the package_install function 
package_install(packages)

# Load the data using a relative path with the `here` package
load(here("filtered-diag-demo-data.RData"))

# Convert state column to factor
df_noexp_1mo$state = as.factor(df_noexp_1mo$state)
df_noexp_7mo$state = as.factor(df_noexp_7mo$state)

# Filtering out rows where 'state' is empty or consists only of spaces
df_noexp_1mo = df_noexp_1mo %>% filter(state != " " & state != "")

df_noexp_7mo = df_noexp_7mo %>% filter(state != " " & state != "")

# Function to create the bar plot for unique encounters or patients
create_bar_plot = function(df, title, top = TRUE, subject = FALSE) {
  if (subject) {
    # Count the number of unique patients for each state
    state_counts = df %>%
      distinct(subject_num, state) %>%
      group_by(state) %>%
      summarise(count = n())
  } else {
    # Count the number of rows for each state
    state_counts = df %>%
      group_by(state) %>%
      summarise(count = n())
  }
  
  # Order the states by count in descending order
  state_counts = state_counts %>%
    arrange(desc(count))
  
  # Get the top 10 or bottom 10 states
  if (top) {
    plot_states = head(state_counts, 10)
  } else {
    plot_states = tail(state_counts, 10)
  }
  
  # Creating the plot 
  plot = ggplot(plot_states, aes(x = reorder(state, -count), y = count, fill = count)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = count), vjust = -0.5, size = 3) +
    scale_fill_gradient(low = "lightblue", high = "darkblue") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text.y = element_blank(),  # Hide y-axis text
      axis.ticks.y = element_blank(),  # Hide y-axis ticks
      legend.position = "none"
    ) +
    labs(
      title = title,
      x = "State",
      y = "Count"
    )
  
  return(plot)
}

# Function to create the treemap plot
create_treemap_plot = function(df, title) {
  
  # Count the number of rows for each state
  state_counts = df %>%
    group_by(state) %>%
    summarise(count = n())
  
  # Create the treemap plot
  plot = treemap(state_counts,
                  index = "state",
                  vSize = "count",
                  title = title,
                  palette = "Blues")
  
  return(plot)
}

# Function to create the choropleth map
create_choropleth_map = function(df, title) {
  # Count the number of unique patients for each state
  state_counts = df %>%
    distinct(subject_num, state) %>%
    group_by(state) %>%
    summarise(count = n())
  
  # Convert state factor levels to state abbreviations
  state_counts$state = as.character(state_counts$state)
  
  # Join the state counts with the usmap data
  plot_data = left_join(us_map("states"), state_counts, by = c("abbr" = "state"))
  
  # Filter out Alaska for better visualization
  plot_data = plot_data %>% filter(abbr != "AK")
  
  # Create the choropleth map using plot_usmap()
  plot =plot_usmap(data = plot_data, values = "count") +
    scale_fill_continuous(
      low = "white", high = "red", name = "Unique Patients",
      label = scales::comma, trans = "log10",
      #breaks = c(1, 10, 100, 1000, 10000, max(plot_data$count, na.rm = TRUE)),
      breaks = c(1, 10, 100, 1000, 10000),
      limits = c(1, max(plot_data$count, na.rm = TRUE))
    ) +
    theme(
      legend.position = "right",
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
    ) +
    labs(title = title)
  
  return(plot)
}

# Filter out rows where 'state' is empty or consists only of spaces
df_noexp_1mo = df_noexp_1mo %>% filter(state != " " & state != "")
df_noexp_7mo = df_noexp_7mo %>% filter(state != " " & state != "")

# Create the choropleth map for df_noexp_1mo
plot_1mo_map = create_choropleth_map(df_noexp_1mo, "Geographic Distribution of Unique Patients (1 Month)")
print(plot_1mo_map)

# Create the choropleth map for df_noexp_7mo
plot_7mo_map = create_choropleth_map(df_noexp_7mo, "Geographic Distribution of Unique Patients (7 Months)")
print(plot_7mo_map)

# Create the plots for df_noexp_1mo
plot_1mo_top_encounters    = create_bar_plot(df_noexp_1mo, "Top 10 States - Unique Encounters (1 Month)")
plot_1mo_bottom_encounters = create_bar_plot(df_noexp_1mo, "Bottom 10 States - Unique Encounters (1 Month)", top = FALSE)
plot_1mo_top_patients      = create_bar_plot(df_noexp_1mo, "Top 10 States - Unique Patients (1 Month)", subject = TRUE)
plot_1mo_bottom_patients   = create_bar_plot(df_noexp_1mo, "Bottom 10 States - Unique Patients (1 Month)", top = FALSE, subject = TRUE)
plot_1mo_treemap           = create_treemap_plot(df_noexp_1mo, "State Distribution (1 Month)")

# Create the plots for df_noexp_7mo
plot_7mo_top_encounters    = create_bar_plot(df_noexp_7mo, "Top 10 States - Unique Encounters (7 Months)")
plot_7mo_bottom_encounters = create_bar_plot(df_noexp_7mo, "Bottom 10 States - Unique Encounters (7 Months)", top = FALSE)
plot_7mo_top_patients      = create_bar_plot(df_noexp_7mo, "Top 10 States - Unique Patients (7 Months)", subject = TRUE)
plot_7mo_bottom_patients   = create_bar_plot(df_noexp_7mo, "Bottom 10 States - Unique Patients (7 Months)", top = FALSE, subject = TRUE)
plot_7mo_treemap           = create_treemap_plot(df_noexp_7mo, "State Distribution (7 Months)")

# Display the plots
print(plot_1mo_top_encounters)
print(plot_1mo_bottom_encounters)
print(plot_1mo_top_patients)
print(plot_1mo_bottom_patients)
print(plot_1mo_treemap)

print(plot_7mo_top_encounters)
print(plot_7mo_bottom_encounters)
print(plot_7mo_top_patients)
print(plot_7mo_bottom_patients)
print(plot_7mo_treemap)
