library('tidyverse')

# Function to read and convert data for a given year
convert_to_csv <- function(year) {
  readme_file <- paste0("yob", year, ".txt")
  readme_text <- readLines(readme_file, warn = FALSE)
  data <- read.csv(text = paste(readme_text, collapse = "\n"), header = FALSE, col.names = c("Name", "Gender", "Count"))
  return(data)
}

# Initialize a list to store data frames for each year
data_list <- list()

# Loop through the years from 1882 to 2022
for (year in 1882:2022) {
  data <- convert_to_csv(year)
  data_list[[as.character(year)]] <- data
}

# Function to filter female names
filter_female_names <- function(data) {
  return(
    data %>%
      filter(Gender == "F")
  )
}

# Function to filter male names
filter_male_names <- function(data) {
  return(
    data %>%
      filter(Gender == "M")
  )
}

# Function to merge female and male names and filter gender-neutral names
merge_names <- function(data) {
  female_names <- filter_female_names(data)
  male_names <- filter_male_names(data)
  
  overlap_names <- merge(
    female_names,
    male_names,
    by = "Name"
  )
  gender_neutral_names <- overlap_names %>%
    filter(Count.x > 500 & Count.y > 500)
  
  return(gender_neutral_names)
}

# Initialize a list to store gender-neutral names for each year
gender_neutral_names_list <- list()

# Process data for each year
for (year in 1882:2022) {
  data <- data_list[[as.character(year)]]
  gender_neutral_names <- merge_names(data)  # Call the function with the data
  gender_neutral_names_list[[as.character(year)]] <- gender_neutral_names
}

# Initialize a list to store proportions for each year
proportion_list <- list()

# Calculate proportions for each year
for (year in 1882:2022) {
  gender_neutral_data <- gender_neutral_names_list[[as.character(year)]]
  all_names_data <- data_list[[as.character(year)]]
  
  total_count_gender_neutral <- sum(gender_neutral_data$Count.x) + sum(gender_neutral_data$Count.y)
  total_count_all_names <- sum(all_names_data$Count)
  proportion <- total_count_gender_neutral / total_count_all_names
  
  proportion_list[[as.character(year)]] <- proportion
}

# Create the proportion_df dataframe
proportion_df <- data.frame(
  year = 1882:2022,
  proportion = unlist(proportion_list)
)

# Create the proportion_df dataframe
proportion_df <- data.frame(
  year = 1882:2022,
  proportion = unlist(proportion_list)
)

library(ggplot2)

plot <- ggplot(
  proportion_df, 
               aes(
                 x = year, 
                   y = proportion)
  ) +
  geom_bar(
    stat = "identity") +
  labs(
    x = "Year", 
       y = "Proportion of Gender-Neutral Names", 
       title = "Proportion of Gender-Neutral Names Over the Years")+ 
  scale_x_continuous(
    limits = c(1882, 2022), 
    breaks = seq(1882, 2022, 
                 by = 20)
    )

# Print the plot
print(plot)


popular_gender_neutral_names<-gender_neutral_names_list[["2022"]] %>% mutate(Difference=abs(Count.x-Count.y)) %>% select(Name,Difference) %>% arrange(Difference) %>% slice(1:5)

library(dplyr)
library(ggplot2)

# Initialize an empty list to store data frames
name_trend_list <- list()

# Loop through the popular gender-neutral names
for (i in 1:5) {
  popular_name <- popular_gender_neutral_names$Name[i]
  name_trend <- data.frame()  # Initialize an empty data frame outside the inner loop
  
  for (year in 1882:2022) {
    all_names_data <- data_list[[as.character(year)]]
    
    # Filter the data for the specific popular name
    name_data <- all_names_data %>% filter(Name == popular_name)
    
    # Calculate the proportion for the specific name
    proportion <- name_data$Count / sum(all_names_data$Count)
    
    name_data <- mutate(name_data, Proportion = proportion, Year = year)
    name_trend <- bind_rows(name_trend, name_data)
  }
  
  # Append this name trend to the list
  name_trend_list[[i]] <- name_trend
}

# Combine all the data frames
combined_name_trend_df <- do.call(rbind, name_trend_list)

# Create a plot to visualize the trend using facet_wrap with free y-scales
gg <- ggplot(combined_name_trend_df, aes(x = Year, y = Proportion, group = Gender, color = Gender)) +
  geom_line() +
  labs(x = "Year", y = "Proportion of Name") +
  ggtitle("Trend of Gender-Neutral Names") +
  facet_wrap(~Name, scales = "free_y") +
  scale_x_continuous(breaks = seq(1882, 2022, by = 25))

# Print the plot
print(gg)



# Function to calculate the mode of first and last characters
calculate_mode <- function(names) {
  first_characters <- substr(names, 1, 1)
  last_characters <- substr(names, nchar(names), nchar(names))
  
  table_first_characters <- table(first_characters)
  first_character_mode <- names(table_first_characters[which.max(table_first_characters)])
  
  table_last_characters <- table(last_characters)
  last_character_mode <- names(table_last_characters[which.max(table_last_characters)])
  
  return(list(first_character_mode = first_character_mode, last_character_mode = last_character_mode))
}

# Combine all female names from all years
all_female_names <- unlist(lapply(data_list, function(data) {
  filter_female_names(data)$Name
}))

# Calculate the mode of first and last characters for female names
female_mode <- calculate_mode(all_female_names)

# Combine all female names from all years
all_male_names <- unlist(lapply(data_list, function(data) {
  filter_male_names(data)$Name
}))

# Calculate the mode of first and last characters for female names
male_mode <- calculate_mode(all_male_names)


# Combine all gender-neutral names from all years
gender_neutral_data <- gender_neutral_names_list[[as.character(year)]]

all_gn_names <- unlist(lapply(data_list, function(data) {
  gender_neutral_data$Name
}))

# Calculate the mode of first and last characters for gender-neutral names
gn_mode <- calculate_mode(all_gn_names)

# Now, you can access the first and last character modes as follows:
female_first_mode <- female_mode$first_character_mode
female_last_mode <- female_mode$last_character_mode

male_first_mode <- male_mode$first_character_mode
male_last_mode <- male_mode$last_character_mode


gn_first_mode <- gn_mode$first_character_mode
gn_last_mode <- gn_mode$last_character_mode


character_trend <- tibble(
  Gender = rep(c("Female", "Male", "Gender-Neutral"), each = 2),
  Character = c("First", "Last", "First", "Last", "First", "Last"),
  Mode = c(female_first_mode, female_last_mode, male_first_mode, male_last_mode, gn_first_mode, gn_last_mode)
)

print(character_trend)