### Who reported shift in gender neutral names

### I'm going to try to test this hypothesis with data. I'll be analyzing the change in the trend of gender-neutral names in America in the past 140 years, from 1882 to 2022, using datasets from the Social Security Administration, an independent agency of the U.S. federal government which reports the names given to babies at birth for 2 genders–male and female. 

# **What defines gender-neutral names?**

### I will be analysing the overlaps in the names given to these 2 genders to determine the change in trend of gender-neutral names. 

### We could analyse the change in the total number of overlaps in the names given to male and female babies, but that wouldn’t highlight the trend very well. Simply because, population growth (cite in write up) would mean that there are more babies born each year, and hence a higher occurance of a baby with a gender-neutral name.  

### there might be outliers, or parents giving their children a historically/supposedly opposite gender name for the reasons of establishing uniqueness or honouring a family name etc. 
### Not comparing proportion will falsely make it seem as if gender-neutral names haven’t risen by much because gender-neutral names only cover a small proportion of all given names. (Gender-specific names are still highly popular-graph of top 10 male/female names, yep, no match here). Hence, I would only consider the proportion of those names which were given to at least 500 babies in both genders in relation to the total number of babies borned in that particular year. 

# **The Repetition of Pop Music**
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
  gender_neutral_names <- merge_gender_names %>% 
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


Distribution of gender-neutral names from 1882 to 2022, excluding 2 years with no data
Graph

As you can tell, gender-neutral names have risen by a huge proportion over the years. The percentage is a whopping % from 1882 to 2022! The peak of gender-neutral names appears to be which year, which however, seems to have fallen slightly over the last few decades. Nonetheless, it cannot be denied that this fall is marginal in comparison to the large rise in gender-neutral names over the years.
Top Gender-Neutral Names of 2022 (in us)?
  table
02_text

To zoom into the top 5 names, how have their trends changed over the years? Were they originally skewed to a particular gender or were they already gender-neutral from the beginning? Also, when did these names come about or start becoming popular/establishing itself for both genders?
  Moving graph (see if can plot in 10 year intervals and then zoom in to see clearer) and scrolly table to explain and determine meaning
04_mpg if cannot move?
What’s the cause of this shifting trend?
  
  To determine this, maybe we wanna relook at the above graph. 3 of the top 5 gender-neutral names have originated from a historically male name. 

Let’s test this trend by analysing the modes for the first and last characters of all supposedly female, male and gender-neutral given names 

Table

According to a source by…girls stronger–match with meaning of alexis?
  
  The reasons stated…the gender neutral and names all

