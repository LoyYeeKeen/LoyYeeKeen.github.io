library(shiny)
library(ggplot2)
library(tidyverse)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Slider for the number of years ----
      sliderInput(inputId = "bars",
                  label = "Number of years:",
                  min = 1,
                  max = 141,
                  value = 141)
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
    )
  )
)

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

# Define server logic required to draw a bar plot ----
server <- function(input, output) {
  output$distPlot <- renderPlot({  # Fixed output ID
    # Get the selected number of years from the slider input
    num_years <- input$bars
    
    # Filter the data to include only the selected number of years
    filtered_data <- proportion_df %>%
      filter(year >= (2022 - num_years + 1))
    
    # Create the bar plot
    ggplot(filtered_data) +
      aes(x = year, y = proportion) +
      geom_bar(stat = "identity") +
      labs(
        x = "Year",
        y = "Proportion of Gender-Neutral Names",
        title = "Proportion of Gender-Neutral Names Over the Selected Years"
      ) +
      scale_x_continuous(
        limits = c(2022 - num_years + 1, 2022),
        breaks = seq(2022 - num_years + 1, 2022, by = 10)
      )
  })
}
#difficulties=dk that need to filter

# Create Shiny app ----
shinyApp(ui = ui, server = server)