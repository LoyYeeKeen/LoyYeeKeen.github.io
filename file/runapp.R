library(shiny)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(DT)

read_year_data <- function(year) {
  data <- read_csv(paste0("yob", year, ".txt"), col_names = c("Name", "Gender", "Count"))
}

# Initialize a list to store data frames for each year
data_list <- list()

# Loop through the years from 1882 to 2022
for (year in 1882:2022) {
  data <- read_year_data(year)
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


popular_gn <- gender_neutral_names_list[["2022"]] %>%
  mutate("Count of female babies" = as.integer(Count.x),
         "Count of male babies" = as.integer(Count.y), 
         sum = as.integer(Count.x) + as.integer(Count.y),
         difference = abs(`Count of female babies`-`Count of male babies`),
         "Proportion Overlap" = (sum-difference)/difference) %>%
  select(Name, "Count of female babies","Count of male babies","Proportion Overlap") %>%
  arrange(desc(`Proportion Overlap`))

# dont know need use backticks? you jiao ma?

popular_gender_neutral_names <- popular_gn %>% slice(1:5)

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

# Define your variables (A, B, C, D, E) with the text you want to display
A <- "This is the text for Finley."
B <- "This is the text for Blake."
C <- "This is the text for Tatum."
D <- "This is the text for Charlie."
E <- "This is the text for Armani"

# Define the calculateYAxisLimits function
calculateYAxisLimits <- function(name) {
  filtered_df <- combined_name_trend_df %>%
    filter(Name == name, Year >= 1882, Year <= 2022)
  min_value <- min(filtered_df$Proportion)
  max_value <- max(filtered_df$Proportion)
  return(c(min_value, max_value))
}

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

# Initialize a list to store gender-neutral names for each year
unique_list <- list()

# Process data for each year
for (year in 1882:2022) {
  data <- data_list[[as.character(year)]]
  unique_names <- data %>% filter(Count<50)  # Call the function with the data
  row_df<-nrow(unique_names)
  unique_list[[as.character(year)]] <- row_df
}

# Create the proportion_df dataframe
unique_df <- data.frame(
  year = 1882:2022,
  number_of_names = unlist(unique_list)
)
popular_gn_only<-popular_gn %>% select(Name)
# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  #Not comparing proportion will falsely make it seem as if gender-neutral names haven’t risen by much because gender-neutral names only cover a small proportion of all given names. (Gender-specific names are still highly popular-graph of top 10 male/female names, yep, no match here). 
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Slider for the number of years ----
      sliderInput(inputId = "bars",
                  label = "Number of years:",
                  min = 1,
                  max = 141,
                  value = 141),
      # Input: Selector for variable to plot against mpg ----
      selectInput("Name", "Name:",
                  popular_gender_neutral_names$Name),
      div(
        id = "card-container",
        style = "background-color: #f7f7f7; border: 1px solid #ccc; padding: 10px; margin-top: 10px;",
        actionButton("prevButton", "Previous"),
        actionButton("nextButton", "Next")
      ), 
      div(
        id = "card-container",
        style = "background-color: #f7f7f7; border: 1px solid #ccc; padding: 10px; margin-top: 10px; width: 100px; height: 80px;",
        textOutput("textOutput"),
        style = "padding: 20px;"
      ),
      # Use the Name column as options
      # Input: Numeric entry for number of obs to view ----
      numericInput(inputId = "obs",
                   label = "Top _ Gender-Neutral Names of 2022:",
                   value = 5,
                   min = 1,  # Minimum value
                   max = 36 # Maximum value (you can adjust this value as needed)
    ),
    
    sliderInput(inputId = "bars2",
                label = "Number of years:",
                min = 1,
                max = 141,
                value = 141),
    
    actionButton("highlightButton", label = "Click to highlight the names ending with 'n'!")
    ),
    
      # Main panel for displaying outputs ----
    mainPanel(
      # Output: Histogram ----
      plotOutput(outputId = "distPlot"),
        plotOutput(outputId = "distPlot2"),
        div(id = "datatable-container", style = "position: relative;", DTOutput("datatable")
        )
        ),
      plotOutput(outputId="distPlot3"),
      # Output: HTML table with requested number of observations ----
      tableOutput("view"),
      tableOutput("characterTable"),
      plotOutput(outputId = "distPlot4"),
      DTOutput("table")
      )
  )

# Define server logic required to draw a bar plot ----
server <- function(input, output, session) {
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

#difficulties=dk that need to filter

  # Initialize the selected name and interval
  selected_name <- reactiveVal()
  selected_name(NULL)
  interval_start <- reactiveVal(1882)
  interval_end <- reactiveVal(1932)
  
  # Keep track of the cumulative interval
  cumulative_start <- reactiveVal(1882)
  cumulative_end <- reactiveVal(1932)
  
  # Observe select input changes and update the card and plot
  observe({
    if (is.null(selected_name()) || selected_name() != input$Name) {
      # Reset the interval to 1882 to 1932
      interval_start(1882)
      interval_end(1932)
      cumulative_start(1882)
      cumulative_end(1932)
    }
    selected_name(input$Name)
    updatePlot(selected_name(), interval_start(), interval_end())
  })
  
  # Function to update the plot and explanation card
  updatePlot <- function(selected_name, interval_start, interval_end) {
    y_limits <- calculateYAxisLimits(selected_name)  # Calculate y-axis limits based on selected name
    filtered_df <- combined_name_trend_df %>%
      filter(Name == selected_name, Year >= interval_start, Year <= interval_end)
    
    filtered_df <- filtered_df %>%
      bind_rows(
        data.frame(Name = selected_name, Year = 1882, Gender = "M", Proportion = 0),
        data.frame(Name = selected_name, Year = 1882, Gender = "F", Proportion = 0)
      )
    
    gg <- ggplot(data = filtered_df) +
      aes(x = Year, y = Proportion, group = Gender, color = Gender) +
      geom_line(linetype = "solid") +
      labs(x = "Year", y = "Proportion of Name", title = "Trend of Gender-Neutral Names") +
      scale_x_continuous(breaks = seq(1882, 2022, by = 25), limits = c(1882, 2022), expand = c(0, 0)) +
      scale_y_continuous(limits = y_limits) +
      scale_color_manual(values = c("M" = "blue", "F" = "red"))
    
    # Update the plot and card
    output$distPlot2 <- renderPlot({ print(gg) })
  }
  
  # Handle next and previous interval button clicks
  observeEvent(input$nextButton, {
    new_cumulative_end <- cumulative_end() + 50
    
    # Check if the new cumulative end exceeds the maximum year (2022)
    if (new_cumulative_end > 2022) {
      new_cumulative_end <- 2022
    }
    
    cumulative_end(new_cumulative_end)
    interval_start(cumulative_start())
    interval_end(cumulative_end())
    updatePlot(selected_name(), interval_start(), interval_end())
  })
  
  observeEvent(input$prevButton, {
    new_cumulative_end <- cumulative_end() - 50
    
    # Check if the new cumulative end goes below the initial interval end
    if (new_cumulative_end < 1932) {
      new_cumulative_end <- 1932
    }
    
    cumulative_end(new_cumulative_end)
    interval_start(cumulative_start())
    interval_end(cumulative_end())
    updatePlot(selected_name(), interval_start(), interval_end())
  })
  output$textOutput <- renderText({
    if (!is.null(selected_name())) {
      if (selected_name() == "Finley") {
        A
      } else if (selected_name() == "Blake") {
        B
      } else if (selected_name() == "Tatum") {
        C
      } else if (selected_name() == "Charlie") {
        D
      } else if (selected_name() == "Armani") {
        E
      }
    }
  })
  
  observeEvent(input$Name, {
    selected_name(input$Name)
  })
  
  observeEvent(input$nextButton, {
    if (!is.null(selected_name())) {
      if (selected_name() == "Finley") {
        A
      } else if (selected_name() == "Blake") {
        B
      } else if (selected_name() == "Tatum") {
        C
      } else if (selected_name() == "Charlie") {
        D
      } else if (selected_name() == "Armani") {
        E
      }
    }
  })
  
  observeEvent(input$prevButton, {
    if (!is.null(selected_name())) {
      if (selected_name() == "Finley") {
        A
      } else if (selected_name() == "Blake") {
        B
      } else if (selected_name() == "Tatum") {
        C
      } else if (selected_name() == "Charlie") {
        D
      } else if (selected_name() == "Armani") {
        E
      }
    }
  })
# Create a plot to visualize the trend using facet_wrap with free y-scales
  output$distPlot3 <- renderPlot({
  ggplot(combined_name_trend_df) +
  aes(x = Year, y = Proportion, group = Gender, color = Gender) +
  geom_line() +
  labs(x = "Year", y = "Proportion of Name", title="Trend of Gender-Neutral Names") +
  facet_wrap(~Name, scales = "free_y") +
  scale_x_continuous(breaks = seq(1882, 2022, by = 25))
})
  
  
  # Show the first "n" observations ----
  output$view <- renderTable({
    head(popular_gn, input$obs)
  })
  
  output$characterTable <- renderTable({
    character_trend
  })
  
  output$distPlot4 <- renderPlot({  # Fixed output ID
    # Get the selected number of years from the slider input
    num_years <- input$bars2
    
    # Filter the data to include only the selected number of years
    filtered_data <- unique_df %>%
      filter(year >= (2022 - num_years + 1))
    
    # Create the bar plot
    ggplot(filtered_data) +
      aes(x = year, y = number_of_names) +
      geom_bar(stat = "identity") +
      labs(
        x = "Year",
        y = "Number of Unique Names",
        title = "Number of Unique Names over the Years"
      ) +
      scale_x_continuous(
        limits = c(2022 - num_years + 1, 2022),
        breaks = seq(2022 - num_years + 1, 2022, by = 10)
      )
  })
  
  output$table <- renderDT({
    datatable(popular_gn_only)
  })
  
  observeEvent(input$highlightButton, {
    # Highlight names ending with 'n'
    popular_gn_only$Name <- ifelse(substr(popular_gn_only$Name, nchar(popular_gn_only$Name), nchar(popular_gn_only$Name)) == "n",
                        paste0("<span style='background-color: yellow;'>", popular_gn_only$Name, "</span>"),
                        popular_gn_only$Name)
    
    output$table <- renderDT({
      datatable(popular_gn_only, escape = FALSE)
    })
  })
}


# Create Shiny app ----
shinyApp(ui = ui, server = server)