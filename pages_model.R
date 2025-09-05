#### Page's Model ####

install.packages("nlme")
library(nlme)

##heres the potential code with the error bars

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(purrr)


#extract k from each drying curve (each lichen), group by site --> anova

# Read in the data
data_dry <- read.csv("RWC_counts.csv")

# Check and correct column names if necessary
names(data_dry) <- gsub("\\s+", "", names(data_dry))



# Transform the data to long format
long_data <- data_dry %>%
    pivot_longer(
        cols = starts_with("x"),  # Selects all columns that start with 'x' (representing time points)
        names_to = "Time",  # New column for time
        values_to = "Weight",  # New column for weight
        names_pattern = "x(\\d+)min",  # Extracting the time number part from names like 'x10min'
        names_transform = list(Time = as.numeric)  # Ensure Time is numeric
    ) %>%
    mutate(Time = as.numeric(gsub("[^0-9]", "", Time))) %>%  # Remove non-numeric characters and convert to numeric
    filter(!is.na(Time))

# Define Page's model function for moisture content in decimal form
page_model <- function(time, k, n) {
    M_0 <- 1   # Initial moisture content (100% in decimal form)
    M_inf <- 0   # Final moisture content (0% in decimal form)
    M_inf + (M_0 - M_inf) * exp(-(k * time + 1e-6)^n)  # Small constant to stabilize the calculation
}

# Function to fit Page's model to individual data and extract k
fit_extract_k <- function(data) {
    model <- try(nls(Weight ~ page_model(Time, k, n),
                     data = data,
                     start = list(k = 0.01, n = 1),
                     control = nls.control(maxiter = 500)),
                 silent = TRUE)  # In case fitting fails
    
    # Extract the 'k' coefficient if the model converges successfully
    if (inherits(model, "nls")) {
        return(coef(model)["k"])
    } else {
        return(NA)  # Return NA if fitting fails
    }
}


# Nest data by individual (assuming each individual has a unique identifier column, e.g., 'ID')
# If there's no individual column, you can skip this step and directly use the long_data.
data_individual <- long_data %>%
    group_by(ID) %>%  # Replace 'Individual_ID' with the actual column name for individuals
    nest()

#----possible fix
# Define a simple linear model as a backup when Page's model fails
simple_linear_model <- function(time, k) {
    M_0 <- 1   # Initial moisture content (100% in decimal form)
    M_inf <- 0   # Final moisture content (0% in decimal form)
    M_inf + (M_0 - M_inf) * exp(-k * time)
}

# Modified function to fit either Page's model or a simple linear model as a fallback
fit_extract_k <- function(data) {
    # Try fitting Page's model
    page_model_fit <- try(nls(Weight ~ page_model(Time, k, n),
                              data = data,
                              start = list(k = 0.01, n = 1),
                              control = nls.control(maxiter = 500)),
                          silent = TRUE)
    
    # If Page's model fits, return k
    if (inherits(page_model_fit, "nls")) {
        return(coef(page_model_fit)["k"])
    }
    
    # If Page's model fails, try fitting the simple linear model
    linear_model_fit <- try(nls(Weight ~ simple_linear_model(Time, k),
                                data = data,
                                start = list(k = 0.01),
                                control = nls.control(maxiter = 500)),
                            silent = TRUE)
    
    # Return k from the linear model or NA if both fail
    if (inherits(linear_model_fit, "nls")) {
        return(coef(linear_model_fit)["k"])
    } else {
        return(NA)
    }
}

# Re-run the model fitting and k extraction process
k_values <- data_individual %>%
    mutate(k = map_dbl(data, fit_extract_k))

k_df <- k_values %>%
    select(ID, k)

# Merge the plot information from the original dataset (assuming 'Plot' is the plot column)
k_df <- k_df %>%
    left_join(data_dry %>% select(ID, Plot), by = "ID") %>%
    mutate(Plot = recode(Plot,
                         "Altos" = "Low",
                         "Estacion Low" = "Middle",
                         "Estacion High" = "High"))

print(k_df)

# Now group the k values by plot and run an ANOVA
anova_result <- aov(k ~ Plot, data = k_df)

# Print the ANOVA result
summary(anova_result)



# Perform Tukey's Honest Significant Differences (HSD) post-hoc test
tukey_result <- TukeyHSD(anova_result)

# Print the Tukey HSD result
print(tukey_result)


#i observed that the NAs were drying fast and didnt fit model
#moreso fit a linear drying model so I extracted the linear k for those 7 individuals 





#--------
# Fit Page's model to each individual and extract k values
k_values <- data_individual %>%
    mutate(k = map_dbl(data, fit_extract_k))  # Apply the function to each individual

# Create a new dataframe with Individual_ID and k values
k_df_na <- k_values %>%
    select(ID, k)

# Print the dataframe with extracted k values
print(k_df_na)


# Identify individuals with NA in k values
problematic_ids <- data_individual %>%
    mutate(k = map_dbl(data, fit_extract_k)) %>%
    filter(is.na(k)) %>%
    pull(ID)

# Inspect data for these individuals
problematic_data <- long_data %>%
    filter(ID %in% problematic_ids)

print(problematic_data)


k_summary <- k_df_na %>%
    summarise(
        mean_k = mean(k, na.rm = TRUE),
        min_k = min(k, na.rm = TRUE),
        max_k = max(k, na.rm = TRUE)
    )

print(k_summary)

mean(k_df_na$k)
min(k_df_na$k)
max(k_df_na$k)


ggplot(long_data, aes(x = Time, y = Weight, color = Plot, group = Plot)) +
    geom_point(position = position_jitter(width = 0.3, height = 0), size = 2) +  # Add jitter to points
    geom_errorbar(aes(ymin = Weight - Weight_sd, ymax = Weight + Weight_sd), width = 0.2) +  # Add error bars
    geom_line(data = fitted_data, aes(x = Time, y = .fitted), linetype = "dashed") +  # Fitted Page's model curve
    labs(title = "Water Loss Rate and Site",
         x = "Time (minutes)",
         y = "Moisture Content (%)") +
    scale_x_continuous(breaks = seq(min(long_data$Time), max(long_data$Time), by = 10)) +  # Adjust x-axis breaks if necessary
    theme_minimal() +
    theme(legend.title = element_blank())  # Optionally remove the legend title
ggsave("figure_2.jpeg", width = 8, height = 5, units = "in", dpi = 600)



