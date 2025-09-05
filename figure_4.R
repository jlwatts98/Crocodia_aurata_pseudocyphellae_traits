#Lichen pseudocyphellae study

library(ggplot2)
library(dplyr)
#first do bar plots with pseudo
#read in data
lichen_data <- read.csv("updated_lichen_data.csv")

library(dplyr)
thickness <- lichen_data$thallus.thickness..mm.

summary_stats3 <- lichen_data %>%
    group_by(Plot) %>%
    summarize(mean_value = mean(thickness),
              sd_value = sd(thickness),
              n = n())
summary(summary_stats3)

# Create the bar plot with mean and distribution data
ggplot(lichen_data, aes(x = Plot, y = thickness, fill = Plot)) + 
    geom_boxplot() +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white", color = "black") +
    labs(title = "Thallus Thickness and Elevation",
         x = "Location",
         y = "Thallus thickness (mm)") +
    theme_minimal() +
    scale_fill_brewer(palette = "Set2") + # Adjusts the fill colors for distinction
    geom_text(data = summary_stats3, aes(label = paste("Mean:", round(mean_value, 6), "\nSD:", round(sd_value, 6), "\nN:", n), x = Plot, y = mean_value), vjust = -3, size = 3.5, color = "black")
ggplot(lichen_data, aes(x = Plot, y = thickness, fill = Plot)) + 
    geom_boxplot() +
    labs(title = "Thallus Thickness and Elevation",
         x = "Location",
         y = "Thallus Thickness (mm)") +
    theme_minimal() +
    scale_fill_brewer(palette = "Set2") # Adjusts the fill colors for distinction
ggsave("figure_4.jpeg", width = 5, height = 5, units = "in", dpi = 600)
anova_result <- aov(thallus.thickness..mm. ~ Plot, data=lichen_data)
summary(anova_result)
tukey_result <- TukeyHSD(anova_result)

# Print the Tukey HSD result
print(tukey_result)

##thallus thickness and location with updated colors
lichen_data <- lichen_data %>%
    mutate(Plot = recode(Plot,
                         "Altos" = "Low",
                         "Estacion Low" = "Middle",
                         "Estacion High" = "High"),
           Plot = factor(Plot, levels = c("Low", "Middle", "High")))

# Define the specific colors for each plot category
custom_colors <- c("Low" = "coral1", "Middle" = "palegreen3", "High" = "steelblue2")

# Create the bar plot with mean and distribution data
ggplot(lichen_data, aes(x = Plot, y = thickness, fill = Plot)) + 
    geom_boxplot() +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white", color = "black") +
    labs(title = "Thallus Thickness and Site",
         x = "Site",
         y = "Thallus Thickness (mm)") +
    theme_minimal() +
    scale_fill_manual(values = custom_colors)
# Check ANOVA results
anova_result <- aov(thallus.thickness..mm. ~ Plot, data = lichen_data)
summary(anova_result)

ggsave("figure_4.jpeg", width = 5, height = 5, units = "in", dpi = 600)


