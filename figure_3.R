##this is the r code for figure 3:
#ppsize, ppdensity, tt, proportion on x
#k-values on y axis 
library(lme4) 

#install this
library(ggplot2)
lichen_data <- read.csv("updated_lichen_data.csv")




#thallus thickness
#once I get lme4 actually installed, run this new code for all functional traits

library(Matrix)


library(performance)


plot(lichen_data$thallus.thickness..mm., lichen_data$k,
     pch = 19,
     xlab= "Thallus Thickness (mm)",
     ylab= "k (Drying Rate Constant",
     main = "Thallus Thickness and k",
     col= "darkseagreen")
#add models
# linear model

library(nlme) 
width_abundance_model <- lme(fixed = k ~ thallus.thickness..mm.,
                             random = ~ 1 | Plot,
                             data = lichen_data)
summary(width_abundance_model)
r2(width_abundance_model)

#PPsize
plot(lichen_data$Pseudocyophellae.size..avg.of.10., lichen_data$k,
     pch = 19,
     xlab= "Average Pseudocyphellae Size (cm^2)",
     ylab= "k (Drying Rate Constant",
     main = "Pseudocyphellae Size and k",
     col= "darkseagreen")
#add models
# linear model


width_abundance_model <- lme(fixed = k ~ Pseudocyophellae.size..avg.of.10.,
                             random = ~ 1 | Plot,
                             data = lichen_data)
summary(width_abundance_model)
abline(width_abundance_model)

#ppdensity

plot(lichen_data$pp_density, lichen_data$k,
     pch = 19,
     xlab= "Pseudocyphellae per cm^2",
     ylab= "k (Drying Rate Constant",
     main = "Pseudocyphellae Density and k",
     col= "darkseagreen")
#add models
# linear model
width_abundance_model <- lme(fixed = k ~ pp_density,
                             random = ~ 1 | Plot,
                             data = lichen_data)
summary(width_abundance_model)
abline(width_abundance_model)


#proportion

plot(lichen_data$proportion_covered, lichen_data$k,
     pch = 19,
     xlab= "Proportion of Thallus Covered by Pseudocyphellae",
     ylab= "k (Drying Rate Constant",
     main = "Pseudocyphellae Area Proportion and k",
     col= "darkseagreen")
#add models
# linear model
width_abundance_model <- lme(fixed = k ~ proportion_covered,
                             random = ~ 1 | Plot,
                             data = lichen_data)
summary(width_abundance_model)
abline(width_abundance_model)


#area

plot(lichen_data$area..cm.2., lichen_data$k,
     pch = 19,
     xlab= "Thallus Area (cm^2)",
     ylab= "k (Drying Rate Constant",
     main = "Thallus Area and k",
     col= "darkseagreen")
#add models
# linear model
width_abundance_model <- lme(fixed = k ~ area..cm.2.,
                             random = ~ 1 | Plot,
                             data = lichen_data)
summary(width_abundance_model)
abline(width_abundance_model)
r2(width_abundance_model)

#Specific thallus mass (import this into the dataset first)

plot(lichen_data$STM..mass.area., lichen_data$k,
     pch = 19,
     xlab= "Specific Thallus Mass",
     ylab= "k (Drying Rate Constant",
     main = "Specific Thallus Mass and k",
     col= "darkseagreen")
#add models
# linear model
width_abundance_model <- lme(fixed = k ~ STM..mass.area.,
                             random = ~ 1 | Plot,
                             data = lichen_data)
summary(width_abundance_model)
abline(width_abundance_model)
r2(width_abundance_model)

#WHC

plot(lichen_data$WHC...wet.dry..dry.x.100.., lichen_data$k,
     pch = 19,
     xlab= "Water Holding Capacity",
     ylab= "k (Drying Rate Constant",
     main = "Water Holding Capacity and k",
     col= "darkseagreen")
#add models
# linear model
width_abundance_model <- lme(fixed = k ~ WHC...wet.dry..dry.x.100..,
                             random = ~ 1 | Plot,
                             data = lichen_data)
summary(width_abundance_model)
abline(width_abundance_model)
r2(width_abundance_model)

###-------------sig variables all in one figure-------------
#group the sig ones in a 4 panel plot here

#  2x2 plotting layout
window()
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1), oma = c(0, 0, 0, 0))

par(pty = "s")

plot(lichen_data$WHC...wet.dry..dry.x.100.., lichen_data$k,
     pch = 19,
     xlab= "Water Holding Capacity (%)",
     ylab= "k (Drying Rate Constant",
     main = "Water Holding Capacity and k",
     col= "darkseagreen")
width_abundance_model <- lm(lichen_data$k ~ lichen_data$WHC...wet.dry..dry.x.100..)
summary(width_abundance_model)
abline(width_abundance_model)

plot(lichen_data$thallus.thickness..mm., lichen_data$k,
     pch = 19,
     xlab= "Thallus Thickness (mm)",
     ylab= "k (Drying Rate Constant",
     main = "Thallus Thickness and k",
     col= "darkseagreen")
width_abundance_model <- lm(lichen_data$k ~ lichen_data$thallus.thickness..mm.)
summary(width_abundance_model)
abline(width_abundance_model)

plot(lichen_data$area..cm.2., lichen_data$k,
     pch = 19,
     xlab= "Thallus Area (cm^2)",
     ylab= "k (Drying Rate Constant",
     main = "Thallus Area and k",
     col= "darkseagreen")
width_abundance_model <- lm(lichen_data$k ~ lichen_data$area..cm.2.)
summary(width_abundance_model)
abline(width_abundance_model)
plot(lichen_data$STM..mass.area., lichen_data$k,
     pch = 19,
     xlab= "Specific Thallus Mass",
     ylab= "k (Drying Rate Constant",
     main = "Specific Thallus Mass and k",
     col= "darkseagreen")
width_abundance_model <- lm(lichen_data$k ~ lichen_data$STM..mass.area.)
summary(width_abundance_model)
abline(width_abundance_model)
ggsave("figure_3.jpeg", width = 5, height = 5, units = "in", dpi = 600)

#combined plot figure!!
# Set up the plotting window with 2x2 grid layout

jpeg("figure2.jpeg", width = 7, height = 7, units = "in", res = 600)
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1), oma = c(0, 0, 0, 0), pty = "s")

# Plot for Water Holding Capacity
plot(lichen_data$WHC...wet.dry..dry.x.100.., lichen_data$k,
     pch = 19,
     xlab= "Water Holding Capacity (%)",
     ylab= "k (Drying Rate Constant)",
     
     col= "black")
title(main = "A", adj = 0)
width_abundance_model <- lm(lichen_data$k ~ lichen_data$WHC...wet.dry..dry.x.100..)
adj_r_squared <- summary(width_abundance_model)$adj.r.squared
abline(width_abundance_model)
mtext(sprintf("Adj R²: %.3f", adj_r_squared), side = 3, line = -1)

# Plot for Thallus Thickness
plot(lichen_data$thallus.thickness..mm., lichen_data$k,
     pch = 19,
     xlab= "Thallus Thickness (mm)",
     ylab= "k (Drying Rate Constant)",
     
     col= "black")
title(main = "B", adj = 0)
width_abundance_model <- lm(lichen_data$k ~ lichen_data$thallus.thickness..mm.)
adj_r_squared <- summary(width_abundance_model)$adj.r.squared
abline(width_abundance_model)
mtext(sprintf("Adj R²: %.3f", adj_r_squared), side = 3, line = -1)

# Plot for Thallus Area
plot(lichen_data$area..cm.2., lichen_data$k,
     pch = 19,
     xlab= expression("Thallus Area (" * cm^2 * ")"),
     ylab= "k (Drying Rate Constant)",
     col= "black")
title(main = "C", adj = 0)
width_abundance_model <- lm(lichen_data$k ~ lichen_data$area..cm.2.)
adj_r_squared <- summary(width_abundance_model)$adj.r.squared
abline(width_abundance_model)
mtext(sprintf("Adj R²: %.3f", adj_r_squared), side = 3, line = -1)

# Plot for Specific Thallus Mass
plot(lichen_data$STM..mass.area., lichen_data$k,
     pch = 19,
     xlab= "Specific Thallus Mass",
     ylab= "k (Drying Rate Constant)",
     col= "black")
title(main = "D", adj = 0)
width_abundance_model <- lm(lichen_data$k ~ lichen_data$STM..mass.area.)
adj_r_squared <- summary(width_abundance_model)$adj.r.squared
abline(width_abundance_model)
mtext(sprintf("Adj R²: %.3f", adj_r_squared), side = 3, line = -1)

dev.off()

#how should I label each figure (a,b,c,d?)
#should I have an overasll title?



#-----------combined models--------------
#put all signigficant ones in one figure/model(TT, STM, WHC, area)


# combined model
combined_model <- lm(k ~ thallus.thickness..mm. + STM..mass.area. + WHC...wet.dry..dry.x.100.. + area..cm.2., data = lichen_data)

summary(combined_model)

# Plot 
plot(fitted(combined_model), lichen_data$k,
     pch = 19,
     xlab = "Fitted k (Predicted)",
     ylab = "Observed k (Actual)",
     main = "STM + WHC + Thallus Thickness + Area and k",
     col = "darkseagreen")

abline(0, 1, col = "blue")

##try this to display r adj on the figure
# Combined model
combined_model <- lm(k ~ thallus.thickness..mm. + STM..mass.area. + WHC...wet.dry..dry.x.100.. + area..cm.2., data = lichen_data)

# Display adjusted R^2
adj_r_squared <- summary(combined_model)$adj.r.squared
cat("Adjusted R^2:", adj_r_squared, "\n")

# Plot 
plot(fitted(combined_model), lichen_data$k,
     pch = 19,
     xlab = "Fitted k (Predicted)",
     ylab = "Observed k (Actual)",
     main = "STM + WHC + Thallus Thickness + Area and k",
     col = "darkseagreen")

abline(0, 1, col = "blue")





#try this subsets model to find the best combinations of variables to yield a high r-squared
# Install and load the leaps package if not already installed
library(leaps)
library(performance)

cor(lichen_data[, c("thallus.thickness..mm.", "STM..mass.area.", "WHC...wet.dry..dry.x.100..", "area..cm.2.")])
check_collinearity(lm(k ~ thallus.thickness..mm. + STM..mass.area. + WHC...wet.dry..dry.x.100.. + area..cm.2., 
                      data = lichen_data))

best_subset_model <- regsubsets(
    k ~ thallus.thickness..mm. + STM..mass.area. + WHC...wet.dry..dry.x.100.. + area..cm.2., 
    data = lichen_data, 
    nvmax = 4  # Maximum number of predictors in the model
)

# Summarize the best subsets model
summary(best_subset_model)


# Best subsets regression to find the best combination of variables
best_subset_model <- regsubsets(k ~ thallus.thickness..mm. + STM..mass.area. + WHC...wet.dry..dry.x.100.. + area..cm.2., 
                                data = lichen_data, 
                                nvmax = 4)  # nvmax is the max number of predictors


# Summary of the best subsets model
summary(best_subset_model)

#run this in performance package and check for co-linearity 
#many of these variables relate to one another 


###create table 1

# Extract model summary
combined_model_summary <- summary(combined_model)
print(combined_model_summary)

# Extract coefficients table (estimates, standard errors, t-values, p-values)
coef_table <- combined_model_summary$coefficients



# Create a data frame for the model summary
summary_table <- as.data.frame(coef_table)

# Round the values for better readability
summary_table <- round(summary_table, digits = 3)
nrow(summary_table)  # Check the number of rows

# View the table
summary_table
# Rename the row names (variables) in the summary table
# Rename the row names (5 rows)
rownames(summary_table) <- c("Intercept", 
                             "Thallus Thickness (mm)", 
                             "Specific Thallus Mass", 
                             "Water Holding Capacity (%)", 
                             "Thallus Area (cm²)")

# Check the renamed table
summary_table

# Save as CSV
write.csv(summary_table, file = "model_summary_table.csv", row.names = TRUE)

# Save as a formatted table (if you're using knitr or xtable for LaTeX)
library(knitr)
kable(summary_table, format = "latex", caption = "Summary of Combined Model")

install.packages("webshot")
library(webshot)
install.packages("kableExtra")
# Load necessary libraries
library(knitr)
library(kableExtra)
webshot::install_phantomjs()
# Create the table
kable(summary_table, format = "html", caption = "Summary of Combined Model") %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
    add_header_above(c(" " = 1, "Model Summary" = 4))

# Create the HTML table and save it as an HTML file
html_table <- kable(summary_table, format = "html", caption = "Summary of Combined Model") %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
    add_header_above(c(" " = 1, "Model Summary" = 4))

# Save the table as an HTML file
html_file <- "summary_table.html"
save_kable(html_table, file = html_file)

# Convert the HTML file to a PNG
png_file <- "summary_table.png"
webshot(html_file, file = png_file)

# Output file paths
cat("Saved PNG at:", png_file, "\n")

#now get the r-squared values for this 

#2var
TT_WHC_model <- lm(k ~ thallus.thickness..mm. + WHC...wet.dry..dry.x.100.. , data = lichen_data)

summary(TT_WHC_model)
#Adjusted R-squared:  0.4623 
#p-value: 7.829e-09

#3var

TT_WHC_Area_model <- lm(k ~ thallus.thickness..mm. + area..cm.2. + WHC...wet.dry..dry.x.100.. , data = lichen_data)

summary(TT_WHC_Area_model)
#Multiple R-squared:  0.5338,	Adjusted R-squared:  0.5089 
#p-value: 2.351e-09

#so it looks like the best model is the combined model
#all 4 sig variables have important co-variance? how can this be???





lichen_data <- read.csv("updated_lichen_data.csv")
print(lichen_data)
install.packages("ggpubr")
library(ggplot2)
library(ggpubr)





#arrange: tt(2), stm(4), whc(1), ta(3)



#--------------------------HERES the updated one (2/24/25)


# Plot 1: Water Holding Capacity and k
lm_info1 <- lm(k ~ WHC...wet.dry..dry.x.100.., data = lichen_data)
coef1 <- coef(lm_info1)  # Correct extraction of coefficients
summary_info1 <- summary(lm_info1)
p_val1 <- summary_info1$coefficients[2, 4]    # p-value for the slope
r2_1 <- summary_info1$r.squared                # R² value

plot1 <- ggplot(lichen_data, aes(x = WHC...wet.dry..dry.x.100.., y = k, color = Plot)) +  
    geom_point() +
    geom_abline(intercept = coef1[1], slope = coef1[2], color = "black") +  
    labs(x = "Water Holding Capacity", y = "k (Drying Rate Constant)", 
         title = "Water Holding Capacity and k")

# Plot 2: Thallus Thickness and k
lm_info2 <- lm(k ~ thallus.thickness..mm., data = lichen_data)
coef2 <- coef(lm_info2)
summary_info2 <- summary(lm_info2)
p_val2 <- summary_info2$coefficients[2, 4]    # p-value for thallus.thickness..mm.
r2_2 <- summary_info2$r.squared                # R² value

plot2 <- ggplot(lichen_data, aes(x = thallus.thickness..mm., y = k, color = Plot)) +  
    geom_point() +
    geom_abline(intercept = coef2[1], slope = coef2[2], color = "black") +  
    labs(x = "Thallus Thickness (mm)", y = "k (Drying Rate Constant)", title = "Thallus Thickness and k")

# Plot 3: Thallus Area and k
lm_info3 <- lm(k ~ area..cm.2., data = lichen_data)
coef3 <- coef(lm_info3)
summary_info3 <- summary(lm_info3)
p_val3 <- summary_info3$coefficients[2, 4]    # p-value for area..cm.2.
r2_3 <- summary_info3$r.squared                # R² value

plot3 <- ggplot(lichen_data, aes(x = area..cm.2., y = k, color = Plot)) +  
    geom_point() +
    geom_abline(intercept = coef3[1], slope = coef3[2], color = "black") +  
    labs(x = "Thallus Area (cm²)", y = "k (Drying Rate Constant)", title = "Thallus Area and k") 


# Plot 4: Specific Thallus Mass and k
lm_info4 <- lm(k ~ STM..mass.area., data = lichen_data)
coef4 <- coef(lm_info4)
summary_info4 <- summary(lm_info4)
p_val4 <- summary_info4$coefficients[2, 4]    # p-value for STM..mass.area.
r2_4 <- summary_info4$r.squared                # R² value

plot4 <- ggplot(lichen_data, aes(x = STM..mass.area., y = k, color = Plot)) +  
    geom_point() +
    geom_abline(intercept = coef4[1], slope = coef4[2], color = "black") +  
    labs(x = "Specific Thallus Mass", y = "k (Drying Rate Constant)", title = "Specific Thallus Mass and k")


ggarrange(plot2, plot4, plot1, plot3, ncol = 2, nrow = 2)

