# Libraries
library(tidyverse) # Wrangling + Visualizations
library(rstudioapi) # Allows dynamic working directory
library(ggthemes) # Download themes for visualizations
library(reshape2) # Visualize correlation matrix

# Set Working Directory
setwd(dirname(getActiveDocumentContext()$path))

# Import Data
uhc <- read_csv("../data/uhcCoverage.csv")
expenses <- read_csv("../data/population10SDG3.8.2.csv")
life_expectancy <- read_csv("../data/lifeExpectancyAtBirth.csv")
infant_mortality <- read_csv("../data/infantMortalityRate.csv")
suicides <- read_csv("../data/crudeSuicideRates.csv")

# Pre-Merge Cleaning
uhc <- uhc %>%
  rename("uhc" = `First Tooltip`) %>%
  filter(Period == 2015) %>%
  select(-c(`Indicator`, `Period`)) 

expenses <- expenses %>%
  rename("pop_high_costs" = `First Tooltip`) %>% # % of population with healthcare costs > 10% of household expenses or income
  filter(Period == 2015, Dim1 == "Total") %>% 
  select(-c(`Indicator`, `Period`, `Dim1`)) 

suicides <- suicides %>%
  rename("suicide_rate" = `First Tooltip`) %>% # suicides per 100k population
  filter(Period == 2015, Dim1 == "Both sexes") %>%
  select(-c(`Indicator`, `Period`, `Dim1`)) 

life_expectancy <- life_expectancy %>%
  rename("life_expectancy" = `First Tooltip`) %>% # life expectancy at birth
  filter(Period == 2015, Dim1 == "Both sexes") %>%
  select(-c(`Indicator`, `Period`, `Dim1`)) 

infant_mortality <- infant_mortality %>%
  rename("infant_mortality" = `First Tooltip`) %>% # probability of dying between birth and age 1 per 1000 live births
  filter(Period == 2015, Dim1 == "Both sexes") %>%
  select(-c(`Indicator`, `Period`, `Dim1`)) 
infant_mortality$infant_mortality <- as.numeric(gsub("\\[.*", "", infant_mortality$infant_mortality))

# Merge the Data
all_data <- full_join(expenses, infant_mortality, by = c("Location")) %>%
  full_join(life_expectancy, by = c("Location")) %>%
  full_join(suicides, by = c("Location")) %>%
  full_join(uhc, by = c("Location"))

# Clean the Merged Data
all_data$pop_high_costs[is.na(all_data$pop_high_costs)] = 0 # change NAs to 0 for population with healthcare costs > 10% of household expenses or income
all_data <- na.omit(all_data) # remove NAs for all other variables for complete data

# View the Data
head(all_data)

# Examine Variable Distributions
summary(all_data)

# Examine Histograms of Each Variable
hist(all_data$pop_high_costs, main = "High Cost Healthcare Distribution", xlab = "Percent of Houselholds with High Cost Healthcare")
hist(all_data$infant_mortality, main = "Infant Mortality Distribution", xlab = "Probability of Dying Between Birth and Age 1 per 1000")
hist(all_data$life_expectancy, main = "Life Expectancy Distribution", xlab = "Life Expectancy at Birth")
hist(all_data$suicide_rate, main = "Suicide Rate Distribution", xlab = "Suicides per 100K Population")
hist(all_data$uhc, main = "Universal Healthcare Index Distribution", xlab = "Universal Healthcare Index")

# EDA - Top and Bottom 5
# Top 5 countries with highest Percent of Houselholds with High Cost Healthcare
all_data %>%
  slice_max(n = 5, pop_high_costs) %>%
  summarize(Location, pop_high_costs)

# Top 5 countries with highest infant mortality rates
all_data %>%
  slice_max(n = 5, infant_mortality) %>%
  summarize(Location, infant_mortality)

# Bottom 5 countries with lowest infant mortality rates
all_data %>%
  slice_min(n = 5, infant_mortality) %>%
  summarize(Location, infant_mortality)

# Top 5 countries with highest life expectancy
all_data %>%
  slice_max(n = 5, life_expectancy) %>%
  summarize(Location, life_expectancy)

# Bottom 5 countries with lowest life expectancy
all_data %>%
  slice_min(n = 5, life_expectancy) %>%
  summarize(Location, life_expectancy)

# Top 5 countries with highest suicide rate
all_data %>%
  slice_max(n = 5, suicide_rate) %>%
  summarize(Location, suicide_rate)

# Bottom 5 countries with lowest suicide rate
all_data %>%
  slice_min(n = 5, suicide_rate) %>%
  summarize(Location, suicide_rate)

# Top 5 countries with highest UCH Index
all_data %>%
  slice_max(n = 5, uhc) %>%
  summarize(Location, uhc)

# Bottom 5 countries with lowest UCH index
all_data %>%
  slice_min(n = 5, uhc) %>%
  summarize(Location, uhc)

# Correlation Matrix
all_data.cor <- round(cor(all_data[-c(1)]), 2)
# Get lower triangle of the correlation matrix
get_lower_tri<-function(all_data.cor){
  all_data.cor[upper.tri(all_data.cor)] <- NA
  return(all_data.cor)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(all_data.cor){
  all_data.cor[lower.tri(all_data.cor)]<- NA
  return(all_data.cor)
}
upper_tri <- get_upper_tri(all_data.cor)
melted_cormat <- melt(upper_tri, na.rm = TRUE)

# Visualize the Coorelation Matrix
cormat_plot <- ggplot(data = melted_cormat, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  coord_fixed() +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank())
cormat_plot

# The Relationship between Suicide Rate and UHC
# Linear Regression Model: Does UHC Index Predict Suicide Rates?
uhc_suic <- lm(suicide_rate ~ uhc, data = all_data)

# Output
summary(uhc_suic)

# Assess Assumptions of the Model
plot(uhc_suic, 1) # Residuals vs Fitted assesses the linearity assumption
plot(uhc_suic, 2) # Normal Q-Q plot asseses the normality of residuals assumption 
plot(uhc_suic, 3) # Scale-location assess the homogeneity of variance assumption

# Visualize the Model
uhc_suic_lr <- ggplot(all_data, aes(`uhc`, `suicide_rate`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Simple Linear Regression Model",
       subtitle = "Does Universal Healthcare Index of Service Coverage Predict Suicide Rates?",
       x = "Universal Healthcare Index",
       y = "Suicides/100k Population",
       caption = expression("R"^2*" = .09, p < .001")) +
  theme_solarized() +
  theme(axis.title = element_text(), 
        legend.position = "right", 
        legend.direction = "vertical", 
        text = element_text(family = "Constantia"))
uhc_suic_lr

# Weighted Least Squares Regression (because of hetereogeneity of variances)
# Residuals vs Fitted Plot
data.frame(y = rstandard(uhc_suic),
           x = uhc_suic$fitted.values) %>%
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(title = "Standardized Residuals vs Fitted Values Plot")

# Create Weights
lm_weights <- 1 / lm((uhc_suic$residuals)^2 ~ uhc_suic$fitted.values)$fitted.values^2

# Weighted Least Squares Regression Model
wlsr <- lm(suicide_rate ~ uhc, data = all_data, weights = lm_weights)

# Output
summary(wlsr)

# Visualize Weighted Least Squares Regression Model
uhc_suic_wlsr <- ggplot(data = all_data,
       aes(y = suicide_rate,
           x = uhc)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE,
              aes(weight = lm_weights),
              color = "black",
              size = .5,
              linetype = "dashed") +
  labs(title = "Weighted Least Squares Regression Model",
       subtitle = "Does Universal Healthcare Index of Service Coverage Predict Suicide Rates?",
       x = "Universal Healthcare Index",
       y = "Suicides/100k Population",
       caption = expression("R"^2*" = .29, p < .001")) +
  theme_solarized() +
  theme(axis.title = element_text(), 
        legend.position = "right", 
        legend.direction = "vertical", 
        text = element_text(family = "Constantia"))
uhc_suic_wlsr

# Visualize Weighted Least Squares vs Ordinary Squares Model
w_vs_o <- ggplot(data = all_data,
       aes(y = suicide_rate,
           x = uhc)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE,
              aes(weight = lm_weights),
              color = "black",
              size = .5,
              linetype = "dashed") +
  geom_smooth(method = "lm", se = FALSE,
              color = "red",
              size = .5,
              linetype = "dashed") +
  labs(title = "Ordinary vs Weighted Least Squares Regression Model",
       subtitle = "Does Universal Healthcare Index of Service Coverage Predict Suicide Rates?",
       x = "Universal Healthcare Index",
       y = "Suicides/100k Population") +
  theme_solarized() +
  theme(axis.title = element_text(), 
        legend.position = "right", 
        legend.direction = "vertical", 
        text = element_text(family = "Constantia"))
w_vs_o
# Black Dotted Line: OLSR; Red Dotted Line: WLSR

# The Relationship between UHC and Life Expectancy
le_uhc_model <- lm(life_expectancy ~ uhc, data = all_data)

# Ouput
summary(le_uhc_model)
  
# Assess Assumptions of the Model
plot(le_uhc_model, 1) # Residuals vs Fitted assesses the linearity assumption
plot(le_uhc_model, 2) # Normal Q-Q plot asseses the normality of residuals assumption 
plot(le_uhc_model, 3) # Scale-location assess the homogeneity of variance assumption

# Visualize the Model
le_uhc_model_vis <- ggplot(all_data, aes(uhc, life_expectancy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Simple Linear Regression Model",
       subtitle = "Does Universal Healthcare Index of Service Coverage Predict Life Expectancy?",
       x = "Universal Healthcare Index",
       y = "Life Expectancy",
       caption = expression("R"^2*" = .75, p < .001")) +
  theme_solarized() +
  theme(axis.title = element_text(), 
        legend.position = "right", 
        legend.direction = "vertical", 
        text = element_text(family = "Constantia"))
le_uhc_model_vis

# The Relationship between UHC and Infant Mortality
im_uhc_model <- lm(infant_mortality ~ uhc, data = all_data)
summary(im_uhc_model)

# Assess Assumptions of the Model
plot(im_uhc_model, 1) # Residuals vs Fitted assesses the linearity assumption
plot(im_uhc_model, 2) # Normal Q-Q plot asseses the normality of residuals assumption 
plot(im_uhc_model, 3) # Scale-location assess the homogeneity of variance assumption

# Visualize the Model
im_uhc_model_vis <- ggplot(all_data, aes(uhc, infant_mortality)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Simple Linear Regression Model",
       subtitle = "Does Universal Healthcare Index of Service Coverage Predict Infant Mortality?",
       x = "Universal Healthcare Index",
       y = "Infant Mortality Rate per 1000 Live Births",
       caption = expression("R"^2*" = .77, p < .001")) +
  theme_solarized() +
  theme(axis.title = element_text(), 
        legend.position = "right", 
        legend.direction = "vertical", 
        text = element_text(family = "Constantia"))
im_uhc_model_vis

