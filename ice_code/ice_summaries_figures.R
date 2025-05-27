#### Great Lakes Ice ###########

#load libraries 
library(tidyverse)# for cleaning
library(lubridate) # for dates
library(ggplot2) # for plots




#### read in CFDD and NMDD data ####

#CFDD and NMDD have daily values from October 1 to April 30 each winter.  

# Define the folder containing the files
input_folder <- "ice_data/raw_data/"
output_folder <- "ice_data/"

# List all CSV files in the folder
files <- list.files(path = input_folder, pattern = "\\.csv$", full.names = TRUE)


# Loop over each file
for (file in files) {
  
  # Read the file 
  raw_dd <- read.csv(file) %>%
  mutate(year = year(DATE), month = month(DATE), day = day(DATE))

#* create a new table with max for each year- e.g. October 1 1897 to April 30 1898  will be 1898
year_max<-raw_dd %>%
  mutate(
    stat_year = ifelse(month(DATE) %in% c(10:12), year(DATE) + 1, year(DATE)) #assign the year based on winter season
  ) %>%
  group_by(stat_year) %>%
  summarise(max_CFDD = max(CFDD), 
            min_NMDD = min(NMDD))

  
  # Create a new filename for saving
  output_file <- file.path(output_folder, paste0("summary_", basename(file)))
  
  # Save the processed data to a new file
  write.csv(year_max, output_file, row.names = FALSE)
  
  # Print a message to show progress
  cat("Processed and saved:", output_file, "\n")
}

#### select years to use for historical mean and cv maps #### 
#file with the data
input_folder <- "ice_data/"
# List all CSV files in the folder
files <- list.files(path = input_folder, pattern = "\\.csv$", full.names = TRUE)

# Loop over each file and read the CSV, then assign each file to a variable in the environment
for (file in files) {
  # Read the current CSV file
  data <- read.csv(file)
  
  # Create a variable name based on the filename (remove the path and extension)
  var_name <- gsub(".csv$", "", basename(file))  # Remove .csv extension from filename
  
  # Assign the data to a variable in the global environment
  assign(var_name, data)
  
} 


summary_Erie_DD_estimates<-summary_Erie_DD_estimates %>% 
  dplyr::mutate(lake = "erie")
summary_Huron_DD_estimates<-summary_Huron_DD_estimates %>% 
  dplyr::mutate(lake = "huron")
summary_Michigan_DD_estimates<-summary_Michigan_DD_estimates %>% 
  dplyr::mutate(lake = "michigan")
summary_Ontario_DD_estimates<-summary_Ontario_DD_estimates %>% 
  dplyr::mutate(lake = "ontario")
summary_Superior_DD_estimates<-summary_Superior_DD_estimates %>% 
  dplyr::mutate(lake = "superior")

dd_estimates<-rbind(summary_Erie_DD_estimates, summary_Huron_DD_estimates, summary_Michigan_DD_estimates, summary_Ontario_DD_estimates, summary_Superior_DD_estimates) %>% 
  rename(year=stat_year)

lake_mean<-dd_estimates %>%
  filter(year >= 1898 & year <= 1960) %>%
  group_by(lake) %>% 
  summarise(hist_mean = mean(max_CFDD),
            SD = sd(max_CFDD)) %>% 
  mutate(pctSD= 0.2*SD,
    high = hist_mean + (0.2*SD), #30% of the SD
         low = hist_mean - (0.2*SD) ) 


ice_selection<-left_join(dd_estimates, lake_mean, by = "lake") %>%
  filter(year >= 1979 & year <= 2014) %>% #select years with spatial data
  mutate(include = ifelse(max_CFDD < high & max_CFDD > low, "YES", "NO")) # if the max cfdd falls within the SD range




#### plots #### 

#* plots for the conceptual diagram ####
#file with the data
input_folder <- "ice_data/"
# List all CSV files in the folder
files <- list.files(path = input_folder, pattern = "\\.csv$", full.names = TRUE)

# Loop over each file and read the CSV, then assign each file to a variable in the environment
for (file in files) {
  # Read the current CSV file
  data <- read.csv(file)
  
  # Create a variable name based on the filename (remove the path and extension)
  var_name <- gsub(".csv$", "", basename(file))  # Remove .csv extension from filename
  
  # Assign the data to a variable in the global environment
  assign(var_name, data)

}

# Create a color variable to specify which points are red and which are blue
summary_Erie_DD_estimates<-dplyr::filter(summary_Erie_DD_estimates, stat_year<2015)
summary_Erie_DD_estimates$color <- ifelse(summary_Erie_DD_estimates$stat_year == 1988 | summary_Erie_DD_estimates$stat_year == 2004 | summary_Erie_DD_estimates$stat_year == 2005 | summary_Erie_DD_estimates$stat_year == 2010, "red", "darkblue")


spatial_years_fig<-ggplot(summary_Erie_DD_estimates, aes(x = stat_year, y = max_CFDD)) +
  geom_line(color = "darkblue") +  # Add the line
  geom_point(aes(color = color), size = 3) +  # Points with specific colors
  scale_color_identity() +  # Use the actual colors (red/blue) without scale
  geom_hline(yintercept = c(255, 311), linetype = "dashed", color = "black") +  # Add horizontal lines 
  geom_hline(yintercept = c(283), linetype = "solid", color = "black") + 
  labs(x = "Year", y = "Max CFDD") +
  theme_minimal() +   
  geom_rect(aes(xmin = 1898, xmax = 1960, ymin = -Inf, ymax = Inf), fill = NA, color = "black", linewidth = 0.5) +  # Box for historical
  geom_rect(aes(xmin = 1979, xmax = 2015, ymin = -Inf, ymax = Inf), fill = NA, color = "black", linewidth = 0.5) +  # Box for contemporary
  annotate("text", x = 1929, y = 664, label ="Historical Time Period",
            color = "black", size = 4) +  # Text for the first box
  annotate("text", x = 1997, y = 664, label ="Spatial Data Time Period",
            color = "black", size = 4) 

ggsave("ice_figures/spatial_years_fig.png", spatial_years_fig,
       bg = "#ffffff",
       width = 8, 
       height = 4, 
       units = c("in"))

erie_subset<-Erie_DD_estimates%>%
  mutate(date = as.Date(DATE)) %>% 
  dplyr::filter(date >= "1921-10-01" & date <= "1924-04-30") %>% 
  mutate(color = ifelse(CFDD == 286.06| CFDD == 275.09 | CFDD ==218.49,  "red", "darkblue"), 
         condition = ifelse(CFDD == 286.06| CFDD == 275.09 | CFDD ==218.49,  1, 0.95)
)


daily_cfdd<-ggplot(erie_subset, aes(x = date, y = CFDD)) +
  geom_line(color = "darkblue") +  # Add the line
  geom_point(aes(color = color, alpha=condition), size = 3) +
  scale_color_identity() +
  theme_minimal()+
  theme(legend.position = "none") +
  labs(x = "Year-Month", y = "Daily CFDD")

ggsave("ice_figures/daily_cfdd.png", daily_cfdd,
       bg = "#ffffff",
       width = 8, 
       height = 4, 
       units = c("in"))

#### read in spatial mean values for ice cover and duration####
spatial_means<-read.csv("ice_data/spatial_means.csv")
summary_Erie_DD_estimates<-summary_Erie_DD_estimates %>% 
  dplyr::mutate(lake = "erie")
summary_Huron_DD_estimates<-summary_Huron_DD_estimates %>% 
  dplyr::mutate(lake = "huron")
summary_Michigan_DD_estimates<-summary_Michigan_DD_estimates %>% 
  dplyr::mutate(lake = "michigan")
summary_Ontario_DD_estimates<-summary_Ontario_DD_estimates %>% 
  dplyr::mutate(lake = "ontario")
summary_Superior_DD_estimates<-summary_Superior_DD_estimates %>% 
  dplyr::mutate(lake = "superior")

dd_estimates<-rbind(summary_Erie_DD_estimates, summary_Huron_DD_estimates, summary_Michigan_DD_estimates, summary_Ontario_DD_estimates, summary_Superior_DD_estimates) %>% 
  rename(year=stat_year)

ice_join<-left_join(spatial_means, dd_estimates ) #join by year and lake

#*plots for linear regression ####
# Custom colors for the groups
custom_colors <- c("erie" = "#E69F00", "huron" = "#56B4E9", "michigan" = "#F0E442", "ontario" ="#CC79A7", "superior" =  "#009E73")


ice_cover<-ggplot(ice_join, aes(x = max_CFDD, y = glahf_mean_cover, color = lake)) +
  geom_point(size = 1) +  # Add points for each data
  geom_smooth(method = "lm", se = FALSE) +  # Linear regression lines, no confidence interval
  scale_color_manual(values = custom_colors) +
  labs( x = "Max CFDD", y = "Mean Ice Cover") +
  theme_minimal() +
  theme(legend.position = c(0.85, 0.2))  # Move the legend inside the plot

ice_duration<-ggplot(ice_join, aes(x = max_CFDD, y = glahf_mean_duration, color = lake)) +
  geom_point(size = 1) +  # Add points for each data
  geom_smooth(method = "lm", se = FALSE) +  # Linear regression lines, no confidence interval
  scale_color_manual(values = custom_colors) +
  labs( x = "Max CFDD", y = "Mean Ice Duration") +
  theme_minimal()+
  theme(legend.position = c(0.85, 0.2))

ice_cover_nmdd<-ggplot(ice_join, aes(x = min_NMDD, y = glahf_mean_cover, color = lake)) +
  geom_point(size = 1) +  # Add points for each data
  geom_smooth(method = "lm", se = FALSE) +  # Linear regression lines, no confidence interval
  scale_color_manual(values = custom_colors) +
  labs( x = "Min NMDD", y = "Mean Ice Cover") +
  theme_minimal() +
  theme(legend.position = c(0.2, 0.2))

ice_duration_nmdd<-ggplot(ice_join, aes(x = min_NMDD, y = glahf_mean_duration, color = lake)) +
  geom_point(size = 1) +  # Add points for each data
  geom_smooth(method = "lm", se = FALSE) +  # Linear regression lines, no confidence interval
  scale_color_manual(values = custom_colors) +
  labs( x = "Min NMDD", y = "Mean Ice Duration") +
  theme_minimal()+
  theme(legend.position = c(0.2, 0.2))



linear_reg<-cowplot::plot_grid(ice_cover, ice_duration, ice_cover_nmdd, ice_duration_nmdd,
                               nrow=2, labels = c('a','b', 'c', 'd'))

ggsave("ice_figures/linear_reg.png", linear_reg,
       bg = "#ffffff",
       width = 12, 
       height = 12, 
       units = c("in"))

#* summary of linear regresssion ####
# Initialize an empty data frame to store the results
results_df <- data.frame(
  group = character(),
  slope = numeric(),
  intercept = numeric(),
  r_squared = numeric(),
  stringsAsFactors = FALSE
)

# Loop over each lake and extract the slope, intercept, and R-squared
for (group_name in unique(ice_join$lake)) {
  # Fit the linear model for the current group
  lm_model <- lm(glahf_mean_cover ~ max_CFDD, data = ice_join[ice_join$lake == group_name, ])
  
  # Get the summary of the linear model
  lm_summary <- summary(lm_model)
  
  # Extract the slope (coefficient for x), intercept, and R-squared
  slope <- round(lm_summary$coefficients[2, 1],2)
  intercept <- round(lm_summary$coefficients[1, 1], 2)
  r_squared <- round(lm_summary$r.squared, 2)
  
  # Append the results to the results data frame
  results_df <- rbind(results_df, data.frame(
    group = group_name,
    slope = slope,
    intercept = intercept,
    r_squared = r_squared
  ))
}

# Print the results table
print(results_df)

#CFDD and Duration # Loop over each lake and extract the slope, intercept, and R-squared
for (group_name in unique(ice_join$lake)) {
  # Fit the linear model for the current group
  lm_model <- lm(glahf_mean_duration ~ max_CFDD, data = ice_join[ice_join$lake == group_name, ])
  
  # Get the summary of the linear model
  lm_summary <- summary(lm_model)
  
  # Extract the slope (coefficient for x), intercept, and R-squared
  slope <- round(lm_summary$coefficients[2, 1],2)
  intercept <- round(lm_summary$coefficients[1, 1], 2)
  r_squared <- round(lm_summary$r.squared, 2)
  
  # Append the results to the results data frame
  results_df <- rbind(results_df, data.frame(
    group = group_name,
    slope = slope,
    intercept = intercept,
    r_squared = r_squared
  ))
}

#NMDD and cover # Loop over each lake and extract the slope, intercept, and R-squared
for (group_name in unique(ice_join$lake)) {
  # Fit the linear model for the current group
  lm_model <- lm(glahf_mean_cover ~ min_NMDD, data = ice_join[ice_join$lake == group_name, ])
  
  # Get the summary of the linear model
  lm_summary <- summary(lm_model)
  
  # Extract the slope (coefficient for x), intercept, and R-squared
  slope <- round(lm_summary$coefficients[2, 1],2)
  intercept <- round(lm_summary$coefficients[1, 1], 2)
  r_squared <- round(lm_summary$r.squared, 2)
  
  # Append the results to the results data frame
  results_df <- rbind(results_df, data.frame(
    group = group_name,
    slope = slope,
    intercept = intercept,
    r_squared = r_squared
  ))
}

#NMDD and Duration # Loop over each lake and extract the slope, intercept, and R-squared
for (group_name in unique(ice_join$lake)) {
  # Fit the linear model for the current group
  lm_model <- lm(glahf_mean_duration ~ min_NMDD, data = ice_join[ice_join$lake == group_name, ])
  
  # Get the summary of the linear model
  lm_summary <- summary(lm_model)
  
  # Extract the slope (coefficient for x), intercept, and R-squared
  slope <- round(lm_summary$coefficients[2, 1],2)
  intercept <- round(lm_summary$coefficients[1, 1], 2)
  r_squared <- round(lm_summary$r.squared, 2)
  
  # Append the results to the results data frame
  results_df <- rbind(results_df, data.frame(
    group = group_name,
    slope = slope,
    intercept = intercept,
    r_squared = r_squared
  ))
}

