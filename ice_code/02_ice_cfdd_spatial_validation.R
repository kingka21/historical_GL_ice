library(terra)
library(ggplot2)
library(dplyr)

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

###############################################################################
#### TEST: variability should be small between layers with similar CFDDs   ####  
###############################################################################


#read in all of the ice layers downloaded from GLAHF 
# list all tif files in the folder
tif_files <- list.files("C:/Users/kingk42/OneDrive - State of Michigan DTMB/Desktop/Ice/ice tiff files/", pattern = "\\.tif$", full.names = TRUE)

# read them all into a SpatRaster collection
rasters <- lapply(tif_files, rast)

#* Erie ####
#crop to a lake extent 
erie_extent = rast("C:/Users/kingk42/OneDrive - State of Michigan DTMB/Desktop/Ice/ice tiff files/ice_test/erie_test_years.tif")

erie_rasters <- lapply(rasters, crop, y = erie_extent)

ref <- erie_rasters[[1]]
rasters_resampled <- lapply(erie_rasters, function(x) {
  resample(x, ref, method = "bilinear")
})

plot(rasters_resampled[[3]])

erie_ice_ras<-rast(rasters_resampled)
plot(erie_ice_ras[[3]])


##get pairwise differences and absolute values and sum 
pairs <- combn(nlyr(erie_ice_ras), 2, simplify = FALSE) #pull out all pairs

results <- lapply(pairs, function(p) {
  r1 <- erie_ice_ras[[p[1]]]
  r2 <- erie_ice_ras[[p[2]]]
  
  # absolute difference
  diff_abs <- abs(r1 - r2)
  
  # sum of all cells (na.rm = TRUE)
  sum_val <- global(diff_abs, fun = "sum", na.rm = TRUE)[1, 1]
  
  # return a tidy row
  data.frame(
    layer1 = names(erie_ice_ras)[p[1]],
    layer2 = names(erie_ice_ras)[p[2]],
    abs_diff_sum = sum_val
  )
})

# bind all rows together
results_df <- bind_rows(results)

#add in CFDD info
erie_pairwise_df<-results_df %>% 
  mutate(year1 = as.numeric(str_extract(layer1, "\\d{4}")), 
         year2 = as.numeric(str_extract(layer2, "\\d{4}")), 
         lake = 'erie'                 
  ) %>% 
  left_join(dd_estimates, by = c("year1" = "year", "lake")) %>% 
  left_join(dd_estimates, by = c("year2" = "year", "lake")) %>% 
  mutate(cfdd_diff = abs(max_CFDD.x - max_CFDD.y))

plot( erie_pairwise_df$cfdd_diff, erie_pairwise_df$abs_diff_sum)

erie<-ggplot(erie_pairwise_df, aes(cfdd_diff, abs_diff_sum)) +
  geom_point(color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  scale_y_continuous(labels = label_scientific()) +
  theme_minimal() +
  theme( 
    axis.text = element_text(size = 12), # affects both axes 
    axis.title.x = element_blank(),
    axis.title.y = element_blank()) +
  theme(plot.margin = margin(10, 10, 25, 25))


#* Huron ####
#crop to a lake extent 
huron_extent = rast("C:/Users/kingk42/OneDrive - State of Michigan DTMB/Desktop/Ice/ice tiff files/ice_test/huron_contemp_test.tif")

huron_rasters <- lapply(rasters, crop, y = huron_extent)

ref <- huron_rasters[[1]]

rasters_resampled <- lapply(huron_rasters, function(x) {
  resample(x, ref, method = "bilinear")
})

plot(rasters_resampled[[3]])

hur_ice_ras<-rast(rasters_resampled)
plot(hur_ice_ras[[3]])

##get pairwise differences and absolute values and sum 
pairs <- combn(nlyr(hur_ice_ras), 2, simplify = FALSE)

results <- lapply(pairs, function(p) {
  r1 <- hur_ice_ras[[p[1]]]
  r2 <- hur_ice_ras[[p[2]]]
  
  # absolute difference
  diff_abs <- abs(r1 - r2)
  
  # sum of all cells (na.rm = TRUE)
  sum_val <- global(diff_abs, fun = "sum", na.rm = TRUE)[1, 1]
  
  # return a tidy row
  data.frame(
    layer1 = names(hur_ice_ras)[p[1]],
    layer2 = names(hur_ice_ras)[p[2]],
    abs_diff_sum = sum_val
  )
})

# bind all rows together
results_df <- bind_rows(results)

#add in CFDD info
huron_pairwise_df<-results_df %>% 
  mutate(year1 = as.numeric(str_extract(layer1, "\\d{4}")), 
         year2 = as.numeric(str_extract(layer2, "\\d{4}")), 
         lake = 'huron'                 
  ) %>% 
  left_join(dd_estimates, by = c("year1" = "year", "lake")) %>% 
  left_join(dd_estimates, by = c("year2" = "year", "lake")) %>% 
  mutate(cfdd_diff = abs(max_CFDD.x - max_CFDD.y))

plot( huron_pairwise_df$cfdd_diff, huron_pairwise_df$abs_diff_sum)

huron<-ggplot(huron_pairwise_df, aes(cfdd_diff, abs_diff_sum))  +
  geom_point(color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  scale_y_continuous(labels = label_scientific()) +
  
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12), # affects both axes 
    axis.title.x = element_blank(),
    axis.title.y = element_blank()) +
  theme(plot.margin = margin(10, 10, 25, 25))



#* Michigan ####
#crop to a lake extent 
mich_extent = rast("C:/Users/kingk42/OneDrive - State of Michigan DTMB/Desktop/Ice/ice tiff files/ice_test/mich_mean_test_years.tif")

mich_rasters <- lapply(rasters, crop, y = mich_extent)

ref <- mich_rasters[[1]]

rasters_resampled <- lapply(mich_rasters, function(x) {
  resample(x, ref, method = "bilinear")
})

plot(rasters_resampled[[3]])

mich_ice_ras<-rast(rasters_resampled)
plot(mich_ice_ras[[3]])

##get pairwise differences and absolute values and sum 
pairs <- combn(nlyr(mich_ice_ras), 2, simplify = FALSE)

results <- lapply(pairs, function(p) {
  r1 <- mich_ice_ras[[p[1]]]
  r2 <- mich_ice_ras[[p[2]]]
  
  # absolute difference
  diff_abs <- abs(r1 - r2)
  
  # sum of all cells (na.rm = TRUE)
  sum_val <- global(diff_abs, fun = "sum", na.rm = TRUE)[1, 1]
  
  # return a tidy row
  data.frame(
    layer1 = names(mich_ice_ras)[p[1]],
    layer2 = names(mich_ice_ras)[p[2]],
    abs_diff_sum = sum_val
  )
})

# bind all rows together
results_df <- bind_rows(results)

#add in CFDD info
michigan_pairwise_df<-results_df %>% 
  mutate(year1 = as.numeric(str_extract(layer1, "\\d{4}")), 
         year2 = as.numeric(str_extract(layer2, "\\d{4}")), 
         lake = 'michigan'                 
  ) %>% 
  left_join(dd_estimates, by = c("year1" = "year", "lake")) %>% 
  left_join(dd_estimates, by = c("year2" = "year", "lake")) %>% 
  mutate(cfdd_diff = abs(max_CFDD.x - max_CFDD.y))

plot( michigan_pairwise_df$cfdd_diff, michigan_pairwise_df$abs_diff_sum)

michigan<-ggplot(michigan_pairwise_df, aes(cfdd_diff, abs_diff_sum)) +
  geom_point(color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  scale_y_continuous(labels = label_scientific()) +
  theme_minimal() +
  theme( 
    axis.text = element_text(size = 12), # affects both axes 
    axis.title.x = element_blank(),
    axis.title.y = element_blank()) +
  theme(plot.margin = margin(10, 10, 25, 25))



#* Onatrio ####
#crop to a lake extent 
ont_extent = rast("C:/Users/kingk42/OneDrive - State of Michigan DTMB/Desktop/Ice/ice tiff files/ice_test/ont_test_years.tif")

ont_rasters <- lapply(rasters, crop, y = ont_extent)

ref <- ont_rasters[[1]]

rasters_resampled <- lapply(ont_rasters, function(x) {
  resample(x, ref, method = "bilinear")
})

plot(rasters_resampled[[3]])

ont_ice_ras<-rast(rasters_resampled)
plot(ont_ice_ras[[3]])

##get pairwise differences and absolute values and sum 
pairs <- combn(nlyr(ont_ice_ras), 2, simplify = FALSE)

results <- lapply(pairs, function(p) {
  r1 <- ont_ice_ras[[p[1]]]
  r2 <- ont_ice_ras[[p[2]]]
  
  # absolute difference
  diff_abs <- abs(r1 - r2)
  
  # sum of all cells (na.rm = TRUE)
  sum_val <- global(diff_abs, fun = "sum", na.rm = TRUE)[1, 1]
  
  # return a tidy row
  data.frame(
    layer1 = names(ont_ice_ras)[p[1]],
    layer2 = names(ont_ice_ras)[p[2]],
    abs_diff_sum = sum_val
  )
})

# bind all rows together
results_df <- bind_rows(results)

#add in CFDD info
ontario_pairwise_df<-results_df %>% 
  mutate(year1 = as.numeric(str_extract(layer1, "\\d{4}")), 
         year2 = as.numeric(str_extract(layer2, "\\d{4}")), 
         lake = 'ontario'                 
  ) %>% 
  left_join(dd_estimates, by = c("year1" = "year", "lake")) %>% 
  left_join(dd_estimates, by = c("year2" = "year", "lake")) %>% 
  mutate(cfdd_diff = abs(max_CFDD.x - max_CFDD.y))

plot( ontario_pairwise_df$cfdd_diff, ontario_pairwise_df$abs_diff_sum)

ontario<-ggplot(ontario_pairwise_df, aes(cfdd_diff, abs_diff_sum))  +
  geom_point(color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  scale_y_continuous(labels = label_scientific()) +
  theme_minimal() +
  theme( 
    axis.text = element_text(size = 12), # affects both axes 
    axis.title.x = element_blank(),
    axis.title.y = element_blank()) +
  theme(plot.margin = margin(10, 10, 25, 25))



#* Superior ####
#crop to a lake extent 
sup_extent = rast("C:/Users/kingk42/OneDrive - State of Michigan DTMB/Desktop/Ice/ice tiff files/ice_test/sup_test_years.tif")

sup_rasters <- lapply(rasters, crop, y = sup_extent)

ref <- sup_rasters[[1]]

rasters_resampled <- lapply(sup_rasters, function(x) {
  resample(x, ref, method = "bilinear")
})

plot(rasters_resampled[[3]])

sup_ice_ras<-rast(rasters_resampled)
plot(sup_ice_ras[[3]])

##get pairwise differences and absolute values and sum 
pairs <- combn(nlyr(sup_ice_ras), 2, simplify = FALSE)

results <- lapply(pairs, function(p) {
  r1 <- sup_ice_ras[[p[1]]]
  r2 <- sup_ice_ras[[p[2]]]
  
  # absolute difference
  diff_abs <- abs(r1 - r2)
  
  # sum of all cells (na.rm = TRUE)
  sum_val <- global(diff_abs, fun = "sum", na.rm = TRUE)[1, 1]
  
  # return a tidy row
  data.frame(
    layer1 = names(sup_ice_ras)[p[1]],
    layer2 = names(sup_ice_ras)[p[2]],
    abs_diff_sum = sum_val
  )
})

# bind all rows together
results_df <- bind_rows(results)

#add in CFDD info
superior_pairwise_df<-results_df %>% 
  mutate(year1 = as.numeric(str_extract(layer1, "\\d{4}")), 
         year2 = as.numeric(str_extract(layer2, "\\d{4}")), 
         lake = 'superior'                 
  ) %>% 
  left_join(dd_estimates, by = c("year1" = "year", "lake")) %>% 
  left_join(dd_estimates, by = c("year2" = "year", "lake")) %>% 
  mutate(cfdd_diff = abs(max_CFDD.x - max_CFDD.y))

plot( superior_pairwise_df$cfdd_diff, superior_pairwise_df$abs_diff_sum)

superior<-ggplot(superior_pairwise_df, aes(cfdd_diff, abs_diff_sum))  +
  geom_point(color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  scale_y_continuous(labels = label_scientific()) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12), # affects both axes 
    axis.title.x = element_blank(),
    axis.title.y = element_blank()) + 
  theme(plot.margin = margin(10, 10, 25, 25))

#* join all plots #### 


# Combine with shared axis labels
combined_pairwise_diff<-plot_grid(
  erie, huron, michigan,
  ontario, superior,
  labels = "AUTO",
  label_x = 0.35,          # move labels horizontally (0 = far left, 1 = far right)
  
  ncol = 3, align = "hv", axis = "tblr"
) +
  draw_label("Difference in CFDD (\u00B0C\u00B7day)", x = 0.5, y = 0.02, vjust = 0, size = 12) +
  draw_label("Sum of the absolute difference in ice duration (days)", x = 0.02, y = 0.5, angle = 90, vjust = 1, size = 12) + 
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggsave(
  filename = "ice_figures/combined_pairwise_diff.png",  # or .pdf, .tiff, etc.
  plot = combined_pairwise_diff,
  width = 10,   # inches
  height = 5,   # inches
  dpi = 300     
)