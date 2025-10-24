####validation of rasters ### 
library(tidyverse)# for cleaning
library(terra) #for rasters
library(sf) #for projections 
library(ggplot2)#for plotting
library(cowplot) #for plotting
library(scales) #for scientific notation #for rounding legend 
library(RColorBrewer) #for colors
library(ggspatial) #for scale bars and north arrows
library(maps) #for inset
library(spdep) #for Moran

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

ice_join<-left_join(spatial_means, dd_estimates ) #join by year and lake

#* calculate the mean and SD and subset years within % of SD  ##### 
contemp_lake_mean<-ice_join %>%
  group_by(lake) %>% 
  summarise(contemp_mean = mean(max_CFDD),
            SD = sd(max_CFDD)) %>% 
  mutate(high = contemp_mean + (0.3*SD), #% of the SD
         low = contemp_mean - (0.3*SD) ) 


contemp_ice_selection<-left_join(ice_join, contemp_lake_mean, by = "lake") %>% 
  mutate(include = ifelse(max_CFDD < high & max_CFDD > low, "YES", "NO")) # if the max cfdd falls within the SD range


#### TEST: SPATIAL TEST compare the subset of years to all of the years in the contemp period  ####  

#* read in all of the lake rasters ####

erie_contemp_all = rast("C:/Users/kingk42/OneDrive - State of Michigan DTMB/Desktop/Ice/ice tiff files/ice_test/erie_mean_1979_2014.tif")
huron_contemp_all = rast("C:/Users/kingk42/OneDrive - State of Michigan DTMB/Desktop/Ice/ice tiff files/ice_test/huron_mean_1979_2014.tif")
mich_contemp_all = rast("C:/Users/kingk42/OneDrive - State of Michigan DTMB/Desktop/Ice/ice tiff files/ice_test/mich_mean_1979_2014.tif")
ont_contemp_all = rast("C:/Users/kingk42/OneDrive - State of Michigan DTMB/Desktop/Ice/ice tiff files/ice_test/ont_mean_1979_2014.tif")
sup_contemp_all = rast("C:/Users/kingk42/OneDrive - State of Michigan DTMB/Desktop/Ice/ice tiff files/ice_test/sup_mean_1979_2014.tif")

plot(erie_contemp_all)
plot(huron_contemp_all)
plot(mich_contemp_all)
plot(ont_contemp_all)
plot(sup_contemp_all)

#Read in all of the yearly ice rasters
# list all tif files in the folder
tif_files <- list.files("C:/Users/kingk42/OneDrive - State of Michigan DTMB/Desktop/Ice/ice tiff files/", pattern = "\\.tif$", full.names = TRUE)

# read them all into a SpatRaster collection
rasters <- lapply(tif_files, rast)

#crop erie rasters
erie_rasters <- lapply(rasters, crop, y = erie_contemp_all)
ref <- erie_rasters[[1]]
rasters_resampled <- lapply(erie_rasters, function(x) { #this is just a list
  resample(x, ref, method = "bilinear")
})
erie_ice_ras<-rast(rasters_resampled)
plot(erie_ice_ras[[3]])

## pull rasters within 30%
r_subset <- erie_ice_ras[[c(10,12,22,23,26,27,32)]]
erie_sub30<-mean(r_subset)
plot(erie_sub30)

#huron rasters
huron_rasters <- lapply(rasters, crop, y = huron_contemp_all)
ref <- huron_rasters[[1]]
rasters_resampled <- lapply(huron_rasters, function(x) {
  resample(x, ref, method = "bilinear")
})
hur_ice_ras<-rast(rasters_resampled)
plot(hur_ice_ras[[3]])

#### pull rasters within 30% 
r_subset <- hur_ice_ras[[c(2, 7,10, 11,14,15,19,21,26,30)]]
huron_sub30<-mean(r_subset)
plot(huron_sub30)

#michigan rasters
michigan_rasters <- lapply(rasters, crop, y = mich_contemp_all)
ref <- michigan_rasters[[1]]
rasters_resampled <- lapply(michigan_rasters, function(x) {
  resample(x, ref, method = "bilinear")
})
mich_ice_ras<-rast(rasters_resampled)
plot(mich_ice_ras[[3]])

## Need rasters 30% 
r_subset <- mich_ice_ras[[c(2,3,7,10,11,12,15,19,26,27,33)]]
michigan_sub30<-mean(r_subset)
plot(michigan_sub30)

#Ontario rasters
ont_rasters <- lapply(rasters, crop, y = ont_contemp_all)
ref <- ont_rasters[[1]]
rasters_resampled <- lapply(ont_rasters, function(x) {
  resample(x, ref, method = "bilinear")
})
ont_ice_ras<-rast(rasters_resampled)
plot(ont_ice_ras[[3]])

## Need to pull out rasters 30% SD
r_subset <- ont_ice_ras[[c(7,10,11,14,23,26,29,31)]]
ont_sub30<-mean(r_subset)
plot(ont_sub30)

#Superior 
sup_rasters <- lapply(rasters, crop, y = sup_contemp_all)
ref <- sup_rasters[[1]]
rasters_resampled <- lapply(sup_rasters, function(x) {
  resample(x, ref, method = "bilinear")
})
sup_ice_ras<-rast(rasters_resampled)
plot(sup_ice_ras[[3]])

#### Need 30% SD rasters
r_subset <- sup_ice_ras[[c(2,3,7,10,12,13,15,27,33)]]
sup_sub30<-mean(r_subset)
plot(sup_sub30)


#* calculate the difference between the two rasters ####

### ERIE ###
erie_diff_diff = abs(erie_contemp_all - erie_sub30)
range(erie_diff_diff)
plot(erie_diff_diff, main = "Difference")

### HURON ###
huron_diff_diff = abs(huron_contemp_all - huron_sub30)
range(huron_diff_diff)
plot(huron_diff_diff, main = "Difference")

### MICH ###
mich_diff_diff = abs(mich_contemp_all - michigan_sub30)
round(range(mich_diff_diff), 3)
plot(mich_diff_diff, main = "Difference")

### ont ###
ont_diff_diff = abs(ont_contemp_all - ont_sub30)
round(range(ont_diff_diff), 3)
plot(ont_diff_diff, main = "Difference")

### sup ###
sup_diff_diff = abs(sup_contemp_all - sup_sub30)
range(sup_diff_diff)
plot(sup_diff_diff, main = "Difference")

#* plot all maps ####
purple_pal <- colorRampPalette(c("#f9f0ff", "#4b0082"))(100) 

#convert to dataframe
erie_dif_df<- as.data.frame(erie_diff_diff, xy = TRUE)
erie_mor_df<- as.data.frame(erie_diff_moran, xy = TRUE)
hur_dif_df<- as.data.frame(huron_diff_diff, xy = TRUE)
hur_mor_df<- as.data.frame(huron_diff_moran, xy = TRUE)
mich_dif_df<- as.data.frame(mich_diff_diff, xy = TRUE)
mich_mor_df<- as.data.frame(mich_diff_moran, xy = TRUE)
ont_dif_df<- as.data.frame(ont_diff_diff, xy = TRUE)
ont_mor_df<- as.data.frame(ont_diff_moran, xy = TRUE)
sup_dif_df<- as.data.frame(sup_diff_diff, xy = TRUE)
sup_mor_df<- as.data.frame(sup_diff_moran, xy = TRUE)

#* ERIE ####
erie_dif_geo <- project(erie_diff_diff, "EPSG:4326")  # change to lon/lat
erie_geo_df<- as.data.frame(erie_dif_geo, xy = TRUE)
ggplot(erie_geo_df) +
  geom_tile(aes(x = x, y = y, fill = erie_mean_1979_2014)) +
  coord_equal() 

(e_dif_plot<-ggplot(erie_dif_df) +
    geom_tile(aes(x = x, y = y, fill = erie_mean_1979_2014)) +
    scale_fill_gradientn(colours = purple_pal) +
    coord_equal() +
  labs(x = NULL, y = NULL) +
  labs(fill = "difference (days)") + 
  theme_bw()+ 
    theme(
    legend.position = c(0.05, 0.97),   # (x, y) in relative coordinates
    legend.justification = c("left", "top"),  # anchor point of legend box
    legend.background = element_rect(fill = alpha("white"), color = NA), 
    legend.key.width = unit(0.6, "cm"),
    legend.key.height = unit(0.4, "cm"),
    legend.title = element_text(size = 12),
    legend.text  = element_text(size = 12),
    legend.direction = "horizontal", 
    axis.text.x  = element_text(size = 12),  # x-axis tick labels
    axis.text.y  = element_text(size = 12),  # y-axis tick labels
  )  + 
    guides(fill = guide_colorbar(title.position = "top")) + # legend title above the bar
    annotation_scale(location = "br", style = "ticks", width_hint = 0.3, text_cex = 1.0)  + 
    annotation_north_arrow(
      location = "br",
      pad_x = unit(0.2, "in"),
      pad_y = unit(0.5, "in"),
      height = unit(0.5, "cm"),   # controls size
      width = unit(0.5, "cm"), 
      style = north_arrow_orienteering(text_size = 5)
    )  +
    scale_x_continuous(
      breaks = c(1100000, 1200000, 1300000, 1400000),
      labels = c("-82.5°W", "-81.5°W", "-80°W", "")
    ) +
    scale_y_continuous(
      breaks = c(2150000, 2200000,2250000, 2300000),
      labels = c("42°N", "42.25°N", "42.5°N", "42.75°N")
    )
) 

ggsave(
  filename = "ice_figures/erie_diff_map.png",  # or .pdf, .tiff, etc.
  plot = e_dif_plot,
  width = 8,   # inches
  height = 5,   # inches
  dpi = 600     
)



#* HURON #####

hur_dif_geo <- project(huron_diff_diff, "EPSG:4326")  # change to lon/lat
hur_geo_df<- as.data.frame(hur_dif_geo, xy = TRUE)
ggplot(hur_geo_df) +
  geom_tile(aes(x = x, y = y, fill = Band_1)) +
  coord_equal() 

(h_dif_plot<-ggplot(hur_dif_df) +
    geom_tile(aes(x = x, y = y, fill = Band_1)) +
    scale_fill_gradientn(colours = purple_pal, labels = label_number(accuracy = 1)) +
    coord_equal() +
    labs(x = NULL, y = NULL) +
    labs(fill = "difference (days)") + 
    theme_bw()+ 
    theme(
      legend.position = c(0.01, 0.01),   # (x, y) in relative coordinates
      legend.justification = c("left", "bottom"),  # anchor point of legend box
      legend.background = element_rect(fill = alpha("white"), color = NA), 
      legend.direction = "horizontal", 
      legend.key.width = unit(0.5, "cm"),
      legend.key.height = unit(0.3, "cm"),
      legend.title = element_text(size = 10)
    )  + 
    guides(fill = guide_colorbar(title.position = "top")) + # legend title above the bar
    annotation_scale(location = "br",  style = "ticks", width_hint = 0.3)  + 
    annotation_north_arrow(
      location = "br",
      pad_x = unit(0.2, "cm"),
      pad_y = unit(1, "cm"),
      height = unit(0.5, "cm"),   # controls size
      width = unit(0.5, "cm"), 
      style = north_arrow_orienteering(text_size = 5)
    ) +
    scale_x_continuous(
      breaks = c(900000, 1000000, 1100000, 1200000),
      labels = c("-84.5°W", "-83.5°W", "-82.5°W", "-81.5°W")
    ) +
    scale_y_continuous(
      breaks = c(2300000, 2400000,2500000, 2600000),
      labels = c("43°N", "44°N", "45°N", "46°N")
    )
)

ggsave(
  filename = "ice_figures/hur_diff_map.png",  # or .pdf, .tiff, etc.
  plot = h_dif_plot,
  width = 5,   # inches
  height = 5,   # inches
  dpi = 300     
)



#* MICHIGAN ####
mich_dif_geo <- project(mich_diff_diff, "EPSG:4326")  # change to lon/lat
mich_geo_df<- as.data.frame(mich_dif_geo, xy = TRUE)
ggplot(mich_geo_df) +
  geom_tile(aes(x = x, y = y, fill = mich_mean_1979_2014)) +
  coord_equal() 

(m_dif_plot<-ggplot(mich_dif_df) +
    geom_tile(aes(x = x, y = y, fill = mich_mean_1979_2014)) +
    scale_fill_gradientn(colours = purple_pal, labels = label_number(accuracy = 1)) +
    coord_equal() +
    labs(x = NULL, y = NULL) +
    labs(fill = "difference (days)") + 
    theme_bw()+ 
    theme(
      legend.position = c(0.99, 0.01),   # (x, y) in relative coordinates
      legend.justification = c("right", "bottom"),  # anchor point of legend box
      legend.background = element_rect(fill = alpha("white"), color = NA), 
      legend.key.width = unit(0.3, "cm"),
      legend.key.height = unit(0.5, "cm"),
      legend.title = element_text(size = 10)
    )  + 
    guides(fill = guide_colorbar(title = "difference\n(days)")) + #wrap title
    annotation_scale(location = "tl",  style = "ticks", width_hint = 0.3)  + 
    annotation_north_arrow(
      location = "tl",
      pad_x = unit(0.2, "cm"),
      pad_y = unit(1, "cm"),
      height = unit(0.5, "cm"),   # controls size
      width = unit(0.5, "cm"), 
      style = north_arrow_orienteering(text_size = 5)
    )+
    scale_x_continuous(
      breaks = c(650000, 800000),
      labels = c("-88°W", "-86°W")
    ) +
    scale_y_continuous(
      breaks = c(2100000, 2200000,2300000, 2400000, 2500000, 2600000),
      labels = c("41.5°N", "42.5°N", "43.5°N", "44.5°N", "45.5°N", "46.5°N")
    )
)

ggsave(
  filename = "ice_figures/mich_diff_map.png",  # or .pdf, .tiff, etc.
  plot = m_dif_plot,
  width = 10,   # inches
  height = 5,   # inches
  dpi = 300     
)




#*ONTARIO ####
ont_dif_geo <- project(ont_diff_diff, "EPSG:4326")  # change to lon/lat
ont_geo_df<- as.data.frame(ont_dif_geo, xy = TRUE)
ggplot(ont_geo_df) +
  geom_tile(aes(x = x, y = y, fill = ont_mean_1979_2014)) +
  coord_equal() 

(o_dif_plot<-ggplot(ont_dif_df) +
    geom_tile(aes(x = x, y = y, fill = ont_mean_1979_2014)) +
    scale_fill_gradientn(colours = purple_pal, 
                         limits = c(0, 15),
                         breaks = seq(0, 15, by = 5), 
                         labels = paste0(seq(0, 15, by = 5)) ) +
    coord_equal() +
    labs(x = NULL, y = NULL) +
    labs(fill = "difference (days)") + 
    theme_bw()+ 
    theme(
      legend.position = c(0.05, 0.97),   # (x, y) in relative coordinates
      legend.justification = c("left", "top"),  # anchor point of legend box
      legend.background = element_rect(fill = alpha("white"), color = NA), 
      legend.key.width = unit(0.6, "cm"),
      legend.key.height = unit(0.4, "cm"),
      legend.title = element_text(size = 12),
      legend.text  = element_text(size = 12),
      legend.direction = "horizontal", 
      axis.text.x  = element_text(size = 12),  # x-axis tick labels
      axis.text.y  = element_text(size = 12),  # y-axis tick labels
    )  + 
    guides(fill = guide_colorbar(title.position = "top")) +  # legend title above the bar
    annotation_scale(location = "br",  style = "ticks", width_hint = 0.3, text_cex = 1.0)  + 
    annotation_north_arrow(
      location = "br",
      pad_x = unit(0.2, "cm"),
      pad_y = unit(1, "cm"),
      height = unit(0.5, "cm"),   # controls size
      width = unit(0.5, "cm"), 
      style = north_arrow_orienteering(text_size = 5)
    )+
    scale_x_continuous(
      breaks = c(1300000, 1400000, 1500000, 1600000),
      labels = c("-80°W", "", "-77°W", "")
    ) +
    scale_y_continuous(
      breaks = c(2350000, 2400000, 2450000, 2500000,2550000),
      labels = c("", "43.5°N", "", "", "44.5°N")
    )
 )

ggsave(
  filename = "ice_figures/ont_diff_map.png",  # or .pdf, .tiff, etc.
  plot = o_dif_plot,
  width = 8,   # inches
  height = 6,   # inches
  dpi = 300     
)



#* SUPERIOR ####
sup_dif_geo <- project(sup_diff_diff, "EPSG:4326")  # change to lon/lat
sup_geo_df<- as.data.frame(sup_dif_geo, xy = TRUE)
ggplot(sup_geo_df) +
  geom_tile(aes(x = x, y = y, fill = sup_mean_1979_2014)) +
  coord_equal() 

(s_dif_plot<-ggplot(sup_dif_df) +
    geom_tile(aes(x = x, y = y, fill = sup_mean_1979_2014)) +
    scale_fill_gradientn(colours = purple_pal ) +
    coord_equal() +
    labs(x = NULL, y = NULL) +
    labs(fill = "difference (days)") + 
    theme_bw()+ 
    theme(
      legend.position = c(0.05, 0.97),   # (x, y) in relative coordinates
      legend.justification = c("left", "top"),  # anchor point of legend box
      legend.background = element_rect(fill = alpha("white"), color = NA), 
      legend.key.width = unit(0.5, "cm"),
      legend.key.height = unit(0.3, "cm"),
      legend.title = element_text(size = 10),
      legend.direction = "horizontal"
    )  + 
    guides(fill = guide_colorbar(title.position = "top")) +  # legend title above the bar
  annotation_scale(location = "tr",  style = "ticks", width_hint = 0.3)  + 
  annotation_north_arrow(
    location = "tr",
    pad_x = unit(0.2, "cm"),
    pad_y = unit(1, "cm"),
    height = unit(0.5, "cm"),   # controls size
    width = unit(0.5, "cm"), 
    style = north_arrow_orienteering(text_size = 5)
  ) +
 scale_x_continuous(
   breaks = c(300000, 400000, 500000, 600000, 700000, 800000, 900000),
   labels = c("-92°W", "", "", "-88°W", "", "", "-84°W")
 ) +
   scale_y_continuous(
     breaks = c(2700000, 2800000, 2900000),
     labels = c("47°N", "48°N", "49°N")
   )
) 

ggsave(
  filename = "ice_figures/sup_diff_map.png",  # or .pdf, .tiff, etc.
  plot = s_dif_plot,
  width = 8,   # inches
  height = 6,   # inches
  dpi = 300     
)



#* Make the inset map ####
us_map <- map_data("state")

red_square <- data.frame(
  xmin = -95,
  xmax = -75,
  ymin = 41,
  ymax = 50
)

inset<-ggplot() +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group),
               fill = "grey80", color = "white") +
  geom_rect(data = red_square, 
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = NA, color = "red", size = 1) +
  theme_void()

ggsave(
  filename = "ice_figures/inset_map.png",  # or .pdf, .tiff, etc.
  plot = inset,
  width = 10,   # inches
  height = 5,   # inches
  dpi = 300     
)


# pseudo p-values  Moran's I, RMSE, and MAD #### 
#https://lanselin.github.io/introbook_vol1/morans-i.html

.
# Root Mean Square Error (RMSE), which quantifies the average of the squared difference between the values of the two rasters.same unit was the variable (i.e. days)
#Mean Absolute Difference (MAD) 
#do a formal test of difference between the two Moran’s I values, simulate it via permutation tests



#* ERIE ####

# --- Step 1: remove NAs ---
sub_stack<-c(erie_contemp_all, erie_sub30)
vals <- values(sub_stack)
non_na <- complete.cases(vals)
vals <- vals[non_na, , drop = FALSE]


# --- Step 2: neighbor structure ---
r0 <- erie_contemp_all[[1]]
nb_full <- cell2nb(nrow(r0), ncol(r0), type = "queen")
lw_full <- nb2listw(nb_full, style = "W", zero.policy = TRUE)

# subset neighbors to non-NA cells
lw_sub <- subset(lw_full, subset = non_na, zero.policy = TRUE)

# drop isolates if any
isolates <- which(card(lw_sub$neighbours) == 0)
if (length(isolates) > 0) {
  cat("Dropping", length(isolates), "isolated cells (no neighbours)\n")
  keep <- rep(TRUE, nrow(vals)); keep[isolates] <- FALSE
  lw_sub <- subset(lw_sub, subset = keep, zero.policy = TRUE)
  vals <- vals[keep, , drop = FALSE]
}

n_zones <- length(lw_sub$neighbours)
S0_val  <- Szero(lw_sub)


# --- Step 3: compute test ---
#permutate based on just one for Moran's, this null model keeps both rasters spatially aligned (ice is inherently spatial),
#but removes their specific spatial structure relative to geography.

vals1 <- vals[,1]  # first layer is first variable
vals2 <- vals[,2]  # second layer is second variable

# Per-cell absolute difference
abs_diffs <- abs(vals1 - vals2)

# Mean Absolute Difference (MAD)
mean_abs_diff <- mean(abs_diffs, na.rm = TRUE)

# Root Mean Squared Error (RMSE)
rmse_rasters <- sqrt(mean((vals1 - vals2)^2, na.rm = TRUE))

#Moran's I 
m1 <- moran(vals1, lw_sub, n_zones, S0_val)$I
m2 <- moran(vals2, lw_sub, n_zones, S0_val)$I
obs_diff <- m1 - m2
obs_diff

n_sims <-999
sim_diffs <- numeric(n_sims)
sim_rmse <- numeric(n_sims)
sim_mad  <- numeric(n_sims)

set.seed(8)

for (i in 1:n_sims) {
  
  # same random reordering
  perm_idx <- sample(seq_along(vals[,1]))  # same random reordering
  perm1 <- vals1[perm_idx]
  perm2 <- vals2[perm_idx]
  mi1 <- moran(perm1, listw = lw_sub, n = n_zones, S0 = S0_val, zero.policy = TRUE)$I
  mi2 <- moran(perm2, listw = lw_sub, n = n_zones, S0 = S0_val, zero.policy = TRUE)$I
  
  sim_diffs[i]  <- mi1 - mi2
  
  # --- RMSE & MAD ---
  # independent permutations
  perm1_ind <- vals1[sample(seq_along(vals1))]
  perm2_ind <- vals2[sample(seq_along(vals2))]
  
  sim_rmse[i] <- sqrt(mean((perm1_ind - perm2_ind)^2, na.rm = TRUE))
  sim_mad[i]  <- mean(abs(perm1_ind - perm2_ind), na.rm = TRUE)
  
}

# --- Compute permutation p-values ---
p_val_moran <- (sum(abs(sim_diffs) >= abs(obs_diff)) + 1) / (n_sims + 1)
p_val_rmse  <- (sum(sim_rmse <= rmse_rasters) + 1) / (n_sims + 1)
p_val_mad   <- (sum(sim_mad  <= mean_abs_diff) + 1) / (n_sims + 1)

cat("Observed Moran's I difference:", obs_diff, "\n")
cat("p-value (Moran’s I difference):", p_val_moran, "\n\n")

cat("Observed RMSE:", rmse_rasters, "\n")
cat("p-value (RMSE, independent perm):", p_val_rmse, "\n\n")

cat("Observed MAD:", mean_abs_diff, "\n")
cat("p-value (MAD, independent perm):", p_val_mad, "\n")

#if pvalue is low <0.05, the observed Moran’s I is so extreme that none of the results computed for simulated data sets exceeds its value, thus spatial autocorrelation differs significantly between the two rasters.
#but if pvalue is high, the observed difference in Moran’s I is likely under the null distribution generated by simulations 
#pvalue for RMSE and MAD - left-tailed test — testing the null hypothesis that the observed similarity is no better than random.
#if p-value is low, then differences are smaller than chance. 

hist(sim_diffs,
     breaks = 30,
     col = "#FFD966",      # soft yellow
     border = "white",
     main = "Moran's I Null Distribution ",
     xlab = "Simulated Moran's I",
     xlim = range(c(sim_diffs, obs_diff)),
     las = 1)
abline(v = obs_diff, col = "red", lwd = 2)
text(obs_diff, par("usr")[4]*0.9,
     labels = paste0("Observed I = ", round(obs_diff, 3)),
     col = "red", adj = -0.1)

hist(sim_rmse, main = "RMSE (independent)", xlab = "RMSE", col = "gray")
abline(v = rmse_rasters, col = "red", lwd = 2)

hist(sim_mad, main = "MAD (independent)", xlab = "MAD", col = "gray")
abline(v = mean_abs_diff, col = "red", lwd = 2)

#* HURON #####

# --- Step 1: remove NAs ---
sub_stack<-c(huron_contemp_all, huron_sub30)
vals <- values(sub_stack)
non_na <- complete.cases(vals)
vals <- vals[non_na, , drop = FALSE]


# --- Step 2: neighbor structure ---
r0 <- huron_contemp_all[[1]]
nb_full <- cell2nb(nrow(r0), ncol(r0), type = "queen")
lw_full <- nb2listw(nb_full, style = "W", zero.policy = TRUE)

# subset neighbors to non-NA cells
lw_sub <- subset(lw_full, subset = non_na, zero.policy = TRUE)

# drop isolates if any
isolates <- which(card(lw_sub$neighbours) == 0)
if (length(isolates) > 0) {
  cat("Dropping", length(isolates), "isolated cells (no neighbours)\n")
  keep <- rep(TRUE, nrow(vals)); keep[isolates] <- FALSE
  lw_sub <- subset(lw_sub, subset = keep, zero.policy = TRUE)
  vals <- vals[keep, , drop = FALSE]
}

n_zones <- length(lw_sub$neighbours)
S0_val  <- Szero(lw_sub)


# --- Step 3: compute test ---

vals1 <- vals[,1]
vals2 <- vals[,2]


# Per-cell absolute difference
abs_diffs <- abs(vals1 - vals2)

# Mean Absolute Difference (MAD)
mean_abs_diff <- mean(abs_diffs, na.rm = TRUE)

# Root Mean Squared Error (RMSE)
rmse_rasters <- sqrt(mean((vals1 - vals2)^2, na.rm = TRUE))

#Moran's I 
m1 <- moran(vals1, lw_sub, n_zones, S0_val)$I
m2 <- moran(vals2, lw_sub, n_zones, S0_val)$I
obs_diff <- m1 - m2
obs_diff

n_sims <-999
sim_diffs <- numeric(n_sims)
sim_rmse <- numeric(n_sims)
sim_mad  <- numeric(n_sims)

set.seed(8)

for (i in 1:n_sims) {
  
  # same random reordering
  perm_idx <- sample(seq_along(vals[,1]))  # same random reordering
  perm1 <- vals1[perm_idx]
  perm2 <- vals2[perm_idx]
  mi1 <- moran(perm1, listw = lw_sub, n = n_zones, S0 = S0_val, zero.policy = TRUE)$I
  mi2 <- moran(perm2, listw = lw_sub, n = n_zones, S0 = S0_val, zero.policy = TRUE)$I
  
  sim_diffs[i]  <- mi1 - mi2
  
  # --- RMSE & MAD ---
  # independent permutations
  perm1_ind <- vals1[sample(seq_along(vals1))]
  perm2_ind <- vals2[sample(seq_along(vals2))]
  
  sim_rmse[i] <- sqrt(mean((perm1_ind - perm2_ind)^2, na.rm = TRUE))
  sim_mad[i]  <- mean(abs(perm1_ind - perm2_ind), na.rm = TRUE)
  
}

# --- Compute permutation p-values ---
p_val_moran <- (sum(abs(sim_diffs) >= abs(obs_diff)) + 1) / (n_sims + 1)
p_val_rmse  <- (sum(sim_rmse <= rmse_rasters) + 1) / (n_sims + 1)
p_val_mad   <- (sum(sim_mad  <= mean_abs_diff) + 1) / (n_sims + 1)

cat("Observed Moran's I difference:", obs_diff, "\n")
cat("p-value (Moran’s I difference):", p_val_moran, "\n\n")

cat("Observed RMSE:", rmse_rasters, "\n")
cat("p-value (RMSE, independent perm):", p_val_rmse, "\n\n")

cat("Observed MAD:", mean_abs_diff, "\n")
cat("p-value (MAD, independent perm):", p_val_mad, "\n")

#* MICHIGAN ####

# --- Step 1: remove NAs ---
sub_stack<-c(mich_contemp_all, michigan_sub30)
vals <- values(sub_stack)
non_na <- complete.cases(vals)
vals <- vals[non_na, , drop = FALSE]


# --- Step 2: neighbor structure ---
r0 <- mich_contemp_all[[1]]
nb_full <- cell2nb(nrow(r0), ncol(r0), type = "queen")
lw_full <- nb2listw(nb_full, style = "W", zero.policy = TRUE)

# subset neighbors to non-NA cells
lw_sub <- subset(lw_full, subset = non_na, zero.policy = TRUE)

# drop isolates if any
isolates <- which(card(lw_sub$neighbours) == 0)
if (length(isolates) > 0) {
  cat("Dropping", length(isolates), "isolated cells (no neighbours)\n")
  keep <- rep(TRUE, nrow(vals)); keep[isolates] <- FALSE
  lw_sub <- subset(lw_sub, subset = keep, zero.policy = TRUE)
  vals <- vals[keep, , drop = FALSE]
}

n_zones <- length(lw_sub$neighbours)
S0_val  <- Szero(lw_sub)


# --- Step 3: compute test ---
vals1 <- vals[,1]
vals2 <- vals[,2]


# Per-cell absolute difference
abs_diffs <- abs(vals1 - vals2)

# Mean Absolute Difference (MAD)
mean_abs_diff <- mean(abs_diffs, na.rm = TRUE)

# Root Mean Squared Error (RMSE)
rmse_rasters <- sqrt(mean((vals1 - vals2)^2, na.rm = TRUE))

#Moran's I 
m1 <- moran(vals1, lw_sub, n_zones, S0_val)$I
m2 <- moran(vals2, lw_sub, n_zones, S0_val)$I
obs_diff <- m1 - m2
obs_diff

n_sims <-999
sim_diffs <- numeric(n_sims)
sim_rmse <- numeric(n_sims)
sim_mad  <- numeric(n_sims)

set.seed(8)

for (i in 1:n_sims) {
  
  # same random reordering
  perm_idx <- sample(seq_along(vals[,1]))  # same random reordering
  perm1 <- vals1[perm_idx]
  perm2 <- vals2[perm_idx]
  mi1 <- moran(perm1, listw = lw_sub, n = n_zones, S0 = S0_val, zero.policy = TRUE)$I
  mi2 <- moran(perm2, listw = lw_sub, n = n_zones, S0 = S0_val, zero.policy = TRUE)$I
  
  sim_diffs[i]  <- mi1 - mi2
  
  # --- RMSE & MAD ---
  # independent permutations
  perm1_ind <- vals1[sample(seq_along(vals1))]
  perm2_ind <- vals2[sample(seq_along(vals2))]
  
  sim_rmse[i] <- sqrt(mean((perm1_ind - perm2_ind)^2, na.rm = TRUE))
  sim_mad[i]  <- mean(abs(perm1_ind - perm2_ind), na.rm = TRUE)
  
}

# --- Compute permutation p-values ---
p_val_moran <- (sum(abs(sim_diffs) >= abs(obs_diff)) + 1) / (n_sims + 1)
p_val_rmse  <- (sum(sim_rmse <= rmse_rasters) + 1) / (n_sims + 1)
p_val_mad   <- (sum(sim_mad  <= mean_abs_diff) + 1) / (n_sims + 1)

cat("Observed Moran's I difference:", obs_diff, "\n")
cat("p-value (Moran’s I difference):", p_val_moran, "\n\n")

cat("Observed RMSE:", rmse_rasters, "\n")
cat("p-value (RMSE, independent perm):", p_val_rmse, "\n\n")

cat("Observed MAD:", mean_abs_diff, "\n")
cat("p-value (MAD, independent perm):", p_val_mad, "\n")

#* ONTARIO ####

# --- Step 1: remove NAs ---
sub_stack<-c(ont_contemp_all, ont_sub30)
vals <- values(sub_stack)
non_na <- complete.cases(vals)
vals <- vals[non_na, , drop = FALSE]


# --- Step 2: neighbor structure ---
r0 <- ont_contemp_all[[1]]
nb_full <- cell2nb(nrow(r0), ncol(r0), type = "queen")
lw_full <- nb2listw(nb_full, style = "W", zero.policy = TRUE)

# subset neighbors to non-NA cells
lw_sub <- subset(lw_full, subset = non_na, zero.policy = TRUE)

# drop isolates if any
isolates <- which(card(lw_sub$neighbours) == 0)
if (length(isolates) > 0) {
  cat("Dropping", length(isolates), "isolated cells (no neighbours)\n")
  keep <- rep(TRUE, nrow(vals)); keep[isolates] <- FALSE
  lw_sub <- subset(lw_sub, subset = keep, zero.policy = TRUE)
  vals <- vals[keep, , drop = FALSE]
}

n_zones <- length(lw_sub$neighbours)
S0_val  <- Szero(lw_sub)


# --- Step 3: compute test ---

vals1 <- vals[,1]
vals2 <- vals[,2]


# Per-cell absolute difference
abs_diffs <- abs(vals1 - vals2)

# Mean Absolute Difference (MAD)
mean_abs_diff <- mean(abs_diffs, na.rm = TRUE)

# Root Mean Squared Error (RMSE)
rmse_rasters <- sqrt(mean((vals1 - vals2)^2, na.rm = TRUE))

#Moran's I 
m1 <- moran(vals1, lw_sub, n_zones, S0_val)$I
m2 <- moran(vals2, lw_sub, n_zones, S0_val)$I
obs_diff <- m1 - m2
obs_diff

n_sims <-999
sim_diffs <- numeric(n_sims)
sim_rmse <- numeric(n_sims)
sim_mad  <- numeric(n_sims)

set.seed(8)

for (i in 1:n_sims) {
  
  # same random reordering
  perm_idx <- sample(seq_along(vals[,1]))  # same random reordering
  perm1 <- vals1[perm_idx]
  perm2 <- vals2[perm_idx]
  mi1 <- moran(perm1, listw = lw_sub, n = n_zones, S0 = S0_val, zero.policy = TRUE)$I
  mi2 <- moran(perm2, listw = lw_sub, n = n_zones, S0 = S0_val, zero.policy = TRUE)$I
  
  sim_diffs[i]  <- mi1 - mi2
  
  # --- RMSE & MAD ---
  # independent permutations
  perm1_ind <- vals1[sample(seq_along(vals1))]
  perm2_ind <- vals2[sample(seq_along(vals2))]
  
  sim_rmse[i] <- sqrt(mean((perm1_ind - perm2_ind)^2, na.rm = TRUE))
  sim_mad[i]  <- mean(abs(perm1_ind - perm2_ind), na.rm = TRUE)
  
}

# --- Compute permutation p-values ---
p_val_moran <- (sum(abs(sim_diffs) >= abs(obs_diff)) + 1) / (n_sims + 1)
p_val_rmse  <- (sum(sim_rmse <= rmse_rasters) + 1) / (n_sims + 1)
p_val_mad   <- (sum(sim_mad  <= mean_abs_diff) + 1) / (n_sims + 1)

cat("Observed Moran's I difference:", obs_diff, "\n")
cat("p-value (Moran’s I difference):", p_val_moran, "\n\n")

cat("Observed RMSE:", rmse_rasters, "\n")
cat("p-value (RMSE, independent perm):", p_val_rmse, "\n\n")

cat("Observed MAD:", mean_abs_diff, "\n")
cat("p-value (MAD, independent perm):", p_val_mad, "\n")

#* SUPERIOR ####

# --- Step 1: remove NAs ---
sub_stack<-c(sup_contemp_all, sup_sub30)
vals <- values(sub_stack)
non_na <- complete.cases(vals)
vals <- vals[non_na, , drop = FALSE]


# --- Step 2: neighbor structure ---
r0 <- sup_contemp_all[[1]]
nb_full <- cell2nb(nrow(r0), ncol(r0), type = "queen")
lw_full <- nb2listw(nb_full, style = "W", zero.policy = TRUE)

# subset neighbors to non-NA cells
lw_sub <- subset(lw_full, subset = non_na, zero.policy = TRUE)

# drop isolates if any
isolates <- which(card(lw_sub$neighbours) == 0)
if (length(isolates) > 0) {
  cat("Dropping", length(isolates), "isolated cells (no neighbours)\n")
  keep <- rep(TRUE, nrow(vals)); keep[isolates] <- FALSE
  lw_sub <- subset(lw_sub, subset = keep, zero.policy = TRUE)
  vals <- vals[keep, , drop = FALSE]
}

n_zones <- length(lw_sub$neighbours)
S0_val  <- Szero(lw_sub)


# --- Step 3: compute test ---

vals1 <- vals[,1]
vals2 <- vals[,2]


# Per-cell absolute difference
abs_diffs <- abs(vals1 - vals2)

# Mean Absolute Difference (MAD)
mean_abs_diff <- mean(abs_diffs, na.rm = TRUE)

# Root Mean Squared Error (RMSE)
rmse_rasters <- sqrt(mean((vals1 - vals2)^2, na.rm = TRUE))

#Moran's I 
m1 <- moran(vals1, lw_sub, n_zones, S0_val)$I
m2 <- moran(vals2, lw_sub, n_zones, S0_val)$I
obs_diff <- m1 - m2
obs_diff

n_sims <-999
sim_diffs <- numeric(n_sims)
sim_rmse <- numeric(n_sims)
sim_mad  <- numeric(n_sims)

set.seed(8)

for (i in 1:n_sims) {
  
  # same random reordering
  perm_idx <- sample(seq_along(vals[,1]))  # same random reordering
  perm1 <- vals1[perm_idx]
  perm2 <- vals2[perm_idx]
  mi1 <- moran(perm1, listw = lw_sub, n = n_zones, S0 = S0_val, zero.policy = TRUE)$I
  mi2 <- moran(perm2, listw = lw_sub, n = n_zones, S0 = S0_val, zero.policy = TRUE)$I
  
  sim_diffs[i]  <- mi1 - mi2
  
  # --- RMSE & MAD ---
  # independent permutations
  perm1_ind <- vals1[sample(seq_along(vals1))]
  perm2_ind <- vals2[sample(seq_along(vals2))]
  
  sim_rmse[i] <- sqrt(mean((perm1_ind - perm2_ind)^2, na.rm = TRUE))
  sim_mad[i]  <- mean(abs(perm1_ind - perm2_ind), na.rm = TRUE)
  
}

# --- Compute permutation p-values ---
p_val_moran <- (sum(abs(sim_diffs) >= abs(obs_diff)) + 1) / (n_sims + 1)
p_val_rmse  <- (sum(sim_rmse <= rmse_rasters) + 1) / (n_sims + 1)
p_val_mad   <- (sum(sim_mad  <= mean_abs_diff) + 1) / (n_sims + 1)

cat("Observed Moran's I difference:", obs_diff, "\n")
cat("p-value (Moran’s I difference):", p_val_moran, "\n\n")

cat("Observed RMSE:", rmse_rasters, "\n")
cat("p-value (RMSE, independent perm):", p_val_rmse, "\n\n")

cat("Observed MAD:", mean_abs_diff, "\n")
cat("p-value (MAD, independent perm):", p_val_mad, "\n")


#### compare mean of the subset of years to the years not used  ####  

# Split data by group, then apply t-test
split_data <- split(contemp_ice_selection, contemp_ice_selection$lake)

results <- lapply(split_data, function(df) {
  t.test(glahf_mean_duration ~ include, data = df)
})

# Extract p-values
sapply(results, function(x) x$p.value)

#A p-value > 0.05 indicates that the observed difference in means is not statistically significant.





