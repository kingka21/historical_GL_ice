# historical_GL_ice

This repository contains the code that supports the manuscript: **Katelyn B.S. King, Ayumi Fujisaki-Manome, Cory Brant, Danielle Cohn, Inigo Peng, Karen Alofs.
Reconstructing Great Lakes air temperature and ice dynamics data back to 1897.**  

The manuscript provides the methods and data for the Great Lakes: daily air temperature, cumulative freezing degree-days, and net melting degree-days from 1897-2023, and raster layers estimating ice duration and variability spatially during the historical period from 1897-1960. 


**The 'ice_code' folder includes the following scripts:** \
**01_ice_summaries_figures.R** includes R code year selection for the historical time period as well as linear regression and figure creation \
**02_ice_cfdd_spatial_validation.R** includes R code for comparing differences in CFDD to differences ice duration across all ice years  \
**03_ice_raster_validation.R** includes R code the MAD, RMSE, and Moran's I validation \
**03_erie_airtemp_bar_and_timeseries.py** includes Python code for air temperature time series plots \



**The 'ice_data' folder includes the following datasets:** \
**air_temp_data** folder that contains the air temperature data used to estimate CFDD and NMDD and make air temp figures \
**raw data** folder that contains the daily CFDD and NMDD estimates \
**spatial_means.csv** which is the yearly means of the ice cover and ice duration rasters  \
**summary_*lake*_DD_estimates.csv** which are summary estimates of CFDD and NMDD created from the raw data. Code for these summaries is in the R code provided. \

**The 'ice_figures' folder includes the figures made in R for the manuscript**
