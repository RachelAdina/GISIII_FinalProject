# GISIII_FinalProject  
## U.S. Domestic Migration Patterns
#### Rachel Steiner-Dillon, June 5, 2020  

This repository contains the data and code for my GIS III final project.  

## Overview  
Despite the highly politicized focus on international migration, domestic migration patterns can have a far greater impact on the distribution of people and resources throughout a country.  

This project visualizes domestic migration patterns using county-level inflows and outflows data from the Statistics of Income Division of the U.S. Internal Revenue Service. It merges the IRS data for given years of interest with county shapefiles downloaded from the U.S. Census Bureau API, and produces maps showing net migration and origin-destination flows. An interactive app developed using the Shiny package allows the user to explore patterns by year.   

## Goals & Objectives  
The goal of this project is to efficiently generate a range of useful visualizations that explore U.S. domestic migration patterns, based on household migration data produced annually by the IRS.  

My hope was to integrate existing data in a way that provides insights into how migration patterns change across the country from year to year.   

## Data Description  
I am relying on two R packages to access U.S. boundary and attribute data. The first is tidycensus (Walker, 2020), which loads U.S. Census TIGER Shapefiles in .shp format through the Census API. The second is spData (Bivand et al., 2020), which includes a us_states shapefile with attributes that can be used as a crosswalk for merging tables with different identification keys.  

I am relying on IRS SOI Tax Statistics for information about migrant households. Migration data is divided by year, geographic level(state or county), and direction of migrants (inflows or outflows). I have downloaded .csv files of county-level inflows and outflows for each of the years 2014 to 2018.  

### Data Sources
*Kyle Walker (2020). tidycensus: Load US Census Boundary and Attribute Data as 'tidyverse' and 'sf'-Ready Data Frames. R package version 0.9.6. https://CRAN.R-project.org/package=tidycensus*  

*Roger Bivand, Jakub Nowosad and Robin Lovelace (2020). spData: Datasets for Spatial Analysis. R package version 0.3.5. https://CRAN.R-project.org/package=spData*  

*U.S. Census Bureau (2019). 2014-2018 American Community Survey 5-year
estimates. Retrieved from U.S. Census API.*  

*U.S. Internal Revenue Service (2019). 2011-2018 U.S. Population Migration
Data. Retrieved from irs.gov/statistics/soi-tax-stats-migration-data.*  

## Figures  
Sample net migration map:  
![state 2018](https://github.com/RachelAdina/GISIII_FinalProject/blob/master/net_state.png)

Sample origin-destination flow map:  
![state 2018](https://github.com/RachelAdina/GISIII_FinalProject/blob/master/od_state.png)  

## Future Work
In the future, I would like to update the Shiny app to make the flow maps interactive, and include separate sliders for the county, state, and regional levels to improve rendering time. It might also be useful for the app to print summary tables for additional data exploration.  
