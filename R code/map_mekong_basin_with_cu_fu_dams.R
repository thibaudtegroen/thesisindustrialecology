# Thibaud te Groen 

# March 2023


library(plotly)
library(tmap)
library(magrittr)
library(purrr)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(dplyr)
library(foreach)
library(rfishbase)
library(ggplot2)
library(grid)
library(gridExtra)
library(ggpubr)
library(tmaptools)
library(reshape2)
library(maps)
library("readxl")

#install.packages("xlsx")
#install.packages("reshape2")
#install.packages("maps")

#source to functions file
source("C:/Industrial Ecology/Jaar 3/Masterscriptie/Databases/R-script/functions_thibaud_te_groen_thesis_1.R")
#source("C:/Industrial Ecology/Jaar 3/Masterscriptie/Databases/R-script/validate_fishbase_names.R")

#** load data **
#Global Nutrient Values from Fishbase Nutrient Tool (FNAT)
nutr <- read.csv("fish_protein_github.csv")

#validated IUCN fishbase species names from the Mekong basin from Keijzer et al. (2022)
tam_species <- read.csv("Mekong_fbnames_IUCNhybas.csv")

#Fishbase matched species names with FNAT
match_nutr <- read.csv("match_nutr.csv")

# Dataset from Keijzer et al. (2022) with HPD scenarios
mekong_ID <- read.csv("mekong_DI_filtered.csv") 

#Hydropower Dams from HydroBASINS (Lehrer & Grill, 2013) from hydrobasin
hb <-read_sf('C:/Industrial Ecology/Jaar 3/Masterscriptie/Databases/Fishbase/hydrobasin_level_12/hybas_lake_as_lev12_v1c.shp')

#**Create main dataframes for analysis**

#Harmonise dataframes 
sp_nutr <- data_cleaning(match_nutr, nutr, tam_species, mekong_ID)

#current scenario CV-values sf dataframe with HydroBASIN geometry
sp_cur_nutr_cv_NA <- create_cur_cv_hb_df (sp_nutr) %>%
  filter(MAIN_BAS != '4120024890') %>% #excluded matched subbasin which is not part of Mekong Basin
  st_as_sf()

#future scenario CV-values sf dataframe with HYDROBASIN geometry
sp_fut_nutr_cv_NA <- create_fut_cv_hb_df (sp_nutr) %>%
  filter(MAIN_BAS != '4120024890') %>% #excluded matched subbasin which is not part of Mekong Basin
  st_as_sf()

#current status CV-values dataframe without geometry
df_sp_fut_nutr_cv_NA <- data.frame(sp_fut_nutr_cv_NA)

#future status CV-values dataframe without geometry
df_sp_cur_nutr_cv_NA <- data.frame(sp_cur_nutr_cv_NA)


tmap_options_reset() #reset tmap options when you have ran other files before
sf::sf_use_s2(FALSE) #spherical geometry switched off
#ttm() #turn on to create interactive plots


#Figure 1: Map of Mekong River Basin with current and future dams ----------------------------------------------------------------

#** Import and create sf layers**
#import shp layer global major rivers
major_rivers <- read_sf('C:/Industrial Ecology/Jaar 3/Masterscriptie/Databases/Dams/majorrivers_0_0/MajorRivers.shp') 

#filter for Mekong river  
mekong_river <- dplyr::filter(major_rivers, NAME %in% c("Mekong"))

#dam locations in current HPD scenario

#GOODD dataset (Mulligan et al., 2020)
GOOD_dams <-read_sf('C:/Industrial Ecology/Jaar 3/Masterscriptie/Databases/Dams/GOOD/GOOD2_unsnapped/GOOD2_unsnapped.shp')
GOOD_hb <- st_filter(GOOD_dams, sp_cur_nutr_cv_NA) %>%
  st_set_crs(st_crs(sp_cur_nutr_cv_NA)) # set the crs to crs of HydroBASINS

GOOD_hb$Dam <- "Current dam"

#GRAND dataset (Lehner et al., 2011)
GRAND_dams <-read_sf('C:/Industrial Ecology/Jaar 3/Masterscriptie/Databases/Dams/GRAND/GRanD_Version_1_3/GRanD_dams_v1_3.shp')
GRAND_hb <- st_filter(GRAND_dams, sp_cur_nutr_cv_NA) %>%
  st_set_crs(st_crs(sp_cur_nutr_cv_NA)) # set the crs to crs of HydroBASINS

#import locations of future HPD scenarios from fhred (Zarfl et al., 2012)
fhred <- read_excel("C:/Industrial Ecology/Jaar 3/Masterscriptie/Databases/Dams/FHReD/FHReD_2015_future_dams_Zarfl_et_al_beta_version/FHReD_2015_future_dams_Zarfl_et_al_beta_version.xlsx",2)

#create sf file from excel file based on Lon and Lat of dam locations in fhred
fhred_locations <- st_as_sf(fhred, coords = c("Lon_Cleaned","LAT_cleaned")) %>%
  st_set_crs(st_crs(sp_cur_nutr_cv_NA)) # set the crs to crs of HydroBASINS

#filter for future HPD locations in the Mekong basin
fhred_hb <- st_filter(fhred_locations, sp_cur_nutr_cv_NA)


#import capital cities from library "MAPS" and filter for  for countries located in the MB
cities <- subset(world.cities, country.etc %in% c("Laos", "Vietnam", "Cambodia", "China", "Thailand", "Myanmar") & 
                   capital == 1)  %>% #turn it into an sf object
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  st_cast("POINT")


#import LMB, UMB and country sf layers
LMB <- read_sf('C:/Industrial Ecology/Jaar 3/Masterscriptie/Databases/Dams/Archive/Lower basin.shp')
UMB <- read_sf('C:/Industrial Ecology/Jaar 3/Masterscriptie/Databases/Dams/Archive/Upper basin.shp')
countries <- read_sf('C:/Industrial Ecology/Jaar 3/Masterscriptie/Databases/Dams/Archive/Relev_countries.shp')
#add country names to sf dataframe 
countries$name <- c("Bangladesh", "Nepal", "Cambodia", "China", "India", "Lao PDR", "Myanmar", "Thailand", "Vietnam" )

#** Create Figure 1**

#plt layer locations GOODD
plt_cu_gooddams <- tm_shape(GOOD_hb) + 
  tm_bubbles(size = 0.15, col = "black") 
#plt layer locations GRAND
plt_cu_granddams <- tm_shape(GRAND_hb) + 
  tm_bubbles(size = 0.15, col = "black") 

#combined plt layer of GRAND and GOODD locations = current scenarios HPD locations
plt_cu_dams <- plt_cu_gooddams + plt_cu_granddams 

#plt layer future HPD locations fhred
plt_fu_dams <- tm_shape(fhred_hb) +
  tm_bubbles(size = 0.15, col="red")

#create figure 1: Mekong river basin
plt_MB_dams<- 
  #countries as main layers and set the bbox boundaries of the plot
  tm_shape(countries, bbox = tmaptools::bb(xlim=c(90, 110), ylim=c(7,35), relative = F)) + tm_fill(col= "lightgrey") +
  
  #LMB
  tm_shape(LMB) + tm_fill(col = "lightgreen") +
  
  #UMB                       
  tm_shape(UMB) + tm_fill(col = "lightyellow") + 
  
  #Mekong river
  tm_shape(mekong_river) + tm_lines(lwd = 2, col = "blue") + 
  
  #add text mekong river
  #tm_text("NAME", col = "blue", along.lines = T, ymod = 43, xmod = -18, size = 1.5)+
  
  #current and future HPD locations
  plt_cu_dams + plt_fu_dams +
  
  #border of countries
  tm_shape(countries) + tm_borders(alpha = 0.5) +
  
  #background layer (ocean and sea)
  tmap_options(bg.color = "lightblue")  +
  
  #capital cities
  tm_shape(cities) + tm_dots(size= 1) + tm_text("name", just = "top", size = 1.5) +
  
  #north and scale of the map
  tm_compass() + tm_scale_bar()  

#save figure 1
#tmap_save(tm = plt_MB_dams, 
         # filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/plt_MB_dams.png", 
         # device = png,
         # height = 20,
         # width = 15)
