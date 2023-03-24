# Get nutrient data, calculate and map Coefficient Variation  over LMB

# Thibaud te Groen 

# oktober 2022

#code adapted from Tamara Keijzer IUCN_hybas_fishbase.R 
#Based on validate_fishbase_names.R
library(rfishbase)
options(FISHBASE_VERSION="21.06")

# Import & cleaning Data -----------------------------------------------------------
#set directory

setwd("C:/Industrial Ecology/Jaar 3/Masterscriptie/Databases/Fishbase")

#**Install and Load packages**

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
library(ggforce)
library(ggbreak)
library(maps)
library(reshape2)

tmap_options_reset() #reset tmap options when you have ran other files before
sf::sf_use_s2(FALSE) #spherical geometry switched off
#ttm() #turn on to create interactive plots

#source to functions file
source("C:/Industrial Ecology/Jaar 3/Masterscriptie/Databases/R-script/functions.R")

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

#Harmonise main dataframes 
sp_nutr <- data_cleaning(match_nutr, nutr, tam_species, mekong_ID)


#Preprocess: current status of CV values across subbasins in the Mekong Basin ----------------------------------------------------------------

#current scenario CV-values sf dataframe with HydroBASIN geometry
sp_cur_nutr_cv_NA <- create_cur_cv_hb_df (sp_nutr) %>%
  filter(MAIN_BAS != '4120024890') %>% #excluded matched subbasin which is not part of Mekong Basin
  st_as_sf()

#current status CV-values dataframe without geometry
df_sp_cur_nutr_cv_NA <- data.frame(sp_cur_nutr_cv_NA)

#**Dam and river layers for plot**
#import shp layer global major rivers
major_rivers <- read_sf('C:/Industrial Ecology/Jaar 3/Masterscriptie/Databases/Dams/majorrivers_0_0/MajorRivers.shp') 

#filter for Mekong river  
mekong_river <- dplyr::filter(major_rivers, NAME %in% c("Mekong"))
mekong_river_nutr <- mekong_river %>%
  st_set_crs(st_crs(sp_cur_nutr_cv_NA))

#dam locations in current HPD scenario

#GOODD dataset (Mulligan et al., 2020)
GOOD_dams <-read_sf('C:/Industrial Ecology/Jaar 3/Masterscriptie/Databases/Dams/GOOD/GOOD2_unsnapped/GOOD2_unsnapped.shp')
GOOD_hb <- st_filter(GOOD_dams, sp_cur_nutr_cv_NA) %>%
  st_set_crs(st_crs(sp_cur_nutr_cv_NA)) # set the crs to crs of HydroBASINS


#GRAND dataset (Lehner et al., 2011)
GRAND_dams <-read_sf('C:/Industrial Ecology/Jaar 3/Masterscriptie/Databases/Dams/GRAND/GRanD_Version_1_3/GRanD_dams_v1_3.shp')
GRAND_hb <- st_filter(GRAND_dams, sp_cur_nutr_cv_NA) %>%
  st_set_crs(st_crs(sp_cur_nutr_cv_NA)) # set the crs to crs of HydroBASINS

GRAND_hb$DAM <- "current dam"

GOOD_hb$DAM <- "current dam"

#import locations of future HPD scenarios from fhred (Zarfl et al., 2012)
fhred <- read_excel("C:/Industrial Ecology/Jaar 3/Masterscriptie/Databases/Dams/FHReD/FHReD_2015_future_dams_Zarfl_et_al_beta_version/FHReD_2015_future_dams_Zarfl_et_al_beta_version.xlsx",2)

#create sf file from excel file based on Lon and Lat of dam locations in fhred
fhred_locations <- st_as_sf(fhred, coords = c("Lon_Cleaned","LAT_cleaned")) %>%
  st_set_crs(st_crs(sp_cur_nutr_cv_NA)) # set the crs to crs of HydroBASINS


#Preprocess: future status of CV values across subbasins in the Mekong Basin ----------------------------------------------------------------

#future scenario CV-values sf dataframe with HYDROBASIN geometry
sp_fut_nutr_cv_NA <- create_fut_cv_hb_df (sp_nutr) %>%
  filter(MAIN_BAS != '4120024890') %>% #excluded matched subbasin which is not part of Mekong Basin
  st_as_sf()


#future status CV-values dataframe without geometry
df_sp_fut_nutr_cv_NA <- data.frame(sp_fut_nutr_cv_NA)

#future status (difference future - current)
df_difference_cv_mean <- data.frame(HYBAS_ID = df_sp_fut_nutr_cv_NA$HYBAS_ID,
                                    diff_cv_se = df_sp_fut_nutr_cv_NA$CV_Selenium - sp_cur_nutr_cv_NA$CV_Selenium,
                                    diff_cv_iro = df_sp_fut_nutr_cv_NA$CV_Iron - sp_cur_nutr_cv_NA$CV_Iron,
                                    diff_cv_VA = df_sp_fut_nutr_cv_NA$CV_Vitamin_A - sp_cur_nutr_cv_NA$CV_Vitamin_A,
                                    diff_cv_Ca = df_sp_fut_nutr_cv_NA$CV_Calcium - sp_cur_nutr_cv_NA$CV_Calcium,
                                    diff_cv_Zn = df_sp_fut_nutr_cv_NA$CV_Zinc - sp_cur_nutr_cv_NA$CV_Zinc,
                                    diff_cv_PRO = df_sp_fut_nutr_cv_NA$CV_Protein - sp_cur_nutr_cv_NA$CV_Protein,
                                    diff_mean_se = df_sp_fut_nutr_cv_NA$mean_Selenium - sp_cur_nutr_cv_NA$mean_Selenium,
                                    diff_mean_iro = df_sp_fut_nutr_cv_NA$mean_Iron - sp_cur_nutr_cv_NA$mean_Iron,
                                    diff_mean_VA = df_sp_fut_nutr_cv_NA$mean_Vitamin_A - sp_cur_nutr_cv_NA$mean_Vitamin_A,
                                    diff_mean_Ca = df_sp_fut_nutr_cv_NA$mean_Calcium - sp_cur_nutr_cv_NA$mean_Calcium,
                                    diff_mean_Zn = df_sp_fut_nutr_cv_NA$mean_Zinc - sp_cur_nutr_cv_NA$mean_Zinc,
                                    diff_mean_PRO = df_sp_fut_nutr_cv_NA$mean_Protein - sp_cur_nutr_cv_NA$mean_Protein)

#future impact HPD on Maximum, minimum CV with geometry
#create sf dataframe for difference between future and current nutrient CV values in subbasins
diff_dam_cv <- left_join(df_difference_cv_mean, hb %>% 
                           dplyr::select(HYBAS_ID,MAIN_BAS, DIST_MAIN, SUB_AREA, geometry)) %>%
  
  st_as_sf()

#future impact HPD on Maximum, minumim mean with geometry 
#create sf dataframe for difference between future and current mean nutrient values in subbasins
diff_dam_mean <- left_join(df_difference_cv_mean, hb %>% 
                             dplyr::select(HYBAS_ID,MAIN_BAS, DIST_MAIN, SUB_AREA, geometry)) %>%
  
  st_as_sf()



#**Dam and river layers for plot**
#import shp layer global major rivers
major_rivers <- read_sf('C:/Industrial Ecology/Jaar 3/Masterscriptie/Databases/Dams/majorrivers_0_0/MajorRivers.shp') 

#filter for Mekong river  
mekong_river <- dplyr::filter(major_rivers, NAME %in% c("Mekong"))
mekong_river_nutr <- mekong_river %>%
  st_set_crs(st_crs(sp_cur_nutr_cv_NA))

#dam locations in current HPD scenario

#GOODD dataset (Mulligan et al., 2020)
GOOD_dams <-read_sf('C:/Industrial Ecology/Jaar 3/Masterscriptie/Databases/Dams/GOOD/GOOD2_unsnapped/GOOD2_unsnapped.shp')
GOOD_hb <- st_filter(GOOD_dams, sp_cur_nutr_cv_NA) %>%
  st_set_crs(st_crs(sp_cur_nutr_cv_NA)) # set the crs to crs of HydroBASINS


#GRAND dataset (Lehner et al., 2011)
GRAND_dams <-read_sf('C:/Industrial Ecology/Jaar 3/Masterscriptie/Databases/Dams/GRAND/GRanD_Version_1_3/GRanD_dams_v1_3.shp')
GRAND_hb <- st_filter(GRAND_dams, sp_cur_nutr_cv_NA) %>%
  st_set_crs(st_crs(sp_cur_nutr_cv_NA)) # set the crs to crs of HydroBASINS

GRAND_hb$DAM <- "current dam"

GOOD_hb$DAM <- "current dam"

#import locations of future HPD scenarios from fhred (Zarfl et al., 2012)
fhred <- read_excel("C:/Industrial Ecology/Jaar 3/Masterscriptie/Databases/Dams/FHReD/FHReD_2015_future_dams_Zarfl_et_al_beta_version/FHReD_2015_future_dams_Zarfl_et_al_beta_version.xlsx",2)

#create sf file from excel file based on Lon and Lat of dam locations in fhred
fhred_locations <- st_as_sf(fhred, coords = c("Lon_Cleaned","LAT_cleaned")) %>%
  st_set_crs(st_crs(sp_cur_nutr_cv_NA)) # set the crs to crs of HydroBASINS

#filter for future HPD locations in the Mekong basin
fhred_hb <- st_filter(fhred_locations, sp_cur_nutr_cv_NA)

fhred_hb$DAM <- "future dam"

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

tmap_options_reset() #reset tmap options when you have ran other files before


#Preprocess: current status of NR1 and NR6 values across subbasins in the Mekong Basin ----------------------------------------------------------------

#current dam influence on individual and cumulative nutrient density

#create nutrient density score dataframes (first per species - than mean for every hydrobasin)
cur_sp_nutr <- sp_nutr[, c("HYBAS_ID", "fb_name", "hab_curdams", "hab_futdams", "Iron_mu",
                           "Selenium_mu", "Zinc_mu", "Vitamin_A_mu", "Protein_mu","Calcium_mu", "Omega_3_mu" )]

cur_dam_nutrient <- cur_sp_nutr %>% filter(hab_curdams == TRUE) #filter species which are extinct in that hydrobasin now (current)

#add RNIs of different nutrients to df
cur_dam_nutrient$RNI_adult_iron <- 13.7 #mg/per day
cur_dam_nutrient$RNI_adult_calcium <- 700 #mg/per day
cur_dam_nutrient$RNI_adult_zinc <- 6.5 #mg/per day
cur_dam_nutrient$RNI_adult_selenium <- 34 #ug/per day
cur_dam_nutrient$RNI_adult_protein <- 48 #mg/per day
cur_dam_nutrient$RNI_adult_Vitamin_A <- 600 #ug/per day

#calculate individual nutrient density for every subbasins
cur_dam_nutrient$cu_NRn_adult_iron <- (cur_dam_nutrient$Iron_mu / cur_dam_nutrient$RNI_adult_iron) *100
cur_dam_nutrient$cu_NRn_adult_calcium <- (cur_dam_nutrient$Calcium_mu / cur_dam_nutrient$RNI_adult_calcium) *100
cur_dam_nutrient$cu_NRn_adult_zinc <- (cur_dam_nutrient$Zinc_mu / cur_dam_nutrient$RNI_adult_zinc) *100
cur_dam_nutrient$cu_NRn_adult_selenium <- (cur_dam_nutrient$Selenium_mu / cur_dam_nutrient$RNI_adult_selenium) *100 # selenium is in the WHO 34 ug ipv mg (wrongly cited) / need to be capped at 100%
cur_dam_nutrient$cu_NRn_adult_protein <- (cur_dam_nutrient$Protein_mu / cur_dam_nutrient$RNI_adult_protein) *100
cur_dam_nutrient$cu_NRn_adult_Vitamin_A <- (cur_dam_nutrient$Vitamin_A_mu / cur_dam_nutrient$RNI_adult_Vitamin_A) *100


#apply capped function to individual nutrients
cur_dam_nutrient$cu_NRn_adult_iron <- sapply(cur_dam_nutrient$cu_NRn_adult_iron,capped)
cur_dam_nutrient$cu_NRn_adult_calcium <- sapply(cur_dam_nutrient$cu_NRn_adult_calcium,capped)
cur_dam_nutrient$cu_NRn_adult_zinc <- sapply(cur_dam_nutrient$cu_NRn_adult_zinc,capped)
cur_dam_nutrient$cu_NRn_adult_selenium <- sapply(cur_dam_nutrient$cu_NRn_adult_selenium,capped)
cur_dam_nutrient$cu_NRn_adult_protein <- sapply(cur_dam_nutrient$cu_NRn_adult_protein,capped)
cur_dam_nutrient$cu_NRn_adult_Vitamin_A <- sapply(cur_dam_nutrient$cu_NRn_adult_Vitamin_A,capped)

#take the mean of nutrient densities of current presence of species in subbasin to get current nutrient density value for every subbasin
#and add cumulative nutrient density
cur_dam_adult_mean_hb_join <- cur_dam_nutrient %>%
  group_by(HYBAS_ID)%>%
  summarise(
    cu_mean_NRn_adult_iron = mean(cu_NRn_adult_iron), #Fe density
    cu_mean_NRn_adult_calcium = mean(cu_NRn_adult_calcium), #Ca density
    cu_mean_NRn_adult_zinc = mean(cu_NRn_adult_zinc), #Zn density
    cu_mean_NRn_adult_selenium = mean(cu_NRn_adult_selenium), #Se density
    cu_mean_NRn_adult_protein = mean(cu_NRn_adult_protein), #PRO density
    cu_mean_NRn_adult_Vitamin_A = mean(cu_NRn_adult_Vitamin_A), #VA density
    cu_mean_NRn_adult_tot =  (cu_mean_NRn_adult_iron + cu_mean_NRn_adult_calcium + cu_mean_NRn_adult_zinc +
                                cu_mean_NRn_adult_selenium + cu_mean_NRn_adult_protein + cu_mean_NRn_adult_Vitamin_A)) #cumulative density


#create current sf dataframe based on HYBAS ID and add Distance to outlets and surface of the subbasin
cur_dam_adult_mean_hb <- left_join(cur_dam_adult_mean_hb_join , hb %>% 
                                     dplyr::select(HYBAS_ID,MAIN_BAS, DIST_MAIN, SUB_AREA, geometry)) %>%
  filter(MAIN_BAS != '4120024890') %>% #exclude this subbasin (not part of MB) due to matching error
  
  st_as_sf() 

#create df (without sf) of current densities 
cur_dam_adult_mean_hb_df <- data.frame(cur_dam_adult_mean_hb)


#Preprocess: future status of nutrient density values across subbasins in the Mekong Basin ----------------------------------------------------------------

df_sp_fut_nutr_cv_NA <- data.frame(sp_fut_nutr_cv_NA)
df_sp_cur_nutr_cv_NA <- data.frame(sp_cur_nutr_cv_NA)


#future dam influence
fut_sp_nutr <- sp_nutr[, c("HYBAS_ID", "fb_name", "hab_curdams", "hab_futdams", "Iron_mu",
                           "Selenium_mu", "Zinc_mu", "Vitamin_A_mu", "Protein_mu","Calcium_mu", "Omega_3_mu" )]

fut_dam_nutrient <- fut_sp_nutr %>% filter(hab_futdams == TRUE) #filter species which are extinct in that hydrobasin now (current)

fut_dam_nutrient$RNI_adult_iron <- 13.7 #mg/per day
fut_dam_nutrient$RNI_adult_calcium <- 700 #mg/per day
fut_dam_nutrient$RNI_adult_zinc <- 6.5 #mg/per day
fut_dam_nutrient$RNI_adult_selenium <- 34 #ug/per day
fut_dam_nutrient$RNI_adult_protein <- 48 #mg/per day
fut_dam_nutrient$RNI_adult_Vitamin_A <- 600 #ug/per day


fut_dam_nutrient$fu_NRn_adult_iron <- (fut_dam_nutrient$Iron_mu / fut_dam_nutrient$RNI_adult_iron) *100
fut_dam_nutrient$fu_NRn_adult_calcium <- (fut_dam_nutrient$Calcium_mu / fut_dam_nutrient$RNI_adult_calcium) *100
fut_dam_nutrient$fu_NRn_adult_zinc <- (fut_dam_nutrient$Zinc_mu / fut_dam_nutrient$RNI_adult_zinc) *100
fut_dam_nutrient$fu_NRn_adult_selenium <- (fut_dam_nutrient$Selenium_mu / fut_dam_nutrient$RNI_adult_selenium) *100 # selenium is in the WHO 34 ug ipv mg (wrongly cited) / need to be capped at 100%
fut_dam_nutrient$fu_NRn_adult_protein <- (fut_dam_nutrient$Protein_mu / fut_dam_nutrient$RNI_adult_protein) *100
fut_dam_nutrient$fu_NRn_adult_Vitamin_A <- (fut_dam_nutrient$Vitamin_A_mu / fut_dam_nutrient$RNI_adult_Vitamin_A) *100


fut_dam_nutrient$fu_NRn_adult_iron <- sapply(fut_dam_nutrient$fu_NRn_adult_iron,capped)
fut_dam_nutrient$fu_NRn_adult_calcium <- sapply(fut_dam_nutrient$fu_NRn_adult_calcium,capped)
fut_dam_nutrient$fu_NRn_adult_zinc <- sapply(fut_dam_nutrient$fu_NRn_adult_zinc,capped)
fut_dam_nutrient$fu_NRn_adult_selenium <- sapply(fut_dam_nutrient$fu_NRn_adult_selenium,capped)
fut_dam_nutrient$fu_NRn_adult_protein <- sapply(fut_dam_nutrient$fu_NRn_adult_protein,capped)
fut_dam_nutrient$fu_NRn_adult_Vitamin_A <- sapply(fut_dam_nutrient$fu_NRn_adult_Vitamin_A,capped)


fut_dam_adult_mean_hb_join <- fut_dam_nutrient %>%
  group_by(HYBAS_ID)%>%
  summarise(
    fu_mean_NRn_adult_iron = mean(fu_NRn_adult_iron),
    fu_mean_NRn_adult_calcium = mean(fu_NRn_adult_calcium),
    fu_mean_NRn_adult_zinc = mean(fu_NRn_adult_zinc),
    fu_mean_NRn_adult_selenium = mean(fu_NRn_adult_selenium),
    fu_mean_NRn_adult_protein = mean(fu_NRn_adult_protein),
    fu_mean_NRn_adult_Vitamin_A = mean(fu_NRn_adult_Vitamin_A),
    fu_mean_NRn_adult_tot =  (fu_mean_NRn_adult_iron + fu_mean_NRn_adult_calcium + fu_mean_NRn_adult_zinc +
                                fu_mean_NRn_adult_selenium + fu_mean_NRn_adult_protein + fu_mean_NRn_adult_Vitamin_A))


fut_dam_adult_mean_hb <- left_join(fut_dam_adult_mean_hb_join, hb %>% 
                                     dplyr::select(HYBAS_ID,MAIN_BAS, DIST_MAIN, SUB_AREA, geometry)) %>%
  filter(MAIN_BAS != '4120024890') %>%
  
  st_as_sf() 

df_sp_fut_NR <- data.frame(fut_dam_adult_mean_hb)
df_sp_cur_NR <- data.frame(cur_dam_adult_mean_hb)

df_difference_NR_mean <- data.frame(HYBAS_ID = sp_fut_nutr_cv_NA$HYBAS_ID,
                                    diff_NR_Fe = df_sp_fut_NR$fu_mean_NRn_adult_iron - df_sp_cur_NR$cu_mean_NRn_adult_iron,
                                    diff_NR_Se = df_sp_fut_NR$fu_mean_NRn_adult_selenium - df_sp_cur_NR$cu_mean_NRn_adult_selenium,
                                    diff_NR_VA = df_sp_fut_NR$fu_mean_NRn_adult_Vitamin_A - df_sp_cur_NR$cu_mean_NRn_adult_Vitamin_A,
                                    diff_NR_Ca = df_sp_fut_NR$fu_mean_NRn_adult_calcium - df_sp_cur_NR$cu_mean_NRn_adult_calcium,
                                    diff_NR_Zn = df_sp_fut_NR$fu_mean_NRn_adult_zinc - df_sp_cur_NR$cu_mean_NRn_adult_zinc,
                                    diff_NR_PRO = df_sp_fut_NR$fu_mean_NRn_adult_protein - df_sp_cur_NR$cu_mean_NRn_adult_protein,
                                    diff_NR_tot = df_sp_fut_NR$fu_mean_NRn_adult_tot - df_sp_cur_NR$cu_mean_NRn_adult_tot)



diff_dam_NR <- left_join(df_difference_NR_mean, hb %>% 
                           dplyr::select(HYBAS_ID,MAIN_BAS, DIST_MAIN, SUB_AREA, geometry)) %>%
  filter(MAIN_BAS != '4120024890') %>%
  
  st_as_sf()



#create df without sf of future densities
fut_dam_adult_mean_hb_df <- data.frame(fut_dam_adult_mean_hb)

#Preprocess:**Dam and river layers for plot** ----------------------------------------------------------------

#**Dam and river layers for plot**
#import shp layer global major rivers
major_rivers <- read_sf('C:/Industrial Ecology/Jaar 3/Masterscriptie/Databases/Dams/majorrivers_0_0/MajorRivers.shp') 

#filter for Mekong river  
mekong_river <- dplyr::filter(major_rivers, NAME %in% c("Mekong"))
mekong_river_nutr <- mekong_river %>%
  st_set_crs(st_crs(sp_cur_nutr_cv_NA))

#dam locations in current HPD scenario

#GOODD dataset (Mulligan et al., 2020)
GOOD_dams <-read_sf('C:/Industrial Ecology/Jaar 3/Masterscriptie/Databases/Dams/GOOD/GOOD2_unsnapped/GOOD2_unsnapped.shp')
GOOD_hb <- st_filter(GOOD_dams, sp_cur_nutr_cv_NA) %>%
  st_set_crs(st_crs(sp_cur_nutr_cv_NA)) # set the crs to crs of HydroBASINS


#GRAND dataset (Lehner et al., 2011)
GRAND_dams <-read_sf('C:/Industrial Ecology/Jaar 3/Masterscriptie/Databases/Dams/GRAND/GRanD_Version_1_3/GRanD_dams_v1_3.shp')
GRAND_hb <- st_filter(GRAND_dams, sp_cur_nutr_cv_NA) %>%
  st_set_crs(st_crs(sp_cur_nutr_cv_NA)) # set the crs to crs of HydroBASINS

GRAND_hb$DAM <- "current dam"

GOOD_hb$DAM <- "current dam"

#import locations of future HPD scenarios from fhred (Zarfl et al., 2012)
fhred <- read_excel("C:/Industrial Ecology/Jaar 3/Masterscriptie/Databases/Dams/FHReD/FHReD_2015_future_dams_Zarfl_et_al_beta_version/FHReD_2015_future_dams_Zarfl_et_al_beta_version.xlsx",2)

#create sf file from excel file based on Lon and Lat of dam locations in fhred
fhred_locations <- st_as_sf(fhred, coords = c("Lon_Cleaned","LAT_cleaned")) %>%
  st_set_crs(st_crs(sp_cur_nutr_cv_NA)) # set the crs to crs of HydroBASINS

#filter for future HPD locations in the Mekong basin
fhred_hb <- st_filter(fhred_locations, sp_cur_nutr_cv_NA)

fhred_hb$DAM <- "future dam"

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

tmap_options_reset() #reset tmap options when you have ran other files before



