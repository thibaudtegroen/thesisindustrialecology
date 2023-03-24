source("C:/Industrial Ecology/Jaar 3/Masterscriptie/Databases/R-script/main.R")

tmap_options_reset() #reset tmap options when you have ran other files before
sf::sf_use_s2(FALSE) #spherical geometry switched off
#ttm() #turn on to create interactive plots

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


#Table 7: summary cumulative density
summary(cur_dam_adult_mean_hb$cu_mean_NRn_adult_tot)

#total nutrient density outliers (current and future change)
print(paste("The total outliers of Calcium NR1 in current scenario is", length(boxplot.stats(cur_dam_adult_mean_hb$cu_mean_NRn_adult_calcium)$out)))
print(paste("The total outliers of Iron NR1 in current scenario is",length(boxplot.stats(cur_dam_adult_mean_hb$cu_mean_NRn_adult_iron)$out)))
print(paste("The total outliers of Protein NR1 in current scenario is",length(boxplot.stats(cur_dam_adult_mean_hb$cu_mean_NRn_adult_protein)$out)))
print(paste("The total outliers of Selenium NR1 in current scenario is",length(boxplot.stats(cur_dam_adult_mean_hb$cu_mean_NRn_adult_selenium)$out)))
print(paste("The total outliers of Vitamin A NR1 in current scenario is",length(boxplot.stats(cur_dam_adult_mean_hb$cu_mean_NRn_adult_Vitamin_A)$out)))
print(paste("The total outliers of Zinc NR1 in current scenario is",length(boxplot.stats(cur_dam_adult_mean_hb$cu_mean_NRn_adult_zinc)$out)))
print(paste("The total outliers of NR6 in current scenario is",length(boxplot.stats(cur_dam_adult_mean_hb$cu_mean_NRn_adult_tot)$out)))




#Spatial Analysis: current status of NR1 and NR6 values across subbasins in the Mekong Basin ----------------------------------------------------------------

#Spatial map current dam scenario
#ranges for individual nutrient density 0% - 100%
#ranges for cumulative nutrient density 100% - 400% - since subbasins lack values between 0-100 or 330 - 600%
# figures are indicated as in thesis text 

#Figure 9B: Cumulative Nutrient Density (NR6, %)
plt_cur_density_tot <- tm_shape(cur_dam_adult_mean_hb) + 
  tm_polygons(col = "cu_mean_NRn_adult_tot", 
              style ="cont",
              title= expression("NR"[6] *"(%)"),
              title.size = 2,
              breaks = seq(100, 400, 25),
              palette = "seq",
              border.alpha = 0.05) +
  tm_layout(aes.palette = list(seq = "YlOrRd"),
            legend.show = T,
            legend.outside = T,
            scale = 2,
            title.size = 2,
            frame = FALSE,
            main.title = "B",
            main.title.size = 2,
            main.title.position = c('left', 'top'),
            legend.width = 10,
            legend.text.size = 1.5,
            legend.format = list(fun = function (x) {ifelse(x %in% c(0, 100, 200, 300, 400), x, "")}),
            legend.position  = c('right', 'top'),
            legend.height = 1)

#save
#tmap_save(tm = plt_cur_density_tot, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/NR/cur_NR_tot.png", 
#         device = png, width = 1800, height = 3200, dpi =300)

#Figure 7A: Current dams
plt_cu_dams_NR_CV <- tm_shape(sp_cur_nutr_cv_NA) + 
  tm_polygons(col = "white", border.alpha = 0.1) +
  tm_shape(mekong_river_nutr) + tm_lines(lwd = 1, col = "blue") +
  
  tm_shape(GOOD_hb) + 
  tm_dots(size = 0.0075, col = "black") +
  
  tm_shape(GRAND_hb) + 
  tm_dots(size = 0.0075, col = "black") +
  
  tm_layout(legend.show = F,
            main.title = "A",
            main.title.size = 1,
            main.title.position = c('left', 'top'),
            frame = F,
            legend.width = 10,
            legend.text.size = 1.5,
            legend.position  = c('right', 'top'),
            #legend.format = list(fun = function (x) {ifelse(x %in% c(-50,-40, -30, -20, -10, 0, 10, 20, 30, 40, 50), x, "")}),
            legend.height = 1)

#Figure 7B: Calcium density
plt_cur_density_Ca <- tm_shape(cur_dam_adult_mean_hb) + 
  tm_polygons(col = "cu_mean_NRn_adult_calcium", 
              style ="cont",
              title="",
              breaks = seq(0, 100, 5),
              palette = "seq",
              border.alpha = 0.05) +
  tm_layout(aes.palette = list(seq = "YlOrRd"),
            legend.show = F,
            frame = FALSE,
            main.title = "B",
            main.title.size = 1,
            main.title.position = c('left', 'top'),
            legend.width = 10,
            legend.text.size = 1.5,
            legend.format = list(fun = function (x) {ifelse(x %in% c(0, 20, 40, 60, 80, 100), x, "")}),
            legend.position  = c('right', 'top'),
            legend.height = 1)

#save
#tmap_save(tm = plt_cur_density_Ca, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/NR/cur_NR_Ca.png", device = png)

#Figure 7C: Fe density
plt_cur_density_iro <- tm_shape(cur_dam_adult_mean_hb) + 
  tm_polygons(col = "cu_mean_NRn_adult_iron", 
              style ="cont",
              title="",
              breaks = seq(0, 100, 5),
              palette = "seq",
              border.alpha = 0.05) +
  tm_layout(aes.palette = list(seq = "YlOrRd"),
            legend.show = F,
            frame = FALSE,
            main.title = "C",
            main.title.size = 1,
            main.title.position = c('left', 'top'),
            legend.width = 10,
            legend.text.size = 1.5,
            legend.format = list(fun = function (x) {ifelse(x %in% c(0, 20, 40, 60, 80, 100), x, "")}),
            legend.position  = c('right', 'top'),
            legend.height = 1)

#tmap_save(tm = plt_cur_density_iro, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/NR/cur_NR_Fe.png", device = png)


#Figure 7D: Protein(PRO) density
plt_cur_density_PRO <- tm_shape(cur_dam_adult_mean_hb) + 
  tm_polygons(col = "cu_mean_NRn_adult_protein", 
              style ="cont",
              title="",
              breaks = seq(0, 100, 5),
              palette = "seq",
              border.alpha = 0.05) +
  tm_layout(aes.palette = list(seq = "YlOrRd"),
            legend.show = F,
            frame = FALSE,
            main.title = "D",
            main.title.size = 1,
            main.title.position = c('left', 'top'),
            legend.width = 10,
            legend.text.size = 1.5,
            legend.format = list(fun = function (x) {ifelse(x %in% c(0, 20, 40, 60, 80, 100), x, "")}),
            legend.position  = c('right', 'top'),
            legend.height = 1)


#tmap_save(tm = plt_cur_density_PRO, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/NR/cur_NR_PRO.png", device = png)


#Figure 7E: Selenium density
plt_cur_density_se <- tm_shape(cur_dam_adult_mean_hb) + 
  tm_polygons(col = "cu_mean_NRn_adult_selenium", 
              style ="cont",
              title="",
              breaks = seq(0, 100, 5),
              palette = "seq",
              border.alpha = 0.05) +
  tm_layout(aes.palette = list(seq = "YlOrRd"),
            legend.show = F,
            frame = FALSE,
            main.title = "E",
            main.title.size = 1,
            main.title.position = c('left', 'top'),
            legend.width = 10,
            legend.text.size = 1.5,
            legend.format = list(fun = function (x) {ifelse(x %in% c(0, 20, 40, 60, 80, 100), x, "")}),
            legend.position  = c('right', 'top'),
            legend.height = 1)

#tmap_save(tm = plt_cur_density_se, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/NR/cur_NR_Se.png", device = png)


#Figure 7F: VA density
plt_cur_density_VA <- tm_shape(cur_dam_adult_mean_hb) + 
  tm_polygons(col = "cu_mean_NRn_adult_Vitamin_A", 
              style ="cont",
              title="",
              breaks = seq(0, 100, 5),
              palette = "seq",
              border.alpha = 0.05) +
  tm_layout(aes.palette = list(seq= "YlOrRd"),
            legend.show = F,
            frame = FALSE,
            main.title = "F",
            main.title.size = 1,
            main.title.position = c('left', 'top'),
            legend.width = 10,
            legend.text.size = 1.5,
            legend.format = list(fun = function (x) {ifelse(x %in% c(0, 20, 40, 60, 80, 100), x, "")}),
            legend.position  = c('right', 'top'),
            legend.height = 1)

#tmap_save(tm = plt_cur_density_VA, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/NR/cur_NR_VA.png", device = png)

#Figure 7G: Zinc density
plt_cur_density_Zn <- tm_shape(cur_dam_adult_mean_hb) + 
  tm_polygons(col = "cu_mean_NRn_adult_zinc", 
              style ="cont",
              title="",
              breaks = seq(0, 100, 5),
              palette = "seq",
              border.alpha = 0.05) +
  tm_layout(aes.palette = list(seq = "YlOrRd"),
            legend.show = F,
            frame = FALSE,
            main.title = "G",
            main.title.position = c('left', 'top'),
            main.title.size = 1,
            legend.width = 10,
            legend.text.size = 1.5,
            legend.format = list(fun = function (x) {ifelse(x %in% c(0, 20, 40, 60, 80, 100), x, "")}),
            legend.position  = c('right', 'top'),
            legend.height = 1)


#tmap_save(tm = plt_cur_density_Zn, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/NR/cur_NR_Zn.png", device = png)

#individual nutrient density legend for arranged spatial map 
legend_cur_density_spatial <- tm_shape(cur_dam_adult_mean_hb) + 
  tm_polygons(col = "cu_mean_NRn_adult_zinc", 
              style ="cont",
              title= expression('NR'[1]* "(%)"),
              breaks = seq(0, 100, 5),
              palette = "seq",
              border.alpha = 0.05) +
  tm_layout(aes.palette = list(seq = "YlOrRd"),
            legend.only = T,
            main.title = "",
            main.title.position = c('left', 'top'),
            main.title.size = 1,
            legend.width = 10,
            legend.text.size = 1,
            legend.position  = c('left', 'top'),
            legend.height = 2,
            legend.format = list(fun = function (x) {ifelse(x %in% c(0, 20, 40, 60, 80, 100), x, "")}))

#arranged map
spatial_cur_density_plot_Ca_Fe_PRO_Se_VA_Zn <- tmap_arrange(plt_cu_dams_NR_CV, plt_cur_density_Ca, plt_cur_density_iro, plt_cur_density_PRO,
                                                            plt_cur_density_se, plt_cur_density_VA, plt_cur_density_Zn, legend_cur_density_spatial,
                                                            ncol = 4, nrow = 2) 
#save
#tmap_save(tm = spatial_cur_density_plot_Ca_Fe_PRO_Se_VA_Zn, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/NR/cur_density_arrange_all.png",
      #    width = 2000, height = 2000, dpi = 300, device = png)

