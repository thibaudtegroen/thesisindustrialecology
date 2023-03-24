source("C:/Industrial Ecology/Jaar 3/Masterscriptie/Databases/R-script/main.R")

#Table C2: Current and difference map of total species per hydrobasin

#current
df_cur_sp_nutr <- sp_nutr %>% filter(hab_curdams == TRUE)
df_cur_sp_nutr_1 <- df_cur_sp_nutr %>%
  group_by(HYBAS_ID) %>%
  summarise(total_count = n(),
            .groups = 'drop') %>%
  as.data.frame()

df_cur_sp_nutr_2 <- left_join(df_cur_sp_nutr_1, hb %>% 
                                dplyr::select(HYBAS_ID,MAIN_BAS, geometry)) %>%
  filter(MAIN_BAS != '4120024890') %>%
  
  st_as_sf()


plt_cur_shape_total_sp <- tm_shape(df_cur_sp_nutr_2) + 
  tm_polygons("total_count", 
              style="cont", 
              breaks = seq(0, 64, 4), 
              title="",
              palette = "Reds",
              #n = 10, 
              textNA = "\u2264 1 species",
              colorNA = "grey",              
              border.alpha = 0.05) +
  tm_layout(legend.show = T,
            frame = FALSE,
            main.title = "Total species",
            main.title.position = c('right', 'top'),
            main.title.size = 3,
            legend.width = 10,
            legend.text.size = 1.5,
            legend.position  = c('right', 'top'),
            legend.format = list(fun = function (x) {ifelse(x %in% seq(0, 64, 8), x, "")}),
            legend.height = 1,
            legend.outside = T) 



tmap_save(tm = plt_cur_shape_total_sp, 
          filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/cur_total_sp_hb.png", 
          device = png,
          width = 3000, 
          height = 3300, 
          dpi =300)
tmap_save(plt_cur_shape_total_sp, 
          filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/cur_total_sp_hb.html")

#difference
df_fut_sp_nutr <- sp_nutr %>% filter(hab_futdams == TRUE)
df_fut_sp_nutr_1 <- df_fut_sp_nutr %>%
  group_by(HYBAS_ID) %>%
  summarise(total_count = n(),
            .groups = 'drop') %>%
  as.data.frame()
df_diff_sp <- data.frame(HYBAS_ID = df_fut_sp_nutr_1$HYBAS_ID,
                         total_species = df_fut_sp_nutr_1$total_count - df_cur_sp_nutr_1$total_count)

df_diff_sp_1 <- left_join(df_diff_sp, hb %>% 
                            dplyr::select(HYBAS_ID,MAIN_BAS, geometry)) %>%
  filter(MAIN_BAS != '4120024890') %>%
  
  st_as_sf()

plt_fut_shape_total_sp <- tm_shape(df_diff_sp_1) + 
  tm_polygons("total_species", 
              style="cont", 
              breaks = seq(-15, 15, 1), 
              title="",
              palette = "RdBu",
              #n = 10, 
              textNA = "\u2264 1 species",
              colorNA = "grey",              
              border.alpha = 0.05) +
  tm_layout(legend.show = T,
            frame = FALSE,
            main.title = "Total species",
            main.title.position = c('right', 'top'),
            main.title.size = 3,
            legend.width = 10,
            legend.text.size = 1.5,
            legend.position  = c('right', 'top'),
            legend.format = list(fun = function (x) {ifelse(x %in% seq(-15, 15, 3), x, "")}),
            legend.height = 1,
            legend.outside = T) 

tmap_save(tm = plt_fut_shape_total_sp, 
          filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/fut_total_sp_hb.png", 
          device = png,
          width = 3000, 
          height = 3300, 
          dpi =300)
tmap_save(plt_fut_shape_total_sp, 
          filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/fut_total_sp_hb.html")