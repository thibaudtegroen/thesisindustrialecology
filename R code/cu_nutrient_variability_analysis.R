
source("C:/Industrial Ecology/Jaar 3/Masterscriptie/Databases/R-script/main.R")

tmap_options_reset() #reset tmap options when you have ran other files before
sf::sf_use_s2(FALSE) #spherical geometry switched off
#ttm() #turn on to create interactive plots

#Cumulative distribution function nutrients freshwater fish species in Mekong Basin (MB) ----------------------------------------------------------------

sp_nutr_unique <- sp_nutr[!duplicated(sp_nutr[,'fb_name']),] # only unique species 

#exclude species which are not present due to HPD
sp_cur_nutr_unique <- sp_nutr_unique %>% filter(hab_curdams == TRUE) #current with HPD


#Figure 2:cumulative distribution function of nutrient concentrations of freshwater fishes in total MB
#calcium
plot_cal <- ggplot(sp_cur_nutr_unique, aes(x = Calcium_mu)) + 
  stat_ecdf(mapping = aes(x = Calcium_mu), 
            #geom = "line", 
            color = 'black',
  ) +
  labs(y = "f(Ca)", x = "Ca concentration (mg/100g)") +
  
  #scale 0-900 mg/100g
  scale_x_continuous(expand = expansion (0),
                     breaks = seq(from=0, 
                                  to = 900, 
                                  by = 100)) 

#iron
plot_iron <- ggplot(sp_cur_nutr_unique, aes(x = Iron_mu)) + 
  stat_ecdf(mapping = aes(), #geom = "line"
  ) +
  labs(y = "f(Fe)", x = "Fe concentration (mg/100g)") +
  scale_x_continuous(expand = expansion (0),
                     breaks = seq(from=0, 
                                  to = 10, 
                                  by = 1))

#protein
plot_protein <- ggplot(sp_cur_nutr_unique, aes(x = Protein_mu)) + 
  stat_ecdf(mapping = aes(), #geom = "line"
  ) +
  labs(y = "f(PRO)", x = "PRO concentration (g/100g)")+
  scale_x_continuous(expand = expansion (0),
                     breaks = seq(from=15, 
                                  to = 20, 
                                  by = 1))

#selenium
plot_sel <- ggplot(sp_cur_nutr_unique, aes(x = Selenium_mu)) + 
  stat_ecdf(mapping = aes(), #geom = "line"
  ) +
  labs(y = "f(Se)", x = "Se concentration (μg/100g)")+
  scale_x_continuous(expand = expansion (0),
                     breaks = seq(from=0, 
                                  to = 400, 
                                  by = 50))
#selenium
plot_vit_A <- ggplot(sp_cur_nutr_unique, aes(x = Vitamin_A_mu )) + 
  stat_ecdf(mapping = aes(), #geom = "line"
  ) +
  labs(y = "f(VA)", x = "VA concentration (μg/100g)") +
  scale_x_continuous(expand = expansion (0),
                     breaks = seq(from=0, 
                                  to = 600, 
                                  by = 50))

#zinc
plot_zinc <- ggplot(sp_cur_nutr_unique, aes(x = Zinc_mu)) + 
  stat_ecdf(mapping = aes(), #geom = "line"
  ) +
  labs(y = "f(Zn)", x = "Zn concentration (μg/100g)")+
  scale_x_continuous(expand = expansion (0),
                     breaks = seq(from=0, 
                                  to = 5, 
                                  by = 1))

#arranged ecdf plot
ecdf_plot = ggarrange(plot_cal, plot_iron, plot_protein, plot_sel, plot_vit_A, plot_zinc,
                      labels = c("A","B","C","D","E","F"),
                      ncol = 3, nrow =3)
#save
#ggsave(filename = "ecdf_plot_Ca_Fe_PRO_Se_VA_Zn.png", plot = ecdf_plot,
   #    path = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken",
    #   device = png, width = 10, height = 7, dpi = 300)

#Analysis: Current mean CV in subbasins across MB   ----------------------------------------------------------------

#total CV outliers (of mean CV in )
print(paste("The total outliers of Calcium CV in current scenario is", length(boxplot.stats(df_sp_cur_nutr_cv_NA$CV_Calcium)$out)))
print(paste("The total outliers of Iron CV in current scenario is",length(boxplot.stats(df_sp_cur_nutr_cv_NA$CV_Iron)$out)))
print(paste("The total outliers of Protein CV in current scenario is",length(boxplot.stats(df_sp_cur_nutr_cv_NA$CV_Protein)$out)))
print(paste("The total outliers of Selenium CV in current scenario is",length(boxplot.stats(df_sp_cur_nutr_cv_NA$CV_Selenium)$out)))
print(paste("The total outliers of Vitamin A CV in current scenario is",length(boxplot.stats(df_sp_cur_nutr_cv_NA$CV_Vitamin_A)$out)))
print(paste("The total outliers of Zinc CV in current scenario is",length(boxplot.stats(df_sp_cur_nutr_cv_NA$CV_Zinc)$out)))

#create dataframe for boxplot
df_cur_boxplot_cv <- melt(data.frame(
  Ca = df_sp_cur_nutr_cv_NA$CV_Calcium,
  Fe = df_sp_cur_nutr_cv_NA$CV_Iron,
  PRO = df_sp_cur_nutr_cv_NA$CV_Protein,
  Se = df_sp_cur_nutr_cv_NA$CV_Selenium,
  VA = df_sp_cur_nutr_cv_NA$CV_Vitamin_A,
  Zn = df_sp_cur_nutr_cv_NA$CV_Zinc), variable.name = "nutrient", value.name = "CV"
)

#Figure 3: Boxplot of current mean CV for all nutrients 
cu_boxplot_plot <- df_cur_boxplot_cv %>%
  ggplot(aes(x = nutrient, y = CV)) +
  geom_boxplot(aes(x = nutrient, y = CV)) +
  coord_flip() +
  scale_y_continuous(expand = expansion (0),
                     breaks = seq(from=0, 
                                  to = 150, 
                                  by = 10),
                     limits = c(0,140)) +
  ylab("Coefficient of variation (%)") +
  theme(
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    panel.background = element_blank(),
    plot.title = element_text(size = 22, face = "bold",
                              hjust = 0.5,
                              margin = margin(b = 15)),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 22, color = "black",
                              face = "bold"),
    axis.text = element_text(size = 22, color = "black"),
    axis.text.x = element_text(margin = margin(t = 10)),
    axis.text.y = element_text(size = 17),
    #axis.title.y = element_text(margin = margin(r = 10)),
    axis.title.y = element_blank(),
    #axis.ticks.x = element_blank(),
    legend.position = c(0.8, 0.8),
    legend.background = element_rect(color = "black"),
    legend.text = element_text(size = 15),
    legend.margin = margin(t = 5, l = 5, r = 5, b = 5),
    legend.key = element_rect(color = NA, fill = NA))

#save figure 3
#ggsave(filename = "boxplot_cu_cv_plot_Ca_Fe_PRO_Se_VA_Zn.png", plot = cu_boxplot_plot,
#path = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken",
#device = png, width = 12, height = 8, dpi = 300)

#Spatial analysis: current scenario mean CV in subbasins   ----------------------------------------------------------------

#Nutrient CV spatial distribution in current status (Figures are indicated as in the thesis text).
#all ranges of the figures are from 0 % to 140%

#Figure 4A: 
#plt_cur_shape_dams <- tm_shape(sp_cur_nutr_cv_NA) + 
#  tm_polygons("HYBAS_ID", col = "grey",
    #          border.alpha = 0.05) +
  
#  plt_cu_dams +
  
#  tm_layout(legend.show = F,
 #           frame = FALSE,
 #           main.title = "A",
 #           main.title.position = c('left', 'top'),
 #           main.title.size = 1,
 #           legend.width = 10,
  #          legend.text.size = 1.5,
  ##          legend.position  = c('right', 'top'),
  #          legend.height = 1) 


#Locations of Dams, Mekong River and subbasin borders
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

#tmap_save(tm = plt_cu_dams_NR_CV, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/cv/plt_cu_dams_NR_CV.png", 
        #  device = png)

#Figure 4A: Calcium (Ca) CV current distribution
plt_cur_shape_cv_cal <- tm_shape(sp_cur_nutr_cv_NA) + 
  tm_polygons("CV_Calcium", 
              style="cont", 
              breaks = seq(0, 140, 10), 
              title="",
              palette = "Reds",
              #n = 10, 
              textNA = "\u2264 1 species",
              colorNA = "grey",              
              border.alpha = 0.05) +
  
  tm_layout(legend.show = F,
            frame = FALSE,
            main.title = "B",
            main.title.position = c('left', 'top'),
            main.title.size = 1,
            legend.width = 10,
            legend.text.size = 1.5,
            legend.position  = c('right', 'top'),
            legend.format = list(fun = function (x) {ifelse(x %in% c(0, 10, 90, 140), x, "")}),
            legend.height = 1) 
#save
#tmap_save(tm = plt_cur_shape_cv_cal, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/cv/cur_cv_Ca.png", device = png)

#Figure 4B: Iron (Fe) CV current
plt_cur_shape_cv_iro <- tm_shape(sp_cur_nutr_cv_NA) + 
  tm_polygons("CV_Iron", 
              style="cont", 
              breaks = seq(0, 140, 10),
              title="",
              palette = "Reds",
              #n = 10,
              textNA = "\u2264 1 species",
              colorNA = "grey",
              border.alpha = 0.05) +
  tm_layout(legend.show = F,
            frame = FALSE,
            main.title = "C",
            main.title.position = c('left', 'top'),
            main.title.size = 1,
            legend.width = 10,
            legend.text.size = 1.5,
            legend.position  = c('right', 'top'),
            legend.format = list(fun = function (x) {ifelse(x %in% c(0, 10, 90, 140), x, "")}),
            legend.height = 1) 
#save
#tmap_save(tm = plt_cur_shape_cv_iro, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/cv/cur_cv_Fe.png", device = png)

#Figure 4C: Protein CV current
plt_cur_shape_cv_prot <- tm_shape(sp_cur_nutr_cv_NA) + 
  tm_polygons("CV_Protein", 
              style="cont", 
              breaks = seq(0, 140, 10),
              title = "",
              #n = 10,
              palette = "Reds",
              textNA = "\u2264 1 species",
              colorNA = "grey",
              border.alpha = 0.05) +
  tm_layout(legend.show = F,
            #legend.outside = TRUE,
            #legend.outside.position = 'right',
            #legend.outside.size = 0.2,
            #legend.title.size = 0.1,
            #legend.width = 
            #legend.height =
            frame = FALSE,
            main.title = "D",
            main.title.position = c('left', 'top'),
            main.title.size = 1,
            legend.width = 10,
            legend.text.size = 1.5,
            legend.position  = c('right', 'top'),
            legend.format = list(fun = function (x) {ifelse(x %in% c(0, 10, 90, 140), x, "")}),
            legend.height = 1)
#save
#tmap_save(tm = plt_cur_shape_cv_prot, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/cv/cur_cv_PRO.png", device = png)

#Figure 4D: Selenium CV current
plt_cur_shape_cv_sel <- tm_shape(sp_cur_nutr_cv_NA) + 
  tm_polygons("CV_Selenium", 
              style="cont", 
              breaks = seq(0, 140, 10),
              title="current Selenium_CV (%)",
              palette = "Reds",
              #n= 10,
              textNA = "\u2264 1 species",
              colorNA = "grey",
              border.alpha = 0.05) +
  
  tm_layout(legend.show = F,
            frame = FALSE,
            main.title = "E",
            main.title.position = c('left', 'top'),
            main.title.size = 1, 
            legend.width = 10,
            legend.text.size = 1.5,
            legend.position  = c('right', 'top'),
            legend.format = list(fun = function (x) {ifelse(x %in% c(0, 10, 90, 140), x, "")}),
            legend.height = 1) 
#save
#tmap_save(tm = plt_cur_shape_cv_sel, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/cv/cur_cv_Se.png", device = png)

#Figure 4E: Vitamin A CV current PRESENTATION
plt_cur_shape_cv_vit_A <- tm_shape(sp_cur_nutr_cv_NA) + 
  tm_polygons("CV_Vitamin_A", 
              style="cont", 
              breaks = seq(0, 140, 10),
              title="",
              palette = "Reds",
              #n = 10,
              textNA = "\u2264 1 species",
              colorNA = "grey",
              border.alpha = 0.05) +
  tm_layout(legend.show = F,
            frame = FALSE,
            main.title = "F",
            main.title.position = c('left', 'top'),
            main.title.size = 1, 
            legend.width = 10,
            legend.text.size = 1.5,
            legend.position  = c('right', 'top'),
            legend.format = list(fun = function (x) {ifelse(x %in% c(0, 10, 90, 140), x, "")}),
            legend.height = 1) 

#save 
#tmap_save(tm = plt_cur_shape_cv_vit_A, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/cv/cur_cv_VA.png", device = png)

#Figure 4F: Zinc CV current
plt_cur_shape_cv_zin <- tm_shape(sp_cur_nutr_cv_NA) + 
  tm_polygons("CV_Zinc", 
              style="cont", 
              breaks = seq(0, 140, 10),
              title="",
              palette = "Reds",
              #n = 10,
              textNA = "\u2264 1 species",
              colorNA = "grey",
              border.alpha = 0.05) +
  tm_layout(legend.show = F,
            frame = FALSE,
            main.title = "G",
            main.title.position = c('left', 'top'),
            main.title.size = 1, 
            legend.width = 10,
            legend.text.size = 1.5,
            legend.position  = c('left', 'top'),
            legend.format = list(fun = function (x) {ifelse(x %in% c(0, 10, 90, 140), x, "")}),
            legend.height = 1) 
#save
#tmap_save(tm = plt_cur_shape_cv_zin, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/cv/cur_cv_Zn.png", device = png)


#general legend for arranged plot
legend_spatial_cv_cur_map <- tm_shape(sp_cur_nutr_cv_NA) +
  tm_polygons("CV_Zinc", 
              style="cont", 
              breaks = seq(0, 140, 10),
              title= "CV (%)",
              palette = "Reds",
              #n = 10,
              textNA = "\u2264 1 species",
              colorNA = "grey",
              border.alpha = 0.05) +
  
  tm_shape(mekong_river_nutr) + tm_lines("NAME", lwd = 1, col = "blue", title.lwd="") +
  
  tm_shape(GOOD_hb) + 
  tm_dots(size = 0.0075, col = "black") +
  
  tm_shape(GRAND_hb) + 
  tm_dots(size = 0.0075, col = "black",) +
  
  tm_layout(legend.only = T, 
            scale=3, 
            asp=0,
            main.title= "Coefficient of variation (%)", 
            main.title.position = c('left', 'top'),
            main.title.size = 3,
            legend.width = 15,
            legend.text.size = 1,
            legend.position  = c('left', 'top'),
            legend.height = 3,
            legend.format = list(fun = function (x) {ifelse(x %in% c(0, 10, 90, 140), x, "")}))


#arranged plot
spatial_cur_cv_plot_Ca_Fe_PRO_Se_VA_Zn <- tmap_arrange(plt_cu_dams_NR_CV, plt_cur_shape_cv_cal, plt_cur_shape_cv_iro, plt_cur_shape_cv_prot,
                                                       plt_cur_shape_cv_sel, plt_cur_shape_cv_vit_A, plt_cur_shape_cv_zin, legend_spatial_cv_cur_map,
                                                       ncol = 4, nrow = 2) 
#save arranged plot 
#tmap_save(tm = spatial_cur_cv_plot_Ca_Fe_PRO_Se_VA_Zn, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/cv/cur_arrange_all.png", device = png)



