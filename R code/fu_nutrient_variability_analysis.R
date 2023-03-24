
source("C:/Industrial Ecology/Jaar 3/Masterscriptie/Databases/R-script/main.R")

tmap_options_reset() #reset tmap options when you have ran other files before
sf::sf_use_s2(FALSE) #spherical geometry switched off
#ttm() #turn on to create interactive plots

#This code describes the difference between current and future status of CV as proxy for future CV

#Analysis: df, boxplot and outliers in future mean CV in subbasins   ----------------------------------------------------------------

#Overall Difference dataframe min, max, values and mean, min and max CV subbasins
#df with min, max of mean and min, max, and mean of nutrient CV values 
df_fut_boxplot_cv <- melt(data.frame(
  Ca = df_difference_cv_mean$diff_cv_Ca,
  Fe = df_difference_cv_mean$diff_cv_iro,
  PRO = df_difference_cv_mean$diff_cv_PRO,
  Se = df_difference_cv_mean$diff_cv_se,
  VA = df_difference_cv_mean$diff_cv_VA,
  Zn = df_difference_cv_mean$diff_cv_Zn), variable.name = "nutrient", value.name = "diff_CV"
)

#amount of outliers per nutrient
length(boxplot.stats(df_difference_cv_mean$diff_cv_Ca)$out) #Ca
length(boxplot.stats(df_difference_cv_mean$diff_cv_iro)$out) #Fe
length(boxplot.stats(df_difference_cv_mean$diff_cv_PRO)$out) #PRO
length(boxplot.stats(df_difference_cv_mean$diff_cv_se)$out) #Se
length(boxplot.stats(df_difference_cv_mean$diff_cv_VA)$out) #VA
length(boxplot.stats(df_difference_cv_mean$diff_cv_Zn)$out) #Zn

#figure 5: boxplot diff CV
fu_boxplot_cv <- df_fut_boxplot_cv %>%
  ggplot(aes(x = nutrient, y = diff_CV)) +
  geom_boxplot(aes(x = nutrient, y = diff_CV)) +
  coord_flip() +
  scale_y_continuous(expand = expansion (0),
                     breaks = seq(from=-60, 
                                  to = 60, 
                                  by = 10),
                     limits = c(-60,60)) +
  ylab("Change in Coefficient of variation (%)") +
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



#ggsave(filename = "boxplot_fut_cv_plot_Ca_Fe_PRO_Se_VA_Zn.png", plot = fu_boxplot_cv,
      # path = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken",
      # device = png, width = 12, height = 8, dpi = 300)

#Spatial analysis: Future mean CV in subbasins   ----------------------------------------------------------------

#maps future status: spatial distribution of difference between future and current CV-values of subbasins (Figure 5)
#ranges kept on -50% to 50% for all nutrients
plt_cu_fu_dams_NR_CV <- tm_shape(sp_cur_nutr_cv_NA) + 
  tm_polygons(col = "white", border.alpha = 0.1) +
  tm_shape(mekong_river_nutr) + tm_lines(lwd = 1, col = "blue") +
  
  tm_shape(GOOD_hb) + 
  tm_dots(size = 0.0075, col = "black") +
  
  tm_shape(GRAND_hb) + 
  tm_dots(size = 0.0075, col = "black") +
  
  tm_shape(fhred_hb) +
  tm_dots(size = 0.0075, col = "red") +
  
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

#Figure 5A: diff Ca CV
plt_diff_shape_cv_Ca <- tm_shape(diff_dam_cv) +
  tm_polygons("diff_cv_Ca", 
              style="cont", 
              breaks = c(-50,-40, -30, -20, -10, 0, 10, 20, 30, 40, 50), 
              title="",
              palette = "RdBu",
              #n= 10,
              midpoint = NA,
              textNA = "\u2264 1 species",
              colorNA = "grey",
              border.alpha = 0.025) +
  
  tm_layout(legend.show = F,
            main.title = "B",
            main.title.position = c('left', 'top'),
            frame = FALSE,
            legend.width = 10,
            legend.text.size = 1.5,
            legend.position  = c('right', 'top'),
            #legend.format = list(fun = function (x) {ifelse(x %in% c(-50,-40, -30, -20, -10, 0, 10, 20, 30, 40, 50), x, "")}),
            legend.height = 1)
#save
#tmap_save(tm = plt_diff_shape_cv_Ca , filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/cv/diff_cv_Ca.png", device = png)


#Figure 5B: diff Fe CV
plt_diff_shape_cv_iro <- tm_shape(diff_dam_cv) +
  tm_polygons("diff_cv_iro", 
              style="cont", 
              breaks = c(-50,-40, -30, -20, -10, 0, 10, 20, 30, 40, 50), 
              title="",
              palette = "RdBu",
              #n= 10,
              midpoint = NA,
              textNA = "\u2264 1 species",
              colorNA = "grey",
              border.alpha = 0.025) +
  
  tm_layout(legend.show = F,
            main.title = "C",
            main.title.position = c('left', 'top'),
            frame = FALSE,
            legend.width = 10,
            legend.text.size = 1.5,
            legend.position  = c('right', 'top'),
            #legend.format = list(fun = function (x) {ifelse(x %in% c(-50,-40, -30, -20, -10, 0, 10, 20, 30, 40, 50), x, "")}),
            legend.height = 1)
#save
#tmap_save(tm = plt_diff_shape_cv_iro, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/cv/diff_Fe.png", device = png)

#Figure 5C: diff protein CV
plt_diff_shape_cv_PRO <- tm_shape(diff_dam_cv) +
  tm_polygons("diff_cv_PRO", 
              style="cont", 
              breaks = c(-50,-40, -30, -20, -10, 0, 10, 20, 30, 40, 50), 
              title="",
              palette = "RdBu",
              #n= 10,
              midpoint = NA,
              textNA = "\u2264 1 species",
              colorNA = "grey",
              border.alpha = 0.025) +
  
  tm_layout(legend.show = F,
            main.title = "D",
            main.title.position = c('left', 'top'),
            frame = FALSE,
            legend.width = 10,
            legend.text.size = 1.5,
            legend.position  = c('right', 'top'),
            #legend.format = list(fun = function (x) {ifelse(x %in% c(-50,-40, -30, -20, -10, 0, 10, 20, 30, 40, 50), x, "")}),
            legend.height = 1)
#save
#tmap_save(tm = plt_diff_shape_cv_PRO, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/cv/diff_cv_PRO.png", device = png)

#Figure 5D: Se current and future difference CV
plt_diff_shape_cv_sel <- tm_shape(diff_dam_cv) + 
  tm_polygons("diff_cv_se", 
              style="cont", 
              breaks = c(-50,-40, -30, -20, -10, 0, 10, 20, 30, 40, 50), 
              title="",
              palette = "RdBu",
              #n= 10,
              midpoint = NA,
              textNA = "\u2264 1 species",
              colorNA = "grey",
              border.alpha = 0.025) +
  
  tm_layout(legend.show = F,
            main.title = "E",
            main.title.position = c('left', 'top'),
            frame = FALSE,
            legend.width = 10,
            legend.text.size = 1.5,
            legend.position  = c('right', 'top'),
            #legend.format = list(fun = function (x) {ifelse(x %in% c(-50,-40, -30, -20, -10, 0, 10, 20, 30, 40, 50), x, "")}),
            legend.height = 1) 
#save
#tmap_save(tm = plt_diff_shape_cv_sel, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/cv/diff_cv_Se.png", device = png)

#Figure 5E: diff VA CV
plt_diff_shape_cv_VA <- tm_shape(diff_dam_cv) +
  tm_polygons("diff_cv_VA", 
              style="cont", 
              breaks = c(-50,-40, -30, -20, -10, 0, 10, 20, 30, 40, 50), 
              title="",
              palette = "RdBu",
              #n= 10,
              midpoint = NA,
              textNA = "\u2264 1 species",
              colorNA = "grey",
              border.alpha = 0.025) +
  
  tm_layout(legend.show = F,
            main.title = "F",
            main.title.position = c('left', 'top'),
            frame = FALSE,
            legend.width = 10,
            legend.text.size = 1.5,
            legend.position  = c('right', 'top'),
            #legend.format = list(fun = function (x) {ifelse(x %in% c(-50,-40, -30, -20, -10, 0, 10, 20, 30, 40, 50), x, "")}),
            legend.height = 1)
#save
#tmap_save(tm = plt_diff_shape_cv_VA, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/cv/diff_VA.png", device = png)


#Figure 5F: diff Zn CV
plt_diff_shape_cv_zn <- tm_shape(diff_dam_cv) +
  tm_polygons("diff_cv_Zn", 
              style="cont", 
              breaks = c(-50,-40, -30, -20, -10, 0, 10, 20, 30, 40, 50), 
              title="",
              palette = "RdBu",
              #n= 10,
              midpoint = NA,
              textNA = "\u2264 1 species",
              colorNA = "grey",
              border.alpha = 0.025) +
  
  tm_layout(legend.show = F,
            main.title = "G",
            main.title.position = c('left', 'top'),
            frame = FALSE,
            legend.width = 10,
            legend.text.size = 1.5,
            legend.position  = c('right', 'top'),
            #legend.format = list(fun = function (x) {ifelse(x %in% c(-50,-40, -30, -20, -10, 0, 10, 20, 30, 40, 50), x, "")}),
            legend.height = 1)

#save
#tmap_save(tm = plt_diff_shape_cv_zn, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/cv/diff_cv_zn.png", 
#          device = png)


#legend map arranged difference map of CV-values in the subbasins
legend_spatial_cv_fut_map <- tm_shape(diff_dam_cv) +
  tm_polygons("diff_cv_PRO", 
              style="cont", 
              breaks = seq(-50,50, 10), 
              title="Change in CV (%)",
              palette = "RdBu",
              #n= 10,
              midpoint = NA,
              textNA = "\u2264 1 species",
              colorNA = "grey",
              border.alpha = 0.025) +
  
  tm_layout(legend.only = T, 
            scale=3, 
            asp=0,
            main.title.position = c('left', 'top'),
            main.title.size = 3,
            legend.width = 15,
            legend.text.size = 0.86,
            legend.position  = c('left', 'top'),
            legend.height = 3
            #legend.format = list(fun = function (x) {ifelse(x %in% c(0, 10, 90, 180), x, "")})
  )


#arrange map
spatial_fut_cv_plot_Ca_Fe_PRO_Se_VA_Zn <- tmap_arrange(plt_cu_fu_dams_NR_CV,plt_diff_shape_cv_Ca, plt_diff_shape_cv_iro, plt_diff_shape_cv_PRO,
                                                       plt_diff_shape_cv_sel, plt_diff_shape_cv_VA, plt_diff_shape_cv_zn, legend_spatial_cv_fut_map,
                                                       ncol = 4, nrow = 2) 
#save
#tmap_save(tm = spatial_fut_cv_plot_Ca_Fe_PRO_Se_VA_Zn, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/cv/fut_arrange_all.png", 
 #          device = png,
  #         dpi = 300,
   #        height = 2500,
    #       width = 2250)

