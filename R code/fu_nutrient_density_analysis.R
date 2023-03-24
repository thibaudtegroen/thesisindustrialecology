
source("C:/Industrial Ecology/Jaar 3/Masterscriptie/Databases/R-script/main.R")

tmap_options_reset() #reset tmap options when you have ran other files before
sf::sf_use_s2(FALSE) #spherical geometry switched off
#ttm() #turn on to create interactive plots

#Analysis: future status of nutrient density values across subbasins in the Mekong Basin ----------------------------------------------------------------

#boxplot df current and future  NR1 and NR6
df_cu_fu_boxplot_NR1 <- melt(data.frame(
  Ca = cur_dam_adult_mean_hb_df$cu_mean_NRn_adult_calcium,
  "change.in.Ca" = df_difference_NR_mean$diff_NR_Ca,
  Fe = cur_dam_adult_mean_hb_df$cu_mean_NRn_adult_iron,
  "change in Fe" = df_difference_NR_mean$diff_NR_Fe,
  PRO = cur_dam_adult_mean_hb_df$cu_mean_NRn_adult_protein,
  "change in PRO" = df_difference_NR_mean$diff_NR_PRO,
  Se = cur_dam_adult_mean_hb_df$cu_mean_NRn_adult_selenium,
  "change in Se" = df_difference_NR_mean$diff_NR_Se,
  VA = cur_dam_adult_mean_hb_df$cu_mean_NRn_adult_Vitamin_A,
  "change in VA" = df_difference_NR_mean$diff_NR_VA,
  Zn = cur_dam_adult_mean_hb_df$cu_mean_NRn_adult_zinc,
  "change in Zn" = df_difference_NR_mean$diff_NR_Zn), 
  variable.name = "nutrient", value.name = "NR1"
)

#Table 7: summary cumulative density difference future
summary(diff_dam_NR$diff_NR_tot)

#outliers nutrient density future change
print(paste("The total outliers of Calcium NR1 in future scenario is", length(boxplot.stats(diff_dam_NR$diff_NR_Ca)$out)))
print(paste("The total outliers of Iron NR1 in future scenario is",length(boxplot.stats(diff_dam_NR$diff_NR_Fe)$out)))
print(paste("The total outliers of Protein NR1 in future scenario is",length(boxplot.stats(diff_dam_NR$diff_NR_PRO)$out)))
print(paste("The total outliers of Selenium NR1 in future scenario is",length(boxplot.stats(diff_dam_NR$diff_NR_Se)$out)))
print(paste("The total outliers of Vitamin A NR1 in future scenario is",length(boxplot.stats(diff_dam_NR$diff_NR_VA)$out)))
print(paste("The total outliers of Zinc NR1 in future scenario is",length(boxplot.stats(diff_dam_NR$diff_NR_Zn)$out)))
print(paste("The total outliers of NR6 in future scenario is",length(boxplot.stats(diff_dam_NR$diff_NR_tot)$out)))



#boxplot current and future NR1
cu_fu_boxplot_NR1 <- df_cu_fu_boxplot_NR1 %>%
  ggplot(aes(x = nutrient, 
             y = NR1, color = nutrient)) +
  geom_boxplot(aes(x = nutrient, y = NR1)) +
  coord_flip() + 
  scale_x_discrete(limits=rev) +
  scale_color_manual(name = "nutrient",
                     values = c("#404040", "#BABABA",
                                "#404040", "#BABABA",
                                "#404040", "#BABABA",
                                "#404040", "#BABABA",
                                "#404040", "#BABABA",
                                "#404040", "#BABABA"))+
  
  scale_y_continuous(expand = expansion(mult = c(0, .02)),
                     breaks = seq(from=-5, 
                                  to = 100, 
                                  by = 5),
                     limits = c(-5,100)) +
  ylab("%") +
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
    legend.position = "none",
    legend.background = element_rect(color = "black"),
    legend.text = element_text(size = 15),
    legend.margin = margin(t = 5, l = 5, r = 5, b = 5),
    legend.key = element_rect(color = NA, fill = NA))


#save NR1 boxplot
#ggsave(filename = "boxplot_cu_fu_NR1_plot_Ca_Fe_PRO_Se_VA_Zn.png", plot = cu_fu_boxplot_NR1,
      # path = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken",
      # device = png, width = 12, height = 8, dpi = 300)


#Spatial analysis: future status of nutrient density values across subbasins in the Mekong Basin ----------------------------------------------------------------

#Cumulative nutrient density 

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

#Figure 9C: cumulative density difference
plt_diff_shape_NR_tot <- tm_shape(diff_dam_NR) +
  tm_polygons("diff_NR_tot", 
              style="cont", 
              breaks = c(-80,-70, -60, -50, -40, -30, -20, -10, 0, 10, 20, 30, 40, 50, 60, 70, 80), 
              title=expression("Change in NR"[6] * "(%)"),
              palette = "RdBu",
              #n= 10,
              midpoint = NA,
              textNA = "\u2264 1 species",
              colorNA = "grey",
              border.alpha = 0.05) +
  
  tm_layout(legend.show = TRUE,
            legend.outside = T,
            scale = 3,
            title.size = 2,
            main.title = "C",
            main.title.position = c('left', 'top'),
            main.title.size = 1.5,
            legend.width = 10,
            legend.text.size = 1.5,
            frame = FALSE,
            legend.outside.position  = c('right', 'top'),
            legend.format = list(fun = function (x) {ifelse(x %in% c(-80,-70, -60, -50, -40, -30, -20, -10, 0, 10, 20, 30, 40, 50, 60, 70, 80), x, "")}),
            legend.height = 1)


#tmap_save(tm = plt_diff_shape_NR_tot, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/NR/diff_NR_tot.png", 
#  device = png,
#   width = 2100, height = 3300, dpi =300)


#turn on only when cu_nutrient_density_analysis.R have been ran
spatial_fut_density_CU_FU <- tmap_arrange(plt_cur_density_tot, plt_diff_shape_NR_tot, 
                                          ncol = 2, nrow = 1) 

#tmap_save(tm = spatial_fut_density_CU_FU, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/NR/diff_NR_tot.png", 
# device = png)


#Individual Densities
#only one figure is described (Figure 8B), since other figures are identical
#FIgure 8A: current and future dams
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

#Figure 8B: Diff NR Calcium
plt_diff_shape_NR_Ca <- tm_shape(diff_dam_NR) +
  tm_polygons("diff_NR_Ca", 
              style="cont", 
              breaks = seq(-35,35, 5), #range = -35% to +35%
              title="",
              palette = "BrBG", #brown to blue to green
              #n= 10,
              midpoint = NA,
              textNA = "\u2264 1 species",
              colorNA = "grey",
              border.alpha = 0.05) +
  
  tm_layout(legend.show = F,
            main.title = "B",
            main.title.size = 1,
            main.title.position = c('left', 'top'),
            frame = FALSE,
            legend.width = 10,
            legend.text.size = 1.5,
            legend.position  = c('right', 'top'),
            #legend.format = list(fun = function (x) {ifelse(x %in% c(-50,-40, -30, -20, -10, 0, 10, 20, 30, 40, 50), x, "")}),
            legend.height = 1)

#tmap_save(tm = plt_diff_shape_NR_Ca, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/NR/diff_NR_Ca.png", device = png)



#tmap_save(tm = plt_diff_shape_NR_Fe, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/cv/diff_NR_Fe.png", device = png)

#Figure 8C: Diff NR Fe
plt_diff_shape_NR_Fe <- tm_shape(diff_dam_NR) +
  tm_polygons("diff_NR_Fe", 
              style="cont", 
              breaks = seq(-35,35, 5), 
              title="",
              palette = "BrBG",
              n= 10,
              midpoint = NA,
              textNA = "\u2264 1 species",
              colorNA = "grey",
              border.alpha = 0.05) +
  
  tm_layout(legend.show = F,
            main.title = "C",
            main.title.size = 1,
            main.title.position = c('left', 'top'),
            frame = FALSE,
            legend.width = 10,
            legend.text.size = 1.5,
            legend.position  = c('right', 'top'),
            #legend.format = list(fun = function (x) {ifelse(x %in% c(-50,-40, -30, -20, -10, 0, 10, 20, 30, 40, 50), x, "")}),
            legend.height = 1)

#tmap_save(tm = plt_diff_shape_NR_Fe, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/cv/diff_NR_Fe.png", device = png)

#Figure 8D: Diff NR PRO
plt_diff_shape_NR_PRO <- tm_shape(diff_dam_NR) +
  tm_polygons("diff_NR_PRO", 
              style="cont", 
              breaks = seq(-35,35, 5), 
              title="",
              palette = "BrBG",
              n= 10,
              midpoint = NA,
              textNA = "\u2264 1 species",
              colorNA = "grey",
              border.alpha = 0.05) +
  
  tm_layout(legend.show = F,
            main.title = "D",
            main.title.size = 1,
            main.title.position = c('left', 'top'),
            frame = FALSE,
            legend.width = 10,
            legend.text.size = 1.5,
            legend.position  = c('right', 'top'),
            #legend.format = list(fun = function (x) {ifelse(x %in% c(-50,-40, -30, -20, -10, 0, 10, 20, 30, 40, 50), x, "")}),
            legend.height = 1)

#tmap_save(tm = plt_diff_shape_NR_PRO, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/NR/diff_NR_PRO.png", device = png)

#Figure 8E: Diff NR Se
plt_diff_shape_NR_Se <- tm_shape(diff_dam_NR) +
  tm_polygons("diff_NR_Se", 
              style="cont", 
              breaks = seq(-35,35, 5), 
              title="",
              palette = "BrBG",
              n= 10,
              midpoint = NA,
              textNA = "\u2264 1 species",
              colorNA = "grey",
              border.alpha = 0.05) +
  
  tm_layout(legend.show = F,
            main.title = "E",
            main.title.size = 1,
            main.title.position = c('left', 'top'),
            frame = FALSE,
            legend.width = 10,
            legend.text.size = 1.5,
            legend.position  = c('right', 'top'),
            #legend.format = list(fun = function (x) {ifelse(x %in% c(-50,-40, -30, -20, -10, 0, 10, 20, 30, 40, 50), x, "")}),
            legend.height = 1)

#tmap_save(tm = plt_diff_shape_NR_Se, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/NR/diff_NR_Se.png", device = png)

#Figure 8F: Diff NR VA
plt_diff_shape_NR_VA <- tm_shape(diff_dam_NR) +
  tm_polygons("diff_NR_VA", 
              style="cont", 
              breaks = seq(-35,35, 5), 
              title="Difference NR VA (%)",
              palette = "BrBG",
              n= 10,
              midpoint = NA,
              textNA = "\u2264 1 species",
              colorNA = "grey",
              border.alpha = 0.05) +
  
  tm_layout(legend.show = F,
            main.title = "F",
            main.title.size = 1,
            main.title.position = c('left', 'top'),
            frame = FALSE,
            legend.width = 10,
            legend.text.size = 1.5,
            legend.position  = c('right', 'top'),
            #legend.format = list(fun = function (x) {ifelse(x %in% c(-50,-40, -30, -20, -10, 0, 10, 20, 30, 40, 50), x, "")}),
            legend.height = 1)

#tmap_save(tm = plt_diff_shape_NR_VA, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/NR/diff_NR_VA.png", device = png)


#Figure 8G: Diff NR Zn
plt_diff_shape_NR_Zn <- tm_shape(diff_dam_NR) +
  tm_polygons("diff_NR_Zn", 
              style="cont", 
              breaks = seq(-35,35, 5), 
              title="",
              palette = "BrBG",
              n= 10,
              midpoint = NA,
              textNA = "\u2264 1 species",
              colorNA = "grey",
              border.alpha = 0.05) +
  
  tm_layout(legend.show = F,
            main.title = "G",
            main.title.size = 1,
            main.title.position = c('left', 'top'),
            frame = FALSE,
            legend.width = 10,
            legend.text.size = 1.5,
            legend.position  = c('right', 'top'),
            #legend.format = list(fun = function (x) {ifelse(x %in% c(-50,-40, -30, -20, -10, 0, 10, 20, 30, 40, 50), x, "")}),
            legend.height = 1)

#tmap_save(tm = plt_diff_shape_NR_Zn, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/NR/diff_NR_Zn.png", device = png)

#legend map difference map Individual Densities
legend_spatial_NR_fut_map <- tm_shape(diff_dam_NR) +
  tm_polygons("diff_NR_Zn", 
              style="cont", 
              breaks = seq(-35,35, 5), 
              title= expression("Change in NR"[1] * "(%)"),
              palette = "BrBG",
              #n= 10,
              midpoint = NA,
              textNA = "\u2264 1 species",
              colorNA = "grey",
              border.alpha = 0.05) +
  
  tm_layout(legend.only = T, 
            scale=3, 
            asp=0,
            main.title.position = c('left', 'top'),
            main.title.size = 3,
            legend.width = 15,
            legend.text.size = 1,
            legend.position  = c('left', 'top'),
            legend.height = 3
            #legend.format = list(fun = function (x) {ifelse(x %in% c(0, 10, 90, 180), x, "")})
  )


spatial_fut_density_plot_Ca_Fe_PRO_Se_VA_Zn <- tmap_arrange(plt_cu_fu_dams_NR_CV, plt_diff_shape_NR_Ca, plt_diff_shape_NR_Fe, plt_diff_shape_NR_PRO, 
                                                            plt_diff_shape_NR_Se, plt_diff_shape_NR_VA, plt_diff_shape_NR_Zn, legend_spatial_NR_fut_map,
                                                            ncol = 4, nrow = 2) 

#tmap_save(tm = spatial_fut_density_plot_Ca_Fe_PRO_Se_VA_Zn, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/cv/fut_density_arrange_all.png", 
   #       device = png)
