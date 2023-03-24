source("C:/Industrial Ecology/Jaar 3/Masterscriptie/Databases/R-script/main.R")
source("C:/Industrial Ecology/Jaar 3/Masterscriptie/Databases/R-script/cu_nutrient_density_analysis.R")
source("C:/Industrial Ecology/Jaar 3/Masterscriptie/Databases/R-script/fu_nutrient_density_analysis.R")


#Analysis: current and future barplot distance status of NR1 and NR6 values across subbasins in the Mekong Basin ----------------------------------------------------------------


#create df (without sf) of current and future nutrient densities
cur_dam_adult_mean_hb_df <- data.frame(cur_dam_adult_mean_hb)
fut_dam_adult_mean_hb_df <- data.frame(fut_dam_adult_mean_hb)

#assign categories current
cur_dam_adult_mean_hb_df$cu_category <-  cut(cur_dam_adult_mean_hb_df$cu_mean_NRn_adult_tot,
                                             breaks = c(0, 100, 200, 300, 400, 500, 600),
                                             labels = c("0%-100%", "100%-200%","200%-300%", "300%-400%", "400%-500%", "500% - 600%"))

cur_dam_adult_mean_hb_df$time_period <- "current"

#assign categories future
fut_dam_adult_mean_hb_df$fu_category <- cut(fut_dam_adult_mean_hb_df$fu_mean_NRn_adult_tot,
                                            breaks = c(0, 100, 200, 300, 400, 500, 600),
                                            labels = c("0%-100%", "100%-200%","200%-300%", "300%-400%", "400%-500%", "500% - 600%"))
fut_dam_adult_mean_hb_df$time_period <- "future"

#merge rows for barplots
barplot_1 <- data.frame(HYBAS_ID_2x = c(cur_dam_adult_mean_hb_df[,"HYBAS_ID"], fut_dam_adult_mean_hb_df[,"HYBAS_ID"]), 
                        cu_fu_mean = c(cur_dam_adult_mean_hb_df[,"cu_mean_NRn_adult_tot"], fut_dam_adult_mean_hb_df[,"fu_mean_NRn_adult_tot"]),
                        cu_fu_time = c(cur_dam_adult_mean_hb_df[,"time_period"], fut_dam_adult_mean_hb_df[,"time_period"]),
                        cu_fu_classes = c(cur_dam_adult_mean_hb_df[,"cu_category"], fut_dam_adult_mean_hb_df[,"fu_category"]),
                        cu_fu_area = c(cur_dam_adult_mean_hb_df[,"SUB_AREA"], fut_dam_adult_mean_hb_df[,"SUB_AREA"]),
                        cu_fu_dist = c(cur_dam_adult_mean_hb_df[,"DIST_MAIN"], fut_dam_adult_mean_hb_df[,"DIST_MAIN"])
)
barplot_final <- barplot_1 %>% group_by(cu_fu_classes,cu_fu_time) %>%
  summarise(total_count = n(), total_area = sum (cu_fu_area)) 


bar_plot_100_200 <- data.frame (barplot_final %>% 
                                  dplyr::filter(cu_fu_classes %in% "100%-200%"))

bar_plot_200_300 <- data.frame (barplot_final %>% 
                                  dplyr::filter(cu_fu_classes %in% "200%-300%"))

bar_plot_300_400 <- data.frame (barplot_final %>% 
                                  dplyr::filter(cu_fu_classes %in% "300%-400%"))

bar_plot_100_200_plt <- bar_plot_100_200 %>% 
  ggplot(aes(x=cu_fu_classes,
             y= total_area,
             fill=cu_fu_time)) + 
  geom_col(width = .5, position = position_dodge(.6),
           color = "black") + 
  scale_fill_manual(values = c("#FAFAFA", "#D4D4D4", "#737373")) +
  #scale_y_log10(limits= c(1, 2e5),
  # expand = expansion(0))
  scale_y_continuous(
    breaks = seq(0, 150000, 15000),
    limits= c(0, 150000),
    expand = expansion(0))+
  labs(
    x = "Nutrient Density classes",
    y = bquote("Total Area log"~km^2),
    fill = NULL) +
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
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.ticks.x = element_blank(),
    legend.position = "none",
    legend.background = element_rect(color = "black"),
    legend.text = element_text(size = 15),
    legend.margin = margin(t = 5, l = 5, r = 5, b = 5),
    legend.key = element_rect(color = NA, fill = NA)
  ) +
  guides(
    fill = guide_legend(
      keywidth = 1.2,
      keyheight = 1.2,
      default.unit= "cm"
    )
  )

bar_plot_200_300_plt <- bar_plot_200_300 %>% 
  ggplot(aes(x=cu_fu_classes,
             y= total_area,
             fill=cu_fu_time)) + 
  geom_col(width = .5, position = position_dodge(.6),
           color = "black") + 
  scale_fill_manual(values = c("#FAFAFA", "#D4D4D4", "#737373")) +
  #scale_y_log10(limits= c(1, 2e6)) +
  #expand = expansion(0))+
  scale_y_continuous(breaks = seq(0, 650000, 65000),
                     limits= c(0, 650000),
                     expand = expansion(0))+
  labs(
    x = "Nutrient Density classes",
    y = bquote("Total Area log"~km^2),
    fill = NULL) +
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
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.ticks.x = element_blank(),
    legend.position = "none",
    legend.background = element_rect(color = "black"),
    legend.text = element_text(size = 15),
    legend.margin = margin(t = 5, l = 5, r = 5, b = 5),
    legend.key = element_rect(color = NA, fill = NA)
  ) +
  guides(
    fill = guide_legend(
      keywidth = 1.2,
      keyheight = 1.2,
      default.unit= "cm"
    )
  )

bar_plot_300_400_plt <- bar_plot_300_400 %>% 
  ggplot(aes(x=cu_fu_classes,
             y= total_area,
             fill=cu_fu_time)) + 
  geom_col(width = .5, position = position_dodge(.6),
           color = "black") + 
  scale_fill_manual(values = c("#FAFAFA", "#D4D4D4", "#737373")) +
  #scale_y_log10(limits= c(1, 2e5)) +
  # expand = expansion(0))+
  scale_y_continuous(
    breaks = seq(0, 7000, 1000),
    limits= c(0, 7000),
    expand = expansion(0))+
  labs(
    x = "Nutrient Density classes",
    y = bquote("Total Area log"~km^2),
    fill = NULL) +
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
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.ticks.x = element_blank(),
    legend.position = "none",
    legend.background = element_rect(color = "black"),
    legend.text = element_text(size = 15),
    legend.margin = margin(t = 5, l = 5, r = 5, b = 5),
    legend.key = element_rect(color = NA, fill = NA)
  ) +
  guides(
    fill = guide_legend(
      keywidth = 1.2,
      keyheight = 1.2,
      default.unit= "cm"
    )
  )


bar_plot_nutr_den_classes_area <- ggarrange(bar_plot_100_200_plt + rremove("ylab") + rremove("xlab"), 
                                            bar_plot_200_300_plt + rremove("ylab") + rremove("xlab"),
                                            bar_plot_300_400_plt + rremove("ylab") + rremove("xlab"),
                                            common.legend = TRUE, legend = "right",
                                            labels = c("A","",""),
                                            ncol = 3, nrow =1)
bar_plot_nutr_den_classes_area_an <- annotate_figure(bar_plot_nutr_den_classes_area,
                                                     bottom = text_grob("Nutrient density classes", color = "black", size = 22),
                                                     left = text_grob(expression("Total area (km"^2 * ")"), color = "black", size = 22, rot = 90))



#save barplot
#ggsave(filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/NR/barchart_apa.jpeg", 
#  plot = bar_plot_nutr_den_classes_area_an, 
#   width = 10, 
#  height = 6,
#  dpi = 300)