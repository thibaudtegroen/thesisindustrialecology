# Calculate correlation fish length and nutrient values

# Thibaud te Groen 

# oktober 2022
source("C:/Industrial Ecology/Jaar 3/Masterscriptie/Databases/R-script/main.R")
library(broom)
library(purrr)

#**Interaction: Species length x nutritional value**
#

#sp_nutr_unique <- sp_nutr[!duplicated(sp_nutr[,'fb_name']),] # only unique species 

#sp_cur_nutr_unique <- sp_nutr_unique %>% filter(hab_curdams == TRUE) #current
#sp_fut_nutr_unique <- sp_nutr_unique %>% filter(hab_futdams == TRUE)

#sp_nutr_unique_small <- sp_cur_nutr_unique[, c("fb_name", "Iron_mu",
      #                                         "Selenium_mu", "Zinc_mu", "Vitamin_A_mu", "Protein_mu","Calcium_mu", "Omega_3_mu")]


#sp_size <- species(sp_nutr_unique_small$fb_name, # vector of species names
                  # fields = c("Species", "Brack","Saltwater","Fresh","AnaCat","Length", "Importance"))
#colnames(sp_size) <- c("fb_name", "Brackish","Saltwater","Freshwater","Migration","Length", "Commercial")

#sp_size_nutr <- left_join(sp_nutr_unique_small, sp_size, by ="fb_name")

#import species_charact.csv
sp_size_nutr <- read.csv("C:/Industrial Ecology/Jaar 3/Masterscriptie/Databases/export files/species_charact.csv")

#Correlation analysis
#Fe concentration X Size

#Outliers
#Iron concentration
sp_size_nutr[which(sp_size_nutr$Iron_mu %in% c(boxplot.stats(sp_size_nutr$Iron_mu)$out)),]


#Size
sp_size_nutr[which(sp_size_nutr$Length %in% c(boxplot.stats(sp_size_nutr$Length)$out)),]


#remove outliers from dataframe
quartiles_Fe <- quantile(sp_size_nutr$Iron_mu, probs=c(.25, .75), na.rm = FALSE)
IQR_Fe <- IQR(sp_size_nutr$Iron_m)

Lower_Fe <- quartiles_Fe[1] - 1.5*IQR_Fe
Upper_Fe <- quartiles_Fe[2] + 1.5*IQR_Fe 


quartiles_size <- quantile(sp_size_nutr$Length, probs=c(.25, .75), na.rm = FALSE)
IQR_size <- IQR(sp_size_nutr$Length)
Lower_size <- quartiles_size[1] - 1.5*IQR_size
Upper_size <- quartiles_size[2] + 1.5*IQR_size


sp_size_nutr_Fe_no_out <- subset(sp_size_nutr, sp_size_nutr$Iron_mu > Lower_Fe & sp_size_nutr$Iron_mu < Upper_Fe)

sp_size_nutr_Fe_size_no_out <- subset(sp_size_nutr_Fe_no_out, sp_size_nutr_Fe_no_out$Length > Lower_size & sp_size_nutr_Fe_no_out$Length < Upper_size)

#Assumption 1: Are the data from each of the 2 variables (x, y) follow a normal distribution? 

#shapiro-wilk normality test 
shapiro.test(sp_size_nutr_Fe_size_no_out$Length)#NO, because n = 55, W=0.80, p = 3.44 * 10^-7
shapiro.test(sp_size_nutr_Fe_size_no_out$Iron_mu) #NO, because n = 55, W = 0.93, p = 0.0047

#qqplot
QQ_Fe_size <- ggqqplot(sp_size_nutr_Fe_size_no_out$Length, ylab = "Length") #NO - Spearman test
QQ_Fe <- ggqqplot(sp_size_nutr_Fe_size_no_out$Iron_mu, ylab = "Iron_mu") #YES 


#Assumption 2: Covariation linear?
Fe_scat_plt <- ggscatter(sp_size_nutr_Fe_size_no_out, x = "Length", y = "Iron_mu", 
                         add = "reg.line", conf.int = TRUE, 
                         cor.coef = TRUE, cor.method = "spearman",
                         xlab = "length (cm) ", ylab = "Fe concentration (mg/100g)") 

#spearman correlation Fe x size  #S = 42312, p-value = 3.66e-05, n=55
cor_Fe_size <- cor.test(sp_size_nutr_Fe_size_no_out$Iron_mu, sp_size_nutr_Fe_size_no_out$Length,method="spearman", exact = F)


#Ca CONTENT x SIZE
#Outliers
#Ca concentration
sp_size_nutr[which(sp_size_nutr$Calcium_mu %in% c(boxplot.stats(sp_size_nutr$Calcium_mu)$out)),]


#remove outliers from dataframe
quartiles_Ca <- quantile(sp_size_nutr$Calcium_mu, probs=c(.25, .75), na.rm = FALSE)
IQR_Ca <- IQR(sp_size_nutr$Calcium_mu)

Lower_Ca <- quartiles_Ca[1] - 1.5*IQR_Ca
Upper_Ca <- quartiles_Ca[2] + 1.5*IQR_Ca 



sp_size_nutr_Ca_no_out <- subset(sp_size_nutr, sp_size_nutr$Calcium_mu > Lower_Ca & sp_size_nutr$Calcium_mu < Upper_Ca)

sp_size_nutr_Ca_size_no_out <- subset(sp_size_nutr_Ca_no_out, sp_size_nutr_Ca_no_out$Length > Lower_size & sp_size_nutr_Ca_no_out$Length < Upper_size)

#Assumption 1: Are the data from each of the 2 variables (x, y) follow a normal distribution? 

#shapiro-wilk normality test 
shapiro.test(sp_size_nutr_Ca_size_no_out$Length)#NO, because n = 57, W=0.80, p = 3.037*10^-7
shapiro.test(sp_size_nutr_Ca_size_no_out$Calcium_mu) #NO, because n = 57, W = 0.92, p = 0.0016

#qqplot
QQ_Ca_size <- ggqqplot(sp_size_nutr_Ca_size_no_out$Length, ylab = "Length") #NO
QQ_Ca <- ggqqplot(sp_size_nutr_Ca_size_no_out$Calcium_mu, ylab = "Calcium_mu") #YES


#Assumption 2: Covariation linear? YES
Ca_scat_plt <- ggscatter(sp_size_nutr_Ca_size_no_out, x = "Length", y = "Calcium_mu", 
                         add = "reg.line", conf.int = TRUE, 
                         cor.coef = TRUE, cor.method = "spearman",
                         xlab = "length (cm) ", ylab = "Ca concentration (mg/100g)") 

#correlation Ca x size  #S = 47729, p-value = 1.077e-05, n=57
cor_Ca_size <- cor.test(sp_size_nutr_Ca_size_no_out$Calcium_mu, sp_size_nutr_Ca_size_no_out$Length,method="spearman", exact = F)

#PRO content x SIZE
sp_size_nutr[which(sp_size_nutr$Protein_mu %in% c(boxplot.stats(sp_size_nutr$Protein_mu)$out)),]


#remove outliers from dataframe
quartiles_PRO <- quantile(sp_size_nutr$Protein_mu, probs=c(.25, .75), na.rm = FALSE)
IQR_PRO <- IQR(sp_size_nutr$Protein_mu)

Lower_PRO <- quartiles_PRO[1] - 1.5*IQR_PRO
Upper_PRO <- quartiles_PRO[2] + 1.5*IQR_PRO 



sp_size_nutr_PRO_no_out <- subset(sp_size_nutr, sp_size_nutr$Protein_mu > Lower_PRO & sp_size_nutr$Protein_mu < Upper_PRO)

sp_size_nutr_PRO_size_no_out <- subset(sp_size_nutr_PRO_no_out, sp_size_nutr_PRO_no_out$Length > Lower_size & sp_size_nutr_PRO_no_out$Length < Upper_size)

#Assumption 1: Are the data from each of the 2 variables (x, y) follow a normal distribution? 

#shapiro-wilk normality test 
shapiro.test(sp_size_nutr_PRO_size_no_out$Length)#NO, because n = 55, W=0.80, p = 4.003e-07
shapiro.test(sp_size_nutr_PRO_size_no_out$Protein_mu) #YES, because n = 55, W = 0.98, p = 0.5112

#qqplot
QQ_PRO_size <- ggqqplot(sp_size_nutr_PRO_size_no_out$Length, ylab = "Length") #NO
QQ_PRO <- ggqqplot(sp_size_nutr_PRO_size_no_out$Protein_mu, ylab = "Protein_mu") #YES


#Assumption 2: Covariation linear? YES
PRO_scat_plt <- ggscatter(sp_size_nutr_PRO_size_no_out, x = "Length", y = "Protein_mu", 
                          #add = "reg.line",
                          conf.int = TRUE, 
                          cor.coef = TRUE, cor.method = "spearman",
                          xlab = "length (cm) ", ylab = "PRO concentration (g/100g)")

#spearman correlation PRO x size  #S = 33144, p-value = 0.1522, n=55
cor_PRO_size <- cor.test(sp_size_nutr_PRO_size_no_out$Protein_mu, sp_size_nutr_PRO_size_no_out$Length, method="spearman", exact = F)


#Se content x SIZE
sp_size_nutr[which(sp_size_nutr$Selenium_mu %in% c(boxplot.stats(sp_size_nutr$Selenium_mu)$out)),]


#remove outliers from dataframe
quartiles_Se <- quantile(sp_size_nutr$Selenium_mu, probs=c(.25, .75), na.rm = FALSE)
IQR_Se <- IQR(sp_size_nutr$Selenium_mu)

Lower_Se <- quartiles_Se[1] - 1.5*IQR_Se
Upper_Se <- quartiles_Se[2] + 1.5*IQR_Se 



sp_size_nutr_Se_no_out <- subset(sp_size_nutr, sp_size_nutr$Selenium_mu > Lower_Se & sp_size_nutr$Selenium_mu < Upper_Se)

sp_size_nutr_Se_size_no_out <- subset(sp_size_nutr_Se_no_out, sp_size_nutr_Se_no_out$Length > Lower_size & sp_size_nutr_Se_no_out$Length < Upper_size)

#Assumption 1: Are the data from each of the 2 variables (x, y) follow a normal distribution? 

#shapiro-wilk normality test 
shapiro.test(sp_size_nutr_Se_size_no_out$Length) #NO, because n = 53, W=0.78997, p = 2.873e-07
shapiro.test(sp_size_nutr_Se_size_no_out$Selenium_mu) #NO, because n = 53, W = 0.94865, p = 0.02355

#qqplot
QQ_Se_size <- ggqqplot(sp_size_nutr_Se_size_no_out$Length, ylab = "Length") #NO
QQ_Se <- ggqqplot(sp_size_nutr_Se_size_no_out$Selenium_mu, ylab = "Selenium_mu") #YES


#Assumption 2: Covariation linear? YES

#scatterplot n = 53
Se_scat_plt <- ggscatter(sp_size_nutr_Se_size_no_out, x = "Length", y = "Selenium_mu", 
                         add = "reg.line", conf.int = TRUE, 
                         cor.coef = TRUE, cor.method = "spearman",
                         xlab = "length (cm) ", ylab = "Se concentration (µg/100g)")

#spearman correlation Ca x size  #S = 12423, p-value = 0.000142, n=53
cor_Se_size <- cor.test(sp_size_nutr_Se_size_no_out$Selenium_mu, sp_size_nutr_Se_size_no_out$Length,method="spearman", exact = F)


#VA content x SIZE
sp_size_nutr[which(sp_size_nutr$Vitamin_A_mu %in% c(boxplot.stats(sp_size_nutr$Vitamin_A_mu)$out)),]


#remove outliers from dataframe
quartiles_VA <- quantile(sp_size_nutr$Vitamin_A_mu, probs=c(.25, .75), na.rm = F)
IQR_VA <- IQR(sp_size_nutr$Vitamin_A_mu)

Lower_VA <- quartiles_VA[1] - 1.5*IQR_VA
Upper_VA <- quartiles_VA[2] + 1.5*IQR_VA 



sp_size_nutr_VA_no_out <- subset(sp_size_nutr, sp_size_nutr$Vitamin_A_mu > Lower_VA & sp_size_nutr$Vitamin_A_mu < Upper_VA)

sp_size_nutr_VA_size_no_out <- subset(sp_size_nutr_VA_no_out, sp_size_nutr_VA_no_out$Length > Lower_size & sp_size_nutr_VA_no_out$Length < Upper_size)

#Assumption 1: Are the data from each of the 2 variables (x, y) follow a normal distribution? 

#shapiro-wilk normality test 
shapiro.test(sp_size_nutr_VA_size_no_out$Length) #NO, because n = 51, W=0.8251, p = 2.914e-06
shapiro.test(sp_size_nutr_VA_size_no_out$Vitamin_A_mu) #No, because n = 51, W = 0.92829, p = 0.004282

#qqplot
QQ_VA_size <- ggqqplot(sp_size_nutr_VA_size_no_out$Length, ylab = "Length") #NO
QQ_VA <- ggqqplot(sp_size_nutr_VA_size_no_out$Vitamin_A_mu, ylab = "Vitamin_A_mu") #YES


#Assumption 2: Covariation linear? YES

#scatterplot n = 51
VA_scat_plt <- ggscatter(sp_size_nutr_VA_size_no_out, x = "Length", y = "Vitamin_A_mu", 
                         #add = "reg.line", 
                         conf.int = TRUE, 
                         cor.coef = TRUE, cor.method = "spearman",
                         xlab = "length (cm) ", ylab = "VA concentration (µg/100g)")

#spearman correlation VA x size  #S = 27989, p-value = 0.05873, n=51
cor_VA_size <- cor.test(sp_size_nutr_VA_size_no_out$Vitamin_A_mu, sp_size_nutr_VA_size_no_out$Length, method="spearman", exact = F)


#Zn content x SIZE
sp_size_nutr[which(sp_size_nutr$Zinc_mu %in% c(boxplot.stats(sp_size_nutr$Zinc_mu)$out)),]


#remove outliers from dataframe
quartiles_Zn <- quantile(sp_size_nutr$Zinc_mu, probs=c(.25, .75), na.rm = F)
IQR_Zn <- IQR(sp_size_nutr$Zinc_mu)

Lower_Zn <- quartiles_Zn[1] - 1.5*IQR_Zn
Upper_Zn <- quartiles_Zn[2] + 1.5*IQR_Zn 



sp_size_nutr_Zn_no_out <- subset(sp_size_nutr, sp_size_nutr$Zinc_mu > Lower_Zn & sp_size_nutr$Zinc_mu < Upper_Zn)

sp_size_nutr_Zn_size_no_out <- subset(sp_size_nutr_Zn_no_out, sp_size_nutr_Zn_no_out$Length > Lower_size & sp_size_nutr_Zn_no_out$Length < Upper_size)

#Assumption 1: Are the data from each of the 2 variables (x, y) follow a normal distribution? 

#shapiro-wilk normality test 
shapiro.test(sp_size_nutr_Zn_size_no_out$Length) #NO, because n = 56, W=0.80103, p = 2.968e-07
shapiro.test(sp_size_nutr_Zn_size_no_out$Zinc_mu) #No, because n = 56, W = 0.96433, p = 0.09629

#qqplot
QQ_Zn_size <- ggqqplot(sp_size_nutr_Zn_size_no_out$Length, ylab = "Length") #NO
QQ_Zn <- ggqqplot(sp_size_nutr_Zn_size_no_out$Zinc_mu, ylab = "Zinc_mu") #YES


#Assumption 2: Covariation linear? YES

#scatterplot n = 51
Zn_scat_plt <- ggscatter(sp_size_nutr_Zn_size_no_out, x = "Length", y = "Zinc_mu", 
                         add = "reg.line", conf.int = TRUE, 
                         cor.coef = TRUE, cor.method = "spearman",
                         xlab = "length (cm) ", ylab = "Zn concentration (µg/100g)")

#spearman correlation Zn x size  #S = 47335, p-value = 3.94e-07, n = 56
cor_Zn_size <- cor.test(sp_size_nutr_Zn_size_no_out$Zinc_mu, sp_size_nutr_Zn_size_no_out$Length,method="spearman", exact = F)


#arrange QQ plots and Scatterplots
QQplot_size_Ca_Fe_PRO_Se_VA_Zn <- ggarrange(QQ_Ca_size, QQ_Ca, QQ_Fe_size,  QQ_Fe, 
                                            QQ_PRO_size, QQ_PRO, QQ_Ca_size, QQ_Se, 
                                            QQ_VA_size, QQ_VA, QQ_Zn_size, QQ_Zn,
                                            labels = c("A","B","C","D",
                                                       "E","F","G","H",
                                                       "I", "J","K","L"),
                                            ncol = 4, nrow =3)

#list all correlations in list

nutrients_size_cor <- list(
  Calcium = cor_Zn_size,
  Iron = cor_Fe_size,
  Protein = cor_PRO_size,
  Selenium = cor_Se_size,
  Vitamin_A = cor_VA_size,
  Zinc = cor_Zn_size
)

nutrients_size_cor_df <- data.frame(map_dfr(nutrients_size_cor, tidy, .id = 'nutrient'))

#all scatterplots where PRO and VA because non-significant
scat_plot_Ca_Fe_PRO_Se_VA_Zn <- ggarrange(Ca_scat_plt, Fe_scat_plt, PRO_scat_plt,
                                          Se_scat_plt, VA_scat_plt, Zn_scat_plt,
                                          labels = c("A","B","C",
                                                     "D", "E","F"),
                                          ncol = 3, nrow =2)




#save
#ggsave(filename = "QQ_plot_size_Ca_Fe_PRO_Se_VA_Zn.png", plot = QQplot_size_Ca_Fe_PRO_Se_VA_Zn,
      # path = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken",
      # device = png, width = 10, height = 7, dpi = 300)
#ggsave(filename = "scat_plot_Ca_Fe_PRO_Se_VA_Zn.png", plot = scat_plot_Ca_Fe_PRO_Se_VA_Zn,
     #  path = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken",
     #  device = png, width = 10, height = 7, dpi = 300)