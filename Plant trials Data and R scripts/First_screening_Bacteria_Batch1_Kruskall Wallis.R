#Analysis 1:First screening Disease suppresion vsP.infestans
#1. Batch 1
#12.10.25

#Load libraries
library(readxl)
library(dplyr)
library(tidyr)
library(ggpubr)
library(moments)
library(rstatix)
library(broom)
library(psych)
library(FSA)
library(rcompanion) #Dependency: "DescTools"


#Read data
library(readxl)
BDS_Firstscreening <- read_excel("~/Ad_Planta_Trials/Raw_data/Firstscreening_P.infestans_Batch1.xlsx")
#View(BDS_Firstscreening)
#remove unnecessary columns
BDS_FS_Batch1 <- BDS_Firstscreening [,c(1:4)]

#Find all missing rel. inf. values
# This converts all non.numeric rel. inf. values to NA and returns the respective row number
x_nonum <- which(is.na(as.numeric(BDS_FS_Batch1$Diseased_leaf_area)))

BDS_FS_Batch1[x_nonum,]

#Cross_validation_in_vitro_vs_ad_planta_3h_120824_new <- Cross_validation_in_vitro_vs_ad_planta_3h_120824[-x_nonum,]

#Column rel_inf_area is not numeric -> convert
BDS_FS_Batch1$Diseased_leaf_area <- as.numeric(BDS_FS_Batch1$Diseased_leaf_area)

#Compute summary statistics:
#1. calculate the mean for each individual plant
#2. Calculate the parameters by groups - mean, sd, var, count: 

#1.
df_mean_BDS_FS_Batch1 <- BDS_FS_Batch1 %>%
  group_by(Treatments, Plant) %>% summarize(Diseased_leaf_area = mean(Diseased_leaf_area, na.rm = TRUE))

### If nececary, rename them
#library(dplyr)

df_mean_BDS_FS_Batch1$Treatments %>% unique()

df_mean_BDS_FS_Batch1$Treatments <- recode(df_mean_BDS_FS_Batch1$Treatments,
                                           "Pb575" = 'Acidovorax sp._Pb575', 
                                           "Pb16" = 'Pseudomonas sp._Pb16', 
                                           "Pb29" = 'Pseudomonas sp._Pb29',
                                            "Pb114" = 'Pseudomonas sp._Pb114',
                                           "Pb170" = 'Pseudomonas sp._Pb170',
                                           "Pb271" = "Pseudomonas sp._Pb271",
                                           "Pb285" = 'Pseudomonas sp._Pb285',
                                            "Pb457" = "Pseudomonas sp._Pb457",
                                           "Pb460" = "Pseudomonas sp._Pb460",
                                           "Pb521" = "Pseudomonas sp._Pb521",
                                           "Cs" = "Copper standard",
                                            "Pc" = "Diseased control",
                                            "Nc" = "Healthy control",
                                           "Pb109" = "Bacillus sp._Pb109",
                                           "Pb168" = "Bacillus sp._Pb168",
                                           "Pb355" = "Paenibacillus sp._Pb355",
                                           "Pb39" = "Streptomyces sp._Pb39",
                                           "Pb242" = 'Rhizobium sp._Pb242',
                                           "Pb539" = 'Microbacterium sp._Pb539',
                                            "Pb177" = 'Pseudarthrobacter sp._Pb177')
### Reorder them
df_mean_BDS_FS_Batch1$Treatments <- factor(df_mean_BDS_FS_Batch1$Treatments, 
                                            levels = c('Acidovorax sp._Pb575',
                                                      'Pseudomonas sp._Pb16', 
                                                        'Pseudomonas sp._Pb29',
                                                       'Pseudomonas sp._Pb114',
                                                      'Pseudomonas sp._Pb170',
                                                       'Pseudomonas sp._Pb271',
                                                        'Pseudomonas sp._Pb285',
                                                        'Pseudomonas sp._Pb457',
                                                        'Pseudomonas sp._Pb460',
                                                      'Pseudomonas sp._Pb521',
                                                        'Bacillus sp._Pb109',
                                                       'Bacillus sp._Pb168',
                                                      'Paenibacillus sp._Pb355',
                                                        'Streptomyces sp._Pb39',
                                                        'Rhizobium sp._Pb242',
                                                        'Microbacterium sp._Pb539',
                                                        'Pseudarthrobacter sp._Pb177',
                                                        'Copper standard',
                                                        'Diseased control',
                                                        'Healthy control'))
unique(df_mean_BDS_FS_Batch1$Treatments)
#Use to order the table according to the factor levels
df_mean_BDS_FS_Batch1 <- with(df_mean_BDS_FS_Batch1, df_mean_BDS_FS_Batch1[order(Treatments),])


Summarydata <- df_mean_BDS_FS_Batch1 %>%
  group_by(Treatments) %>%
 summarise(mean=mean(Diseased_leaf_area, na.rm = TRUE),
           median=median(Diseased_leaf_area, na.rm = TRUE),
           min=min(Diseased_leaf_area, na.rm = TRUE),
           max=max(Diseased_leaf_area, na.rm = TRUE),
           sd=sd(Diseased_leaf_area, na.rm = TRUE),
           var=var(Diseased_leaf_area, na.rm = TRUE),
           se=se(Diseased_leaf_area, na.rm = TRUE),
          #rel_prot_eff=(1-(median(Diseased_leaf_area, na.rm = TRUE)/median_H2O_P_120824))*100,
           plants =sum(!is.na(Diseased_leaf_area)))

#library(xlsx)
library(openxlsx)
# read the help file to identify the arguments needed to 
# correctly read the file

Alldatalist_BDS_FS_Batch1 <- list("Raw data" = BDS_FS_Batch1,
                                "Mean per plant" = df_mean_BDS_FS_Batch1, 
                               "Statistics" = Summarydata)
write.xlsx(Alldatalist_BDS_FS_Batch1, file = "Excelfile_Alldatalist_BDS_FS_Batch1.xlsx")

getwd()
#Create df1: Remove the water control

#df1_120824 <- df_mean_by_plant_120824[!(df_mean_by_plant_120824$Treatment == 'CON- \n 0h ad'),]

#df1_120824$inf.area <- df1_120824$rel_inf_area

#####################################
#Choosing the right test for analysis
#1. Kruskal-Wallis Test
# Based on: https://statisticseasily.com/kruskal-wallis-test/
# non-parametric method for comparing medians across multiple independent groups
# Non-Normal Data Distributions: 
# When the data is not normal distributed, especially with small sample sizes where the Central Limit Theorem does not apply, the Kruskal-Wallis Test provides a reliable alternative.
# Heterogeneous Variances: 
# When the groups have different variances, the Kruskal-Wallis Test can still be applied.
# Small Sample Sizes: 
# When sample sizes are too small to check the assumptions of parametric tests reliably, the Kruskal-Wallis Test can be a more suitable choice.

#1.1 Check for outliers

df_mean_BDS_FS_Batch1 %>% 
  group_by(Treatments, Plant) %>%
  identify_outliers(Diseased_leaf_area)

#No extreme outliers
#No need for this step. (Skip: #Remove outliers)
#Go to "Analysis for non-normal data"

#d <- (df1_120824 %>% 
       # group_by(Treatment) %>%
       # identify_outliers(inf.area))[c(1,2),1:4]

#Remove outliers

#Spore_germination_091024_grouped_24h_noOL <- anti_join(Spore_germination_091024_grouped_24h, d, by=c('Treatment', 'dh', 'Germinated', 'Ungerminated', 'Rel.Germ'))

#####################

#No outliers detected

##########Analysis for non-normal data###########
library(psych)
describeBy(df_mean_BDS_FS_Batch1$Diseased_leaf_area,df_mean_BDS_FS_Batch1$Treatments)

# Is there a difference in medians across the multiple independent groups?
# Kruskal-wallis test
# H0 = The means of all compared groups do not differe.
#https://bjoernwalther.com/kruskal-wallis-test-in-r-rechnen/
kruskal.test(df_mean_BDS_FS_Batch1$Diseased_leaf_area~df_mean_BDS_FS_Batch1$Treatments)

# p-value = 4.793e-07 < 0.05 
# The medians of the groups differ.

# Which of the groups do significantly differ?
##pairwise Dunn's Test
###This test makes pairwise comparisons among the groups.
#### This increases the probability of an alpha-error.
#### --> The p-value needs to be adjusted.

library(rstatix)
library(FSA)

DT <- dunnTest(Diseased_leaf_area ~ Treatments,
               data=df_mean_BDS_FS_Batch1,
               method="bh") 
install.packages("rcompanion")
library(rcompanion)
install.packages("rcompanion", dependencies = TRUE)
install.packages("DescTools", configure.args = "--CXXFLAGS='-std=c++17'")

download.packages("DescTools", destdir = ".", type = "source")
install.packages("DescTools", type = "binary")

cldList(P.adj ~ Comparison,
        data = DT$res,
        threshold = 0.05)
####################

# Create a vector of colors, mapping the custom ones and setting the default for all others
custom_colors <- c(
  "Diseased control" = "#C62828",  # Red for Diseased Control
  "Copper standard" = "#1F78B4",   # Blue for Copper Standard
  "Healthy control" = "#2E7D32"    # Green for Healthy Control
) 

treatments_list <- unique(df_mean_BDS_FS_Batch1$Treatments)
all_colors <- setNames(rep("gray80", length(treatments_list)), treatments_list)

all_colors[names(custom_colors)] <- custom_colors


install.packages("ggplot2")
library(ggplot2)

#change x axis labels vertically
z<-df_mean_BDS_FS_Batch1 %>% 
  group_by(Treatments) %>%
  ggplot(aes(x = Treatments, y = `Diseased_leaf_area`)) +
  geom_boxplot(aes(fill = Treatments), 
               alpha = 0.7, 
               outlier.shape = NA) + # Hide outliers since we plot the points
  geom_point(aes(color = "black"), 
             position = position_jitter(width = 0.2), 
             size = 1.5, 
             alpha = 0.6) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), color = "black", fill = NA) + # Enclosing rectangle
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10), expand = c(0, 0)) +
  scale_fill_manual(values = all_colors) +
  scale_color_manual(values = all_colors) + # Apply colors to points
  theme_classic() +
  labs(x = "Treatment") +
  labs(y = "Diseased leaf area (%)") +
  xlab("Treatment") +
  ylab("Diseased leaf area (%)") +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 12),  # Reduce x-axis title font size
    axis.title.y = element_text(size = 12),  # Reduce y-axis title font size
    axis.text.x = element_text(size = 10, face = "italic", angle = 90, hjust = 1, vjust = 0.5),  # Italicize and reduce font size for x-axis labels
    axis.text.y = element_text(size = 10),  # Reduce font size for y-axis labels
    panel.border = element_rect(color = "black", fill = NA),  # Panel border
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.line = element_blank(),  # Remove axis lines
    axis.ticks = element_blank(),
    legend.position = "none")  # Remove axis ticks


ggsave(filename = "plot2.png", plot = z, path = "~/Data/output.png", dpi = 300)

