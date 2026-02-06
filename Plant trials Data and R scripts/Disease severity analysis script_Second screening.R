#Analysis 1:Disease suppresionvsselectedbacteriavsP.infestans
#26.11.24

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
BDS_merged <- read_excel("~/Ad_Planta_Trials/Promising_plant_trials/SelectedbacteriavsPinfestans_merged.xlsx")
View(BDS_merged)

library(readxl)
library(dplyr)


df_raw<- read_excel("~/Ad_Planta_Trials/Promising_plant_trials/SelectedbacteriavsPinfestans_merged.xlsx") %>%
  rename(
    Treatment = Treatments,
    Rep = Replicate
  ) %>%
  mutate(
    Treatment = as.factor(Treatment),
    Rep = as.factor(Rep),
    Plant = as.factor(Plant),
    Leaf  = as.factor(Leaf),
    Diseased_leaf_area = as.numeric(Diseased_leaf_area)
  )


# sanity checks
table(df_raw$Rep)
table(df_raw$Treatment)
summary(df_raw$Diseased_leaf_area)

#Plant is the experimental unit. Because you treat a plant once, and the 3 leaves are subsamples (pseudo-replicates). So we first average leaves within plant:
#This gives you 6 plants per treatment per replicate (except where some treatments are missing in Rep3).

df_plant <- df_raw %>%
  group_by(Rep, Treatment, Plant) %>%
  summarise(severity = mean(Diseased_leaf_area, na.rm = TRUE), .groups = "drop")

#Use a mixed model across Rep1–3 (treat “Rep” as the independent run)
#Your response is a percentage with many small values and many zeros (healthy control is all zeros). Two robust choices:
# Choice 1: Gamma mixed model on (severity + small constant)
#Works well for right-skewed % data and avoids beta-model headaches with 0/100.

library(glmmTMB)

df_plant <- df_plant %>%
  mutate(severity_pos = severity + 0.5)  # small offset to allow zeros

m1 <- glmmTMB(
  severity_pos ~ Treatment + (1|Rep),
  family = Gamma(link = "log"),
  data = df_plant
)
summary(m1)

#make Diseased control the reference
df_plant <- df_plant %>%
  mutate(Treatment = relevel(Treatment, ref = "Diseased_control"))

m1 <- glmmTMB(
  severity_pos ~ Treatment + (1|Rep),
  family = Gamma(link="log"),
  data = df_plant
)

summary(m1)

library(emmeans)

emm <- emmeans(m1, ~ Treatment)

# Compare each treatment to diseased control (Dunnett-style)
contr <- contrast(emm, method = "trt.vs.ctrl", ref = "Diseased_control", adjust = "BH")
summary(contr)

#Convert effects to the response scale (for figures & text)Right now estimates are log-scale. Convert to fold reduction:
#This will give you: % reduction in disease severity, Much more intuitive numbers for readers

summary(contr, type = "response")

#Compare isolates to copper (benchmark)
contrast(emm, method = "trt.vs.ctrl", ref = "Copper_standard", adjust = "BH")

#Final sanity check (optional but recommended) Run model diagnostics:

install.packages("DHARMa")

library(DHARMa)

res <- simulateResiduals(m1)
plot(res)


plot(emm, comparisons = TRUE)


library(dplyr)
library(ggplot2)
library(emmeans)

# 1) Get treatment vs diseased-control contrasts on response scale (ratios)
contr_resp <- summary(
  contr,
  type = "response",
  infer = c(TRUE, TRUE)   # <-- this adds lower.CL and upper.CL
) %>%
  as.data.frame()

colnames(contr_resp)

# contr_resp typically contains: contrast, ratio, SE, z.ratio, p.value, lower.CL, upper.CL

# 2) Convert ratios to % reduction (and CI)
eff_df <- contr_resp %>%
  mutate(
    Treatment = sub(" / Diseased_control", "", contrast),
    
    # Effect as % reduction in disease vs diseased control
    perc_reduction = (1 - ratio) * 100,
    
    # CI for % reduction (invert CI on ratio scale)
    perc_low  = (1 - asymp.UCL) * 100,
    perc_high = (1 - asymp.LCL) * 100,
    
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ "ns"
    )
  ) %>%
  arrange(desc(perc_reduction)) %>%
  mutate(Treatment = factor(Treatment, levels = Treatment))

#Quick sanity check:
eff_df %>% select(Treatment, ratio, perc_reduction, perc_low, perc_high, p.value)


# 3) Plot % reduction with 95% CI error bars
library(dplyr)
library(ggplot2)

# eff_df comes from your earlier step using contr_resp (ratio + asymp.LCL/UCL)
# Optionally drop Healthy control from the reduction plot:

eff_plot <- eff_df %>%
  filter(Treatment != "Healthy_control") %>%
  mutate(Treatment = factor(Treatment, levels = Treatment))

p_reduction <- ggplot(eff_plot, aes(x = Treatment, y = perc_reduction)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = perc_low, ymax = perc_high), width = 0.2) +
  geom_text(aes(label = sig), vjust = -0.6, size = 4) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20), expand = c(0, 0)) +
  labs(x = "Treatment", y = "Disease reduction vs diseased control (%)") +
  theme_bw() +
  theme(
    axis.text.x  = element_text(size = 14, angle = 60, vjust = 1, hjust = 1, face = "italic"),
    axis.text.y  = element_text(size = 12),
    axis.title   = element_text(size = 14),
    panel.border = element_rect(color = "black", fill = NA)
  )

ggsave(
  filename = "BDSf_reduction_vs_control.tiff",
  plot = p_reduction,
  path = "~/Ad_Planta_Trials/Outputfiles/Plots",
  width = 7.5, height = 6.5, dpi = 300, units = "in",
  device = "tiff", compression = "lzw"
)







