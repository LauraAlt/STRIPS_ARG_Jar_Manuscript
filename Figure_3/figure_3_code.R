library("readxl")
library("emmeans")
library("ggplot2")
theme_set(theme_bw())

#Create Plots for ermB, ermF, sul2, tet44, and tetM by changing gene name

data <- read_xlsx("figure_3_data.xlsx", sheet = "Sheet1")
data$Site <- factor(data$Site, levels = c("Neal Smith", "INH", "WOR"))
data$Vegetation <- factor(data$Vegetation, levels = c("Strip", "Crop"))

mod_ermB <- lm(log(ermB_rel) ~ Day + Vegetation + Site + Day:Vegetation + Day:Site + Site:Vegetation + Day:Site:Vegetation,
               data = data)

em <- emtrends(mod_ermB, pairwise ~ Vegetation | Site, var = "Day")

tiff(file = "ermB.tiff", width = 7, height = 4, units = "in", res = 500)

plot(em, ylab = "", xlab = "K-Value"~(d^-1)~"with 95% Confidence Intervals", colors = c("black", "darkgreen")) +
  facet_grid(rows = vars(Site)) +
  xlim(-0.18, -0.03) +
  theme(axis.title.x = element_text(family = "Times New Roman", size = 16),
        axis.title.y = element_text(family = "Times New Roman", size = 16),
        axis.text.x = element_text(family = "Times New Roman", size = 16),
        axis.text.y = element_text(family = "Times New Roman", size = 16),
        strip.text.y = element_text(family = "Times New Roman", size = 15))
  
dev.off()
