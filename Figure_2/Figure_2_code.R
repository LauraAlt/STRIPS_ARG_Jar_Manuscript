library("readxl")
library("ggplot2")
library("tidyr")
library("scales")
theme_set(theme_bw())

source("summarySE.R")

data <- read_xlsx("Figure_2_data.xlsx", sheet = "36 Day")

data.long <- pivot_longer(data = data, 
                          cols = -c(Sample, Site, Site_figure, Vegetation, Treatment, Replicate, Rep, Day), 
                          names_to = "Primer", 
                          values_to = "Relative_Abundance")

manured.data.long <- subset(data.long, Treatment == "Manure")
non.manured.data.long <- subset(data.long, Treatment =="No Manure")

SE <- summarySE(data.long, measurevar="Relative_Abundance", groupvars=c("Site", "Site_figure", "Vegetation","Treatment", "Replicate", "Day", "Primer"))

SE$Site_figure1 <- factor(SE$Site_figure, levels = c("Neal Smith", "INH", "WOR"))

figure <- ggplot(SE, aes(x = Day, y = Relative_Abundance, color = Replicate)) + 
  facet_grid(rows = vars(Primer), cols = vars(Site_figure1), scales = "free") +
  geom_point(aes(shape = Replicate), size = 3.5) +
  geom_errorbar(aes(ymin = Relative_Abundance - se, ymax = Relative_Abundance + se), width = 2) +
  xlab("Sample Day") +
  ylab(bquote("Gene Copies 16S rRNA"~Copies^-1)) +
  theme(legend.position = "top", 
        legend.title = element_blank(),
        legend.text = element_text(family = "Times New Roman", size = 14),
        axis.text.x = element_text(family = "Times New Roman", size = 14),
        axis.text.y = element_text(family = "Times New Roman", size = 14),
        axis.title.x = element_text(family = "Times New Roman", size = 16),
        axis.title.y = element_text(family = "Times New Roman", size = 16),
        strip.text.x = element_text(family = "Times New Roman", size = 16),
        strip.text.y = element_text(family = "Times New Roman", size = 16, face = "italic")) +
  scale_color_manual(values=c('black', 'gray55', '#8C510A','#01665E')) +
  scale_shape_manual(values = c(17, 17, 16, 16)) +
  scale_y_continuous(trans = log_trans(), breaks = trans_breaks("log", function(x) exp(x)),
                     labels = trans_format("log", math_format(e^.x)))


figure

ggsave("figure_2.tiff", plot = figure,
       width = 10, height = 8, units = c("in"))
