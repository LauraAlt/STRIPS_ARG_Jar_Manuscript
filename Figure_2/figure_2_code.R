library("readxl")
library("ggplot2")
library("tidyr")
theme_set(theme_bw())

source("summarySE.R")

data <- read_xlsx("figure_2_data.xlsx", sheet = "Sheet1")

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
  geom_line(size = 0.5) +
  geom_point(aes(shape = Replicate), size = 2) +
  xlab("Sample Day") +
  ylab(bquote("Gene Copies 16S rRNA"~Copies^-1)) +
  theme(legend.position = "top", 
        legend.title = element_blank(),
        legend.text = element_text(family = "Times New Roman", size = 14),
        axis.text.x = element_text(family = "Times New Roman", size = 14),
        axis.text.y = element_text(family = "Times New Roman", size = 11),
        axis.title.x = element_text(family = "Times New Roman", size = 16),
        axis.title.y = element_text(family = "Times New Roman", size = 16),
        strip.text.x = element_text(family = "Times New Roman", size = 16),
        strip.text.y = element_text(family = "Times New Roman", size = 16, face = "italic")) +
  scale_color_manual(values=c('black', 'black', '#8C510A','#01665E'))


figure

ggsave("figure_2.tiff", plot = figure,
       width = 10, height = 8, dpi = 500, units = c("in"))
