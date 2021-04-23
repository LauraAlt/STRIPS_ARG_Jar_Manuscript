library("readxl")
library("ggplot2")
library("tidyr")
theme_set(theme_bw())

source("summarySE.R")

data <- read_excel("figure_s2_data.xlsx")

data <- subset(data, Day == "0")

levels = ordered(c('Control Crop', 'Treated Crop', 'Control Strip', 'Treated Strip'))

data.long <- pivot_longer(data = data, 
                          cols = -c(Sample, Site, Site_figure, Vegetation, Treatment, Replicate, Rep, Day), 
                          names_to = "Primer", 
                          values_to = "Relative_Abundance")

manured.data.long <- subset(data.long, Treatment == "Manure")
non.manured.data.long <- subset(data.long, Treatment =="No Manure")

SE <- summarySE(data.long, measurevar="Relative_Abundance", groupvars=c("Site", "Site_figure", "Vegetation","Treatment",
                                                                        "Replicate", "Day", "Primer"))

SE$Site_figure1 <- factor(SE$Site_figure, levels = c("Neal Smith", "INH", "WOR"))

figure <- ggplot(SE, aes(x = as.factor(Day), y = Relative_Abundance, fill = ordered(Replicate, levels = levels))) + 
  geom_bar(stat = "identity", color = "black", position = position_dodge()) +
  facet_grid(rows = vars(Primer), cols = vars(Site_figure1), scales = "free") +
  geom_errorbar(aes(ymin = Relative_Abundance - se, ymax = Relative_Abundance + se), width=.2,
                position=position_dodge(.9)) +
  xlab("Sample Day") +
  ylab(bquote("ARG Copies 16S rRNA"~Copies^-1)) +
  theme(legend.title = element_blank(), legend.position = "top",
        axis.title.x = element_text(family = "Times New Roman", size = 16),
        axis.text.x = element_text(family = "Times New Roman", size = 14),
        axis.title.y = element_text(family = "Times New Roman", size = 16),
        axis.text.y = element_text(family = "Times New Roman", size = 11),
        legend.text = element_text(family = "Times New Roman", size = 12),
        strip.text.x = element_text(family = "Times New Roman", size = 16),
        strip.text.y = element_text(family = "Times New Roman", face = "italic", size = 16)) +
  scale_fill_manual(values=c('#BF812D', '#8C510A', '#35978F', '#01665E'))

figure

ggsave("figure_s2.tiff", plot = figure,
       width = 6, height = 8, dpi = 500, units = c("in"))



