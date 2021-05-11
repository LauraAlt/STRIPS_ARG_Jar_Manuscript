library("readxl")
library("ggplot2")
library("tidyr")
library("scales")
theme_set(theme_classic())

source("summarySE.R")
source("mylog_trans.R")

data <- read_excel("Figure_1_data.xlsx", sheet = "Day0_Manure")

data.long <- pivot_longer(data = data, 
                             cols = -c(Sample, Site, Treatment), 
                             names_to = "Primer", 
                             values_to = "Copy_Number")

SE <- summarySE(data.long, measurevar="Copy_Number", groupvars=c("Treatment", "Primer"))

levels_primer = ordered(c("ermB", "ermF", "sul1", "sul2", "tet44", "tetM"))
levels_treatment = ordered(c("Manure", "Crop", "Strip"))

figure1 <- ggplot(SE, aes(x = ordered(Primer, levels = levels_primer), y = Copy_Number, 
                                 fill = ordered(Treatment, levels = levels_treatment))) + 
  geom_bar(stat = "identity", color = "black", position = position_dodge(width = 0.75), width = 0.6) +
  geom_errorbar(aes(ymin = Copy_Number - se, ymax = Copy_Number + se), width = 0.2,
                position = position_dodge(width = 0.75)) + 
  scale_y_continuous(trans = mylog_trans(base = 10, from = -5),
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x))) +
  scale_x_discrete(labels = c("ermB" = "ermB", "ermF" = "ermF", "sul1" = "sul1",
                              "sul2" = "sul2", "tet44" = "tet44", "tetM" = "tetM"))  +
  ylab(bquote("Gene Copies 16S rRNA"~Copies^-1)) +
  scale_fill_manual(labels = c("Manure", "Control Field Crop Soil", "Control Prairie Strip Soil"), 
                    values=c('#A9A9A9', '#8C510A', '#01665E')) +
  theme(plot.title = element_text(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(family = "Times New Roman",
                                   face = c("italic", "italic", "italic", "italic", "italic",
                                            "italic"), size = 16, angle = 45, hjust = 1),
        axis.title.y = element_text(family = "Times New Roman", size = 18),
        axis.text.y = element_text(family = "Times New Roman", size = 16),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(family = "Times New Roman", size = 18))

figure1

ggsave("Figure_1.tiff", plot = figure1,
       width = 10, height = 4.8, units = c("in"))
