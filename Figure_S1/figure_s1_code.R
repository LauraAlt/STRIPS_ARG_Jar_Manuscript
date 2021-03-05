library("readxl")
library("ggplot2")
library("scales")
theme_set(theme_bw())

source("summarySE.R")
source("fancy_scientific.R")
source("mylog_trans.R")

data <- read_excel("figure_s1_data.xlsx", sheet = "Sheet1")

SE <- summarySE(data, measurevar="16S_rRNA_Copy_Num", 
                groupvars=c("Site", "Site_figure", "Vegetation", "Legend", "Treatment", "Label", "Day"))

SE$Site_figure1 <- factor(SE$Site_figure, levels = c("Neal Smith", "INH", "WOR"))

figure <- ggplot(SE, aes(x = Day, y = `16S_rRNA_Copy_Num`, color = Legend)) + 
  facet_grid(rows = vars(Label), cols = vars(Site_figure1)) +
  geom_line(size = 1) +
  geom_point(size = 3, shape = 21, fill = "white") +
  geom_errorbar(aes(ymin = `16S_rRNA_Copy_Num` - se, ymax = `16S_rRNA_Copy_Num` + se), color="black",
                linetype = 1) +
  xlab("Sample Day") +
  ylab(bquote("16S rRNA Copies gram dry"~soil^-1)) +
  theme(legend.position = "top", legend.title = element_blank(),
        axis.title.x = element_text(family = "Times New Roman", size = 16),
        axis.text.x = element_text(family = "Times New Roman", size = 14),
        axis.title.y = element_text(family = "Times New Roman", size = 16),
        axis.text.y = element_text(family = "Times New Roman", size = 14),
        legend.text = element_text(family = "Times New Roman", size = 14),
        strip.text.x = element_text(family = "Times New Roman", size = 16),
        strip.text.y = element_text(family = "Times New Roman", size = 16)) +
  scale_color_manual(values=c('#8C510A', '#01665E')) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  )

figure

ggsave("figure_s1.tiff", plot = figure,
       width = 10, height = 6, dpi = 500, units = c("in"))
