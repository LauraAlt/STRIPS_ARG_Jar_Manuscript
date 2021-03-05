library("readxl")

data1 <- read_excel("half_lives_stats_data.xlsx", sheet = "Sheet1")

stats1 <- pairwise.wilcox.test(data1$Half_life,
                              data1$Gene)

stats_export1 <- as.data.frame(stats1$p.value)



data2 <- read_excel("16S_stats_data.xlsx", sheet = "Sheet1")

stats2 <- pairwise.wilcox.test(data2$`16S_rRNA_Copy_Num`,
                              data2$Comparison)

stats_export2 <- as.data.frame(stats2$p.value)
