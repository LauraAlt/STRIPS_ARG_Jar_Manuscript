library("readxl")


copy_num <- read_excel("cumulative_copy_jar_manure.xlsx", sheet = "Sheet2")
rel <- read_excel("cumulative_rel_jar_manure.xlsx", sheet = "Day 0")

stats <- pairwise.wilcox.test(rel$sul1,
                              rel$Treatment)

stats_export <- as.data.frame(stats$p.value)
