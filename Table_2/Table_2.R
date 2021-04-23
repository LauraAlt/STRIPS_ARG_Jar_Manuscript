library("readxl")
library("plyr")
library("dplyr")
library("tidyr")

data <- read_xlsx("Table_2_data.xlsx", sheet= "Sheet1")
data$Site <- as.factor(data$Site)
data$Vegetation <- as.factor(data$Vegetation)

data_long <- pivot_longer(
  data, cols = c("tetM_rel", "tet44_rel", "ermF_rel", "ermB_rel", "sul2_rel"))

data_long$sort <- paste(data_long$Site, data_long$Vegetation, data_long$name, sep = "_")

model <- function(df) {
  lm(log(value) ~ Day, data = df)}

soil_models <- dlply(data_long, .(sort), model)

rsq <- function(a) summary(a)$r.squared
half_life <- function(b) log(2)/(-1*summary(b)$coefficients[2,1])
err <- function(c) 1*(summary(c)$coefficients[2,2])
lower_CI <- function(d) summary(d)$coefficients[2,1]+(-1*qt(0.975, 16)*summary(d)$coefficients[2,2])
upper_CI <- function(e) summary(e)$coefficients[2,1]+(1*qt(0.975, 16)*summary(e)$coefficients[2,2])
lmp <- function (modelobject) {
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

soil_coefs <- ldply(soil_models, function(x) c(coef(x), rsquare = rsq(x), half = half_life(x), SE = err(x),
                                               lower = lower_CI(x), upper = upper_CI(x), pvalue = lmp(x)))

