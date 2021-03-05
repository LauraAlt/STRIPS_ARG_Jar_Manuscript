library("readxl")
library("ggplot2")
library("dplyr")
library("writexl")

#Change sheet name to generate data for each gene and then compile together
standard_data <- read_excel("Raw_Standards_Data.xlsx",
                            sheet = "tetM")

LOD_LOQ_data <- read_excel("Raw_LOD_LOQ_Data.xlsx",
                           sheet = "tetM")

sample_ct_values <- read_excel("Raw_Sample_Data.xlsx",
                               sheet = "tetM")

meta_data <- read_excel("Metadata.xlsx",
                        sheet = "Sheet1")

#Plotting Standard Curve
ggplot(standard_data, aes(x = `log10 [Copy Number]`, y = `Ct Value`)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "log10 [Copy Number]", y = "Ct")

#r^2 and efficiency
lm.r <- lm(`Ct Value` ~ `log10 [Copy Number]`, data = standard_data)
summary(lm.r)$r.squared

coef <- coef(lm.r)[2]
eff <- 100*((10^(-1/coef))-1)
eff

#LOD and LOQ Calcs
slope <- summary(lm.r)$coefficients[2]
y_intercept <- summary(lm.r)$coefficients[1]
LOD_ct <- LOD_LOQ_data$LOD
LOQ_ct <- LOD_LOQ_data$LOQ

#dilution factor is the result of
#2 uL of DNA per reaction out of 100 uL extracted (50x)
#1:10 dilution (10x) in soil or 1:100 dilution in manure (100x)
#0.25 g to 1 g (4x)
#30% moisutre in soil (dw) (10/7x)or manure is kept in (ww)
dilution_factor_soil <- (50*10*4*10/7)
dilution_factor_manure <- (50*100*4)

LOD_copy_num_soil <- 10^((LOD_ct - y_intercept)/slope)*dilution_factor_soil
LOQ_copy_num_soil <- 10^((LOQ_ct - y_intercept)/slope)*dilution_factor_soil
AVG_LOD_LOQ_copy_num_soil <- (LOD_copy_num_soil + LOQ_copy_num_soil)/2

LOD_copy_num_manure <- 10^((LOD_ct - y_intercept)/slope)*dilution_factor_manure
LOQ_copy_num_manure <- 10^((LOQ_ct - y_intercept)/slope)*dilution_factor_manure
AVG_LOD_LOQ_copy_num_manure <- (LOD_copy_num_manure + LOQ_copy_num_manure)/2

#Calculating Copy Numbers of Samples
#All Manure samples are above LOQ, so there is no need to censor them
sample_copy_num <- sample_ct_values %>% group_by(Sample) %>%
  summarise(AVG_Ct = mean(Cq), .groups = "drop") %>%
  merge(., meta_data, by = "Sample")

sample_copy_num1 <- sample_copy_num %>% 
  mutate(Copy_Num = 10^((sample_copy_num$AVG_Ct - y_intercept)/slope)*sample_copy_num$Dil_factor)

sample_copy_num2 <- sample_copy_num1 %>%
  mutate(Copy_Num = case_when(Copy_Num < LOQ_copy_num_soil & Copy_Num > LOD_copy_num_soil ~ LOQ_copy_num_soil,
                              Copy_Num < LOD_copy_num_soil ~ AVG_LOD_LOQ_copy_num_soil,
                              is.na(Copy_Num) ~ LOD_copy_num_soil,
                              TRUE ~ Copy_Num))

write_xlsx(sample_copy_num2, "tetM_Copy_Num.xlsx")

