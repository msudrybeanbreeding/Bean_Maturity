# install.packages("devtools")
#devtools::install_github("adriancorrendo/metrica")

library(metrica)
library(dplyr)
library(purrr)
library(ggplot2)
library(tidyr)
library(ggpubr)

rm(list=ls())
my.path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my.path)

df_GT1 <- read.csv('result_mtr_test_v4.csv')
df_LOESS <- read.csv("I:/My Drive/UAS_Beans/Beans_Maturity/2021/SVREC_Mat/h._VIs_Method2/Analysis/9_flights/Mat_LOESS_GLI_mean_0.06.csv")
df_SEG <- read.csv("I:/My Drive/UAS_Beans/Beans_Maturity/2021/SVREC_Mat/h._VIs_Method2/Analysis/9_flights/Mat_SEG_GLI_mean_0.06.csv")

names(df_GT1)  
names(df_LOESS)

df_GT <- df_GT1 %>% 
  subset(df_GT1$Env == 2) %>% 
  select(Global_ID, GT)

df_GT_LOESS_SEG <- df_LOESS %>% 
  filter(Plots_ID %in% df_GT$Global_ID) %>% 
  left_join(df_GT, by = c("Plots_ID" = "Global_ID")) %>% 
  left_join(df_SEG, by = "Plots_ID") 

names(df_GT_LOESS_SEG)
str(df_GT_LOESS_SEG)


# # Create list of selected metrics
# selected.metrics <- c("r","MAE","MSE", "R2")
# 
# df_SVREC20<- df %>% 
#   subset(df$Env == 1) 
# 
# df_SVREC21<- df %>% 
#   subset(df$Env == 2) 
# 
# df_HUR21<- df %>% 
#   subset(df$Env == 3) 
# 
# df_SVREC22<- df %>% 
#   subset(df$Env == 4) 
# 
# df_HUR22<- df %>% 
#   subset(df$Env == 5) 

#### Ground truth vs stand count predicted ####

##### Train set #####
# Create the plot
# str(df_SVREC20)
# SVREC20_TR <- metrica::scatter_plot(data = df_SVREC20, 
#                                                obs = GT, pred = MTR_Pred,
#                                                # Activate print_metrics arg.
#                                                print_metrics = TRUE, 
#                                                # Indicate metrics list
#                                                metrics_list = selected.metrics,
#                                                # Customize metrics position
#                                                position_metrics = c(x = 39 , y = 54),
#                                                # Customize equation position
#                                                position_eq = c(x = 45, y = 54),
#                                                regline_size = 1,
#                                                shape_color = "steelblue") + 
#   ylab("Pred. Drone") +
#   xlab(NULL) +
#   # Customize axis breaks
#   scale_y_continuous(breaks = seq(0,60, by = 2), limits = c(0,60))+
#   scale_x_continuous(breaks = seq(0,60, by = 2), limits = c(0,60)) +
#   labs(title = "SVREC 2020") + 
#   theme(plot.title=element_text(hjust=0.5))
# 
# SVREC20_TR
####


#### Correlation table ####

names(df_GT_LOESS_SEG)
selected.metrics <- c("r","MAE","MSE", "R2")

metrics.sum1 <- df_GT_LOESS_SEG %>%
  drop_na() %>% 
  metrics_summary( obs = GT, pred = Mat_LOESS_GLI_mean_0.06,
                   type = "regression")  %>%
  filter(Metric %in% c("r","MAE","MSE", "R2")) %>%
  mutate(Dataset = c("LOESS_2021_SVREC_9_fly")) %>% 
  print()

metrics.sum2 <- df_GT_LOESS_SEG %>%
  drop_na() %>% 
  metrics_summary( obs = GT, pred = Mat_SEG_GLI_mean_0.06,
                   type = "regression")  %>%
  filter(Metric %in% c("r","MAE","MSE", "R2")) %>%
  mutate(Dataset = c("SEG_2021_SVREC_9_fly")) %>% 
  print()

data_metric_final<- rbind(metrics.sum1, metrics.sum2)

print(data_metric_final)


write.csv(data_metric_final, "Metric_2021_SVREC.csv", row.names = F)



