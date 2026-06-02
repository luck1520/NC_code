#Supplementary Fig. 2 
library(mgcv)
library(ggplot2)
library(visreg)
library(dplyr)

data <- read.csv("G:/NC/GAM_data_all.csv")
df <- na.omit(data)
df$vegtype <- as.factor(df$vegtype)

gam_model <- gam(
  resilience ~
    s(mat) +
    s(spei) +
    s(toc)+
    s(awc)+
    s(st)+
    s(ksn)+
    s(hfp)+
    vegtype,
  data = df,
)
summary(gam_model)
plot(gam_model , pages=1, shade=TRUE, scale=0) # partial effect of continuous predictors

library(ggeffects)
eff <- ggpredict(gam_model, terms="vegtype") # predicted marginal means of responses for categorical predictors
plot(eff)
ggplot(eff, aes(x = x, y = predicted)) +
  geom_col(fill = "grey80", width = 0.6, color = "black") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.15, linewidth = 0.8, color = "black") +
  theme_classic(base_size = 14) +
  labs(x = "vegtype", y= "predicted α") +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
  )
