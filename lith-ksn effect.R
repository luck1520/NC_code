#Supplementary Fig. 6 ksn effects on resilience across 4 lithological groups
library(mgcv)
library(ggplot2)
library(visreg)
library(dplyr)

data <- read.csv("G:/NC/GAM_data_all.csv")
df <- na.omit(data)
df$veg_group <- as.factor(df$veg_group) # 3 major vegetation types
df$lith <- as.factor(df$lith) # 4 lithological groups according to mechanical strength

gam_model2 <- gam(
  resilience ~
    s(ksn,by=lith)+
    s(mat) +
    s(spei) +
    s(toc)+
    s(awc)+
    s(st)+
    s(hfp)+
    lith+
    veg_group,
  data = df,
)
summary(gam_model2)

lith_levels <- unique(df$lith)
ksn_range <- seq(min(df$ksn), max(df$ksn), length.out = 100)
other_means <- df %>%
  summarise(
    mat = mean(mat),
    spei = mean(spei),
    toc = mean(toc),
    awc = mean(awc),
    st = mean(st),
    hfp = mean(hfp)
  )

pred_data <- expand.grid(
  ksn = ksn_range,
  lith = lith_levels,
  mat = other_means$mat,
  spei = other_means$spei,
  toc = other_means$toc,
  awc = other_means$awc,
  st = other_means$st,
  hfp = other_means$hfp,
  veg_group = df$veg_group[1]  # select trees as reference level
)

pred <- predict(gam_model2, newdata = pred_data, se.fit = TRUE)
pred_data$fit <- pred$fit
pred_data$lower <- pred$fit - 1.96 * pred$se.fit
pred_data$upper <- pred$fit + 1.96 * pred$se.fit

p1 <- ggplot(pred_data, aes(x = ksn, y = fit, color = lith, fill = lith)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  xlim(0,2000)+
  ylim(0.4,0.6)+
geom_line(size = 1) +
  labs(title = "ksn effects on resilience across lithological groups", x = "ksn", y = "α") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_color_brewer(palette = "Set1")

print(p1)  
