#Fig. 4d resilience sensitivity to MAT under different lithologies
data <- read.csv("G:/NC/GAM_data_all_shrub.csv")
df <- na.omit(data)
df$lithology <- as.factor(df$lithology)

m <- gamm(
  resilience ~ s(spei) + s(awc) + s(toc) + s(st) + s(ksn) + s(hfp),
  random = list(lithology = ~ mat),
  data = df
)

ranef_vals <- ranef(m$lme)$lithology
sens <- abs(ranef_vals$mat) 
