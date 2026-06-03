library(mgcv)
data <- read.csv("G:/NC/GAM_data_all_trees.csv")
df <- na.omit(data)

mod <- gam(resilience ~ s(mat)+s(spei)+s(toc)+s(st)+s(awc)+s(ksn)+s(hfp), data = df)

terms_out <- predict(mod, type = "terms", se.fit = TRUE)  
terms_mat <- terms_out$fit
term_names <- colnames(terms_mat)
clim_cols <- c("s(spei)", "s(mat)","s(hfp)") # removing climatic and anthropogenic signals
clim_effect_link <- rowSums(terms_mat[, clim_cols, drop = FALSE])

eta_total <- predict(mod, type = "link")
eta_no_clim <- eta_total - clim_effect_link
mu_no_clim <- mod$family$linkinv(eta_no_clim)  
df$mu_no_clim <- mu_no_clim

write.csv(df, "G:/NC/no_clim&hum_resi_trees.csv", row.names = FALSE)