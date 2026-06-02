pred_terms <- predict(gam_model, type = "terms")
term_var <- apply(pred_terms, 2, var)
total_var <- sum(term_var)
variance_contributions <- (term_var / total_var) * 100
variance_contributions