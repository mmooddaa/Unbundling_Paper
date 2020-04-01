# Plotting Functions

# Function plotting matching estimates with bootstrapped standard errors
nicePMplot <- function(pm.object) {
  set.seed(1023)
  PE.results95 <- PanelEstimate(inference = "bootstrap", sets = pm.object, 
                                data = fullData, CI = .95, ITER = 500)
  results95 <- as.data.frame(summary(PE.results95)$summary)
  
  PE.results90 <- PanelEstimate(inference = "bootstrap", sets = pm.object, 
                                data = fullData, CI = .90, ITER = 500)
  results90 <- as.data.frame(summary(PE.results90)$summary)
  
  lead <- attr(pm.object, 'lead')
  
  plot <- ggplot(data = results95, aes(y = estimate, x = lead)) +
    geom_errorbar(aes(ymin = `2.5%`, 
                      ymax = `97.5%`), width=0, size = 2) +
    geom_errorbar(data = results90, aes(ymin = `5%`,
                                        ymax = `95%`), 
                  width=0, size = 4) +
    geom_point(size = 10) +
    geom_hline(yintercept = 0, linetype = "dashed", size = 1) +
    scale_y_continuous() +
    scale_x_continuous(breaks = lead) +
    theme(panel.grid.major.x = element_blank(), 
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank()) +
    xlab("Years Since Model Law Enactment") +
    ylab("Estimated Effect of Treatment on the Treated")

  return(plot)
}

# Function plotting covariate balance pre- and post-refinement
covBalPlot <- function(pm.object) {
  refined_bal <- as.data.frame(get_covariate_balance(pm.object$att, fullData, 
                                                     covariates = c("v2x_polyarchy", "v2x_rule",
                                                                    "lnBITcount", "lnGDP",
                                                                    "lnGDPpc", "trade",
                                                                    "growth", "lnPOP"), 
                                                     plot = F, use.equal.weights = F))
  
  refined_bal <- gather(refined_bal, key = "covariate", value = "ref_sd")
  refined_bal$t <- rev(attr(pm.object, 'lead'))
  refined_bal$abs_ref_sd <- abs(refined_bal$ref_sd)
  
  unrefined_bal <- as.data.frame(get_covariate_balance(pm.object$att, fullData, 
                                                       covariates = c("v2x_polyarchy", "v2x_rule",
                                                                      "lnBITcount", "lnGDP",
                                                                      "lnGDPpc", "trade",
                                                                      "growth", "lnPOP"), 
                                                       plot = F, use.equal.weights = T))
  unrefined_bal <- gather(unrefined_bal, key = "covariate", value = "unref_sd")
  unrefined_bal$t <- rev(attr(pm.object, 'lead'))
  unrefined_bal$abs_unref_sd <- abs(unrefined_bal$unref_sd)
  
  balance_test <- merge(refined_bal, unrefined_bal, by = c("covariate", "t"))
  balance_test$covariate <- as.factor(balance_test$covariate)
  
  # Create a reference line
  refLine <- data.frame(x = c(0, 1.5), y = c(0, 1.5))
  
  # Plot the covariate balance of "refined" set against "unrefined" set
  plot <- ggplot(data = balance_test, 
                 aes(x = abs_unref_sd, y = abs_ref_sd, color = covariate)) +
    geom_point(size = 5) +
    scale_y_continuous(limits = c(0, 1.5), breaks = c(0, .25, .5, .75, 1, 1.25, 1.5)) +
    scale_x_continuous(limits = c(0, 1.5), breaks = c(0, .25, .5, .75, 1, 1.25, 1.5)) +
    geom_line(data = refLine, aes(x = x, y = y, color = NULL), linetype = 2) +
    ylab("Post-Refinement") +
    xlab("Pre-Refinement")
  
  return(plot)
}