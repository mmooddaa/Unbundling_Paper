# Replication File --------------------------------------------------------
# "Unbundling the State: 
#     Legal Development in an Era of Global, Private Governance"
# By Michael O. Allen
#
# Graphs and Matching Analysis

# Set working directory to directory containing all scripts and data
setwd("")

library(PanelMatch)
library(ggplot2)
library(tidyr)
library(dplyr)
library(zoo)

### Load plotting functions
source("plotFunctions.R")

theme_set(theme_bw(base_size = 22))

# Plot 1 - TGI Growth -----------------------------------------------------
# filePath currently set for URL, if URL broken download and change filePath.
filePath <- "https://static-content.springer.com/esm/art%3A10.1007%2Fs11558-019-09366-w/MediaObjects/11558_2019_9366_MOESM3_ESM.csv"
TGIdata <- read.csv(filePath)
rm(filePath)

TGIdata <- subset(TGIdata, govtask_implement == 1 | 
                    TGIdata$govtask_monitoring == 1 | 
                    TGIdata$govtask_setting == 1)

TGI_TS <- data.frame(year = 1980:2016)

temp <- data.frame(count = cumsum(tapply(TGIdata$id, TGIdata$strt_yr, length)))
TGI_TS$count <- temp$count[match(TGI_TS$year, row.names(temp))]
rm(temp)

# 1980 had 7 cumulative TGIs
TGI_TS$count <- c(7, na.locf(TGI_TS$count))

TGI_TS_end <- data.frame(end = cumsum(tapply(TGIdata$id, TGIdata$end_yr, length)))
TGI_TS$end <- TGI_TS_end$end[match(TGI_TS$year, row.names(TGI_TS_end))]
TGI_TS$end <- c(rep(0, 21), na.locf(TGI_TS$end))
TGI_TS$count <- TGI_TS$count - TGI_TS$end
rm(TGI_TS_end)

# Commerce related issues:
# regdev, trade, commerce, bankfinance, labor, legal, monetary policy, development
TGIdata <- mutate(TGIdata, issue_trade +
                    issue_commerce +
                    issue_bankfinance +
                    issue_labor +
                    issue_legal +
                    issue_monetarypolicy)

colnames(TGIdata)[length(colnames(TGIdata))] <- "commerceIssue"
TGIdata$commerceIssue[TGIdata$commerceIssue > 0] <- 1

temp <- data.frame(commerce = cumsum(tapply(TGIdata$commerceIssue, TGIdata$strt_yr, sum)))
TGI_TS$commerce <- temp$commerce[match(TGI_TS$year, row.names(temp))]
TGI_TS$commerce <- c(3, na.locf(TGI_TS$commerce))
rm(temp)

plot1 <- ggplot(data = TGI_TS[TGI_TS$year >= 1980, ], aes(x = year, y = count)) +
  geom_area(fill = "grey") +
  geom_area(aes(x = year, y = commerce), fill = "black") +
  scale_y_continuous(expand = c(0,0), limits = c(0, 205),
                     breaks = seq(0, 200, 25)) +
  scale_x_continuous(breaks = seq(1980, 2015, 5),
                     labels = seq(1980, 2015, 5)) +
  labs(x = element_blank(), y = element_blank()) +
  annotate(geom = "text", x = 2007.5, y = 20, size = 11,
           label = "Commerce-related",
           hjust = 0.5, lineheight = .75, color = "white") +
  annotate(geom = "text", x = 2008, y = 95, size = 11,
           label = "Total",
           hjust = 0.5, lineheight = .75, color = "black") +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank())

rm(TGIdata, TGI_TS)

# Figure 2 - ICC Caseload -------------------------------------------------
ICCdata <- readxl::read_xlsx("/Users/mallen/Box Sync/Cornell/Data/ICC Data/ICC Total Cases/ICC Cases.xlsx")

ICCdata$icsidCases[ICCdata$icsidCases == 0] <- as.integer(NA)

ICCdata <- gather(ICCdata, "institution", "cases", -"year")
ICCdata <- ICCdata[ICCdata$year < 2018, ]

# Missing values warning due to "icsidCases" 0s recoded as NAs
plot2 <- ggplot(ICCdata[ICCdata$institution == "iccCases" | 
                          ICCdata$institution == "icsidCases",], 
                aes(x = year, y = cases, fill = institution)) +
  geom_bar(stat= "identity", position = "identity") +
  labs(title = "", x = "Year", y = "Cases Registered", fill = "") +
  scale_fill_manual(labels = c("ICC", 
                               "ICSID"), values = c("grey", "black")) +
  scale_y_continuous(limits = c(0,1010), 
                     breaks = seq(0, 1010, 250),
                     expand = c(0,0)) +
  scale_x_continuous(limits = c(1921, 2018),
                     breaks = c(1921, seq(1930, 2010, 10), 2017)) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank(),
        legend.position = "bottom")
rm(ICCdata)

# Figure 3 - Model Law Enactment ------------------------------------------
# Import Model Law Enactment Data
ML.data  <- read.csv("/Users/mallen/Box Sync/Cornell/Data/UNICTRAL Model Law/Model_Law_Data.csv", 
                     row.names = "X")

# Plot with full ML data
yearlyAvg_ML <- data.frame(year = 1985:2017, 
                           modelLaw = tapply(ML.data$ml, ML.data$year, mean, na.rm = TRUE),
                           modelLaw_count = tapply(ML.data$ml, ML.data$year, sum, na.rm = TRUE))

plot3 <- ggplot(data = yearlyAvg_ML, aes(x = year, y = modelLaw_count)) + 
  geom_bar(fill = "lightgrey", color = "white", stat="identity") +
  geom_line(data = yearlyAvg_ML, aes(x = year, y = modelLaw * 79.8), 
            size = 3) +
  xlab("") + 
  ylab("# of Enacting States") +
  ggtitle("") +
  scale_x_continuous(breaks = seq(1985, 2017, 5)) +
  scale_y_continuous(limits = c(0, 80),
                     breaks = seq(0, 80, 15), 
                     labels = seq(0, 80, 15),
                     sec.axis = sec_axis(~. / 79.8, name = "Enacting States as % of All States", 
                                         breaks = seq(0, 1, .25),
                                         label = scales::percent(seq(0, 1, .25), 1L)),
                     expand = c(0,0)) +
  theme(legend.position = "bottom",  
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank()) +
  guides(linetype=FALSE)

rm(ML.data, yearlyAvg_ML)

# Estimate Main Models ----------------------------------------------------
fullData <- read.csv("unbundling_data.csv", row.names = "X", 
                     stringsAsFactors = FALSE)

# Table 3, Column 1 ====
PM.results1 <- PanelMatch(lag = 5, time.id = "year", unit.id = "iso3c", 
                          treatment = "ml", refinement.method = "CBPS.weight", 
                          data = fullData, 
                          match.missing = TRUE, 
                          covs.formula = ~ lag("v2x_polyarchy", 1)
                          + lag("v2x_rule", 1)
                          + lag("lnBITcount", 1)
                          + lag("lnGDP", 1)
                          + lag("lnGDPpc", 1)
                          + lag("trade", 1)
                          + lag("growth", 1)
                          + lag("lnPOP", 1),
                          qoi = "att", verbose = TRUE, matching = FALSE,
                          outcome.var = "v2x_rule",
                          lead = 0:5, forbid.treatment.reversal = TRUE)
# Table 3, Column 2 ====
PM.results2 <- PanelMatch(lag = 5, time.id = "year", unit.id = "iso3c", 
                          treatment = "ml", refinement.method = "CBPS.weight", 
                          data = fullData[fullData$highRoL != 1, ], 
                          match.missing = TRUE, 
                          covs.formula = ~ lag("v2x_polyarchy", 1)
                          + lag("v2x_rule", 1)
                          + lag("lnBITcount", 1)
                          + lag("lnGDP", 1)
                          + lag("lnGDPpc", 1)
                          + lag("trade", 1)
                          + lag("growth", 1)
                          + lag("lnPOP", 1),
                          qoi = "att", verbose = TRUE, matching = FALSE,
                          outcome.var = "v2x_rule",
                          lead = 0:5, forbid.treatment.reversal = TRUE)
# Table 3, Column 3 ====
PM.results3 <- PanelMatch(lag = 5, time.id = "year", unit.id = "iso3c", 
                          treatment = "ml", refinement.method = "ps.weight", 
                          data = fullData[fullData$highRoL != 1, ], 
                          match.missing = TRUE, 
                          covs.formula = ~ lag("v2x_polyarchy", 1)
                          + lag("v2x_rule", 1)
                          + lag("lnBITcount", 1)
                          + lag("lnGDP", 1)
                          + lag("lnGDPpc", 1)
                          + lag("trade", 1)
                          + lag("growth", 1)
                          + lag("lnPOP", 1),
                          qoi = "att", verbose = TRUE, matching = FALSE,
                          outcome.var = "v2x_rule",
                          lead = 0:5, forbid.treatment.reversal = TRUE)
# Table 3, Column 4 ====
PM.results4 <- PanelMatch(lag = 5, time.id = "year", unit.id = "iso3c", 
                          treatment = "ml", refinement.method = "CBPS.weight", 
                          data = fullData[fullData$lowRoL != 1, ], 
                          match.missing = TRUE, 
                          covs.formula = ~ lag("v2x_polyarchy", 1)
                          + lag("v2x_rule", 1)
                          + lag("lnBITcount", 1)
                          + lag("lnGDP", 1)
                          + lag("lnGDPpc", 1)
                          + lag("trade", 1)
                          + lag("growth", 1)
                          + lag("lnPOP", 1),
                          qoi = "att", verbose = TRUE, matching = FALSE,
                          outcome.var = "v2x_rule",
                          lead = 0:5, forbid.treatment.reversal = TRUE)

# Appendix B --------------------------------------------------------------
# Table B1
PM.results_B1 <- PanelMatch(lag = 5, time.id = "year", unit.id = "iso3c", 
                            treatment = "ml", refinement.method = "CBPS.weight", 
                            data = fullData[fullData$highRoL != 1, ], 
                            match.missing = FALSE, 
                            covs.formula = ~ lag("v2x_polyarchy", 1)
                            + lag("v2x_rule", 1)
                            + lag("lnBITcount", 1)
                            + lag("lnGDP", 1)
                            + lag("lnGDPpc", 1)
                            + lag("trade", 1)
                            + lag("growth", 1)
                            + lag("lnPOP", 1),
                            qoi = "att", verbose = TRUE, matching = FALSE,
                            outcome.var = "v2x_rule",
                            lead = 0:5, forbid.treatment.reversal = TRUE)
# Table B2a
PM.results_B2a <- PanelMatch(lag = 5, time.id = "year", unit.id = "iso3c", 
                             treatment = "ml", refinement.method = "CBPS.weight", 
                             data = fullData[fullData$highRoL != 1, ], 
                             match.missing = FALSE, 
                             covs.formula = ~ lag("v2x_polyarchy", 1)
                             + lag("v2x_rule", 1)
                             + lag("lnBITcount", 1)
                             + lag("lnGDP", 1)
                             + lag("lnGDPpc", 1)
                             + lag("trade", 1)
                             + lag("growth", 1)
                             + lag("lnPOP", 1),
                             qoi = "att", verbose = TRUE, matching = FALSE,
                             outcome.var = "v2x_rule",
                             lead = 0:10, forbid.treatment.reversal = TRUE)
# Table B2b 
PM.results_B2b <- PanelMatch(lag = 5, time.id = "year", unit.id = "iso3c", 
                             treatment = "ml", refinement.method = "CBPS.weight", 
                             data = fullData[fullData$highRoL != 1, ], 
                             match.missing = TRUE, 
                             covs.formula = ~ lag("v2x_polyarchy", 1)
                             + lag("v2x_rule", 1)
                             + lag("lnBITcount", 1)
                             + lag("lnGDP", 1)
                             + lag("lnGDPpc", 1)
                             + lag("trade", 1)
                             + lag("growth", 1)
                             + lag("lnPOP", 1),
                             qoi = "att", verbose = TRUE, matching = FALSE,
                             outcome.var = "v2x_rule",
                             lead = 0:10, forbid.treatment.reversal = TRUE)

# Table 3 - Standard Errors -----------------------------------------------
# Plug in the PM object into the "sets = " option
PE.results <- PanelEstimate(inference = "bootstrap", sets = PM.results2, 
                            data = fullData, CI = .95, ITER = 500)
summary(PE.results)

# Figure 4 - Main Results ATT ---------------------------------------------
# Main Results Plot (Figure 3)
plot4 <- nicePMplot(PM.results2)

# Plots in Appendix B
plotB1 <- nicePMplot(PM.results_B1)
plotB2a <- nicePMplot(PM.results_B2a)
plotB2b <- nicePMplot(PM.results_B2b)


# Figure 5 - Disaggregated Plot -------------------------------------------
namesList <- names(PM.results2$att)

# Create holder dfs
weightsDf <- data.frame()
diffDf <- data.frame()

for (i in 1:length(namesList)) {
  # Separate country name and treatment year
  # Then create df with weights for that matched set
  countryInfo <- strsplit(namesList[i], "[_\\.]")
  tempDf <- data.frame(iso3c = countryInfo[[1]][1], 
                       treatYear = as.integer(countryInfo[[1]][2]))
  tempDf <- cbind(tempDf, 
                  t(data.frame(attributes(PM.results2[["att"]][[namesList[i]]]))))
  weightsDf <- bind_rows(weightsDf, tempDf)
  
  # Create df of sample country values
  sampleCountries <- colnames(tempDf[3:ncol(tempDf)])
  
  cntrlDf <- data.frame()
  
  # Create df with weighted values
  for (p in 1:length(sampleCountries)) {
    # Grab values for matched unit P for 1 year prior to treatment and 5 years post.
    dataHolder <- fullData[fullData$iso3c == sampleCountries[p]
                           & fullData$year >= (tempDf$treatYear - 1)
                           & fullData$year <= (tempDf$treatYear + 5), 
                           c("iso3c", "year", "v2x_rule")]
    dataHolder$did <- 0
    for (d in 0:5) {
      dataHolder$did[(d+2)] <- dataHolder$v2x_rule[(d+2)] - dataHolder$v2x_rule[1]
    }
    dataHolder$weightedDID <- dataHolder$did * tempDf["weights",sampleCountries[p]]
    cntrlDf <- bind_rows(cntrlDf, dataHolder)
  }
  # Create holder for matched set weighted DiD
  cntrlHolder <- data.frame(ml_distance = -1:5, 
                            cntrlDID = tapply(cntrlDf$weightedDID, cntrlDf$year, sum))
  
  # Create treated unit DiD
  treatedValues <- fullData[fullData$iso3c == tempDf$iso3c
                            & fullData$year >= (tempDf$treatYear - 1)
                            & fullData$year <= (tempDf$treatYear + 5), 
                            c("iso3c", "year", "v2x_rule")]
  treatedValues$did <- 0
  for (d in 0:5) {
    treatedValues$did[(d+2)] <- treatedValues$v2x_rule[(d+2)] - treatedValues$v2x_rule[1]
  }
  
  diffHolder <- data.frame(iso3c = rep(tempDf$iso3c[1], 7),
                           treatYear = rep(tempDf$treatYear[1], 7),
                           ml_distance = -1:5, 
                           diff = treatedValues$did - cntrlHolder$cntrlDID,
                           didTreated = treatedValues$did,
                           didControl = cntrlHolder$cntrlDID)
  diffDf <- bind_rows(diffDf, diffHolder)
}
rm(i, tempDf, countryInfo, cntrlHolder, 
   namesList, p, sampleCountries, diffHolder, 
   dataHolder, treatedValues, cntrlDf, d)

# Merge with v2x_rule during treatYear, for Appendix A
diffDf <- merge(diffDf, fullData[,c("iso3c", "year", "v2x_rule", "v2x_polyarchy")],
                by.x = c("iso3c", "treatYear"), by.y = c("iso3c", "year"),
                all.x = TRUE)

## PLOT DIFFERENCES ##
treatedDelta <- data.frame(time = -1:5, 
                           treatDelta = tapply(diffDf$didTreated, diffDf$ml_distance, mean))
cntrlDelta   <- data.frame(time = -1:5, 
                           cntrlDelta = tapply(diffDf$didControl, diffDf$ml_distance, mean))
diffs        <- left_join(treatedDelta, cntrlDelta, by = "time")
diffs        <- gather(diffs, key = "status", value = "diff", -time)

rm(weightsDf, cntrlDelta, treatedDelta)

plot5 <- ggplot(data = diffs[diffs$time >= 0, ], aes(x = time, y = diff, color = status)) +
  geom_line(size = 2, show.legend = FALSE) +
  geom_point(size = 8, aes(shape = status)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1) +
  labs(title = "", x = "Years Since Model Law Enactment", 
       y = "Change in Rule of Law Index", 
       color = "", shape = "") +
  scale_colour_manual(labels = c("No Model Law", "Model Law"), 
                      values = c("#56B4E9", "tomato2")) +
  scale_shape_manual(labels = c("No Model Law", "Model Law"), 
                     values=c(19, 17)) +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.y = element_blank()) +
  theme(legend.position="bottom")

rm(diffs, diffDf)

# Appendix C - Covariate Balance Plots ------------------------------------
# Figures C1, a-c
plotC1a <- covBalPlot(PM.results1)
plotC1b <- covBalPlot(PM.results2)
plotC1c <- covBalPlot(PM.results_B1)