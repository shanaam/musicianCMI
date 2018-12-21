#############################
## MUSICIAN CMI EXPERIMENT ##
#############################

## BEFORE YOU  BEGIN
# If you havent already, uncomment (Ctrl + Shift + C) and run the following lines of code to install required packages (only do this one time)
# install.packages("tidyverse") # for plotting / others
# install.packages("doBy") # for summary statistics function
# install.packages("ez") # for ezANOVA
# install.packages("lsmeans") # for posthocs
# install.packages("ggbeeswarm") # for plots

# set the working directory to wherever this file is located
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

# read in raw data files
rawDataStandard <- read.csv("data/standard_BrDIMeansZScores.csv")
rawDataStandard$Condition <- "standard"
rawDataPC <- read.csv("data/planeChange_BrDIMeansZScores.csv")
rawDataPC$Condition <- "planeChange"
rawDataFBR <- read.csv("data/FBReversal_BrDIMeansZScores.csv")
rawDataFBR$Condition <- "FBReversal"
rawDataPCandFBR <- read.csv("data/FBRandPC_BrDIMeansZScores.csv")
rawDataPCandFBR$Condition <- "PCandFBR"
#allData <- rbind(rawDataStandard, rawDataPC, rawDataFBR, rawDataPCandFBR)
allData <- rbind(rawDataStandard, rawDataPCandFBR)

#####
# General Functions

# Function to get sd and ci (confidence intervals)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE, conf.interval=.95) {
  library(doBy)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # Collapse the data
  formula <- as.formula(paste(measurevar, paste(groupvars, collapse=" + "), sep=" ~ "))
  datac <- summaryBy(formula, data=data, FUN=c(length2,mean,sd), na.rm=na.rm)
  
  # Rename columns
  names(datac)[ names(datac) == paste(measurevar, ".mean",    sep="") ] <- measurevar
  names(datac)[ names(datac) == paste(measurevar, ".sd",      sep="") ] <- "sd"
  names(datac)[ names(datac) == paste(measurevar, ".length2", sep="") ] <- "N"
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

roundANOVA <- function (ezOutput) {
  format(round(ezOutput$ANOVA$p, 3), nsmall = 2)
}

setFactors <- function(df) {
  df$Participant_Nr <- as.factor(df$Participant_Nr)
  df$Participant_ID <- as.factor(df$Participant_ID)
  df$Condition <- as.factor(df$Condition)
  df$Musician <- as.factor(df$Musician)
  df$Sex <- as.factor(df$Sex)
  df$Diagnosis <- as.factor(df$Diagnosis)
  
  return(df)
}
#####
## Statistics

# remove outlier
allData <- allData[allData$Participant_ID != "MUS_AZ", ]

# set factor (categorical) variables as factors
allData <- setFactors(allData)

# Set reference factors
allData <- within(allData, Condition <- relevel(Condition, ref = "standard"))
allData <- within(allData, Musician <- relevel(Musician, ref = "0"))
allData <- within(allData, Diagnosis <- relevel(Diagnosis, ref = "4"))
allData <- within(allData, Sex <- relevel(Sex, ref = "2"))

nonStandardData <- allData[allData$Condition == "PCandFBR", ]
standardData <- allData[allData$Condition == "standard", ]
# Descriptive Statistics

# Mean and SD for each composite z-score per condition -- is this necessary?
# Standard condition
meanTimingScoreStandard <- mean(rawDataStandard[rawDataStandard$Musician == 1, ]$TimingScore)
sdTimingScoreStandard <- sd(rawDataStandard[rawDataStandard$Musician == 1, ]$TimingScore)
meanTimingScorePC <- mean(rawDataPC[rawDataPC$Musician == 1, ]$TimingScore)
sdTimingScorePC <- sd(rawDataPC[rawDataPC$Musician == 1, ]$TimingScore)
meanTimingScoreFBR <- mean(rawDataFBR[rawDataFBR$Musician == 1, ]$TimingScore)
sdTimingScoreFBR <- sd(rawDataFBR[rawDataFBR$Musician == 1, ]$TimingScore)
meanTimingScorePCandFBR <- mean(rawDataPCandFBR[rawDataPCandFBR$Musician == 1, ]$TimingScore)
sdTimingScorePCandFBR <- sd(rawDataPCandFBR[rawDataPCandFBR$Musician == 1, ]$TimingScore)

# Library
library(ez)

# Timing Score
timingScoreAOV <- ezANOVA(data=allData, dv=TimingScore, wid=Participant_Nr, within=Condition, between=Musician, return_aov=TRUE, type=3)
#timingScoreAOV$ANOVA$p <- roundANOVA(timingScoreAOV)
print("Timing Score ANOVA")
print(timingScoreAOV$ANOVA)


# Endpoint Error
endpointErrorAOV <- ezANOVA(data=allData, dv=EndpointErrorScore, wid=Participant_Nr, within=Condition, between=Musician, return_aov=TRUE, type=3)
# endpointErrorAOV$ANOVA$p <- roundANOVA(endpointErrorAOV)
print("Endpoint Error ANOVA")
print(endpointErrorAOV$ANOVA)


# corrective path length
CPLAOV <- ezANOVA(data=allData, dv=CPL, wid=Participant_Nr, within=Condition, between=Musician, return_aov=TRUE, type=3)
# CPL$ANOVA$p <- roundANOVA(CPLAOV)
print("Corrective Path Length ANOVA")
print(CPLAOV$ANOVA)

# direction reversal
DirRevAOV <- ezANOVA(data=allData, dv=X..Dir.Rev, wid=Participant_Nr, within=Condition, between=Musician, return_aov=TRUE, type=3)
#CPL$ANOVA$p <- roundANOVA(DirRevAOV)
print("Direction Reversal ANOVA")
print(DirRevAOV$ANOVA)

# T-tests to test group differences in standard and non-stndard conditions seperately
# Standard
comparison <- t.test( standardData[standardData$Musician == 0, "TimingScore"], standardData[standardData$Musician == 1, "TimingScore"])
cat("\nT-tests for timing scores in standard condition")
cat("\n", sprintf("Controls: %.3f (%.3f)", mean(standardData[standardData$Musician == 0, "TimingScore"]), sd(standardData[standardData$Musician == 0, "TimingScore"])))
cat("\n", sprintf("Musicians: %.3f (%.3f)", mean(standardData[standardData$Musician == 1, "TimingScore"]), sd(standardData[standardData$Musician == 1, "TimingScore"])))
cat("\n", sprintf("t(%.0f) = %.3f, p = %.3f", comparison$parameter, comparison$statistic, comparison$p.value), "\n\n")

comparison <-t.test( standardData[standardData$Musician == 0, "EndpointErrorScore"], standardData[standardData$Musician == 1, "EndpointErrorScore"])
cat("\nT-tests for endpoint error scores in standard condition")
cat("\n", sprintf("Controls: %.3f (%.3f)", mean(standardData[standardData$Musician == 0, "EndpointErrorScore"]), sd(standardData[standardData$Musician == 0, "EndpointErrorScore"])))
cat("\n", sprintf("Musicians: %.3f (%.3f)", mean(standardData[standardData$Musician == 1, "EndpointErrorScore"]), sd(standardData[standardData$Musician == 1, "EndpointErrorScore"])))
cat("\n", sprintf("t(%.0f) = %.3f, p = %.3f", comparison$parameter, comparison$statistic, comparison$p.value), "\n\n")

comparison <-t.test( standardData[standardData$Musician == 0, "CPL"], standardData[standardData$Musician == 1, "CPL"])
cat("\nT-tests for corrective path length scores in standard condition")
cat("\n", sprintf("Controls: %.3f (%.3f)", mean(standardData[standardData$Musician == 0, "CPL"]), sd(standardData[standardData$Musician == 0, "CPL"])))
cat("\n", sprintf("Musicians: %.3f (%.3f)", mean(standardData[standardData$Musician == 1, "CPL"]), sd(standardData[standardData$Musician == 1, "CPL"])))
cat("\n", sprintf("t(%.0f) = %.3f, p = %.3f", comparison$parameter, comparison$statistic, comparison$p.value), "\n\n")

comparison <-t.test( standardData[standardData$Musician == 0, "X..Dir.Rev"], standardData[standardData$Musician == 1, "X..Dir.Rev"])
cat("\nT-tests for direction reversals (%) in standard condition")
cat("\n", sprintf("Controls: %.3f (%.3f)", mean(standardData[standardData$Musician == 0, "X..Dir.Rev"]), sd(standardData[standardData$Musician == 0, "X..Dir.Rev"])))
cat("\n", sprintf("Musicians: %.3f (%.3f)", mean(standardData[standardData$Musician == 1, "X..Dir.Rev"]), sd(standardData[standardData$Musician == 1, "X..Dir.Rev"])))
cat("\n", sprintf("t(%.0f) = %.3f, p = %.3f", comparison$parameter, comparison$statistic, comparison$p.value), "\n\n")

# report: mean, sd (for each group), t(df)= , p = 

# Non-standard
comparison <- t.test( nonStandardData[nonStandardData$Musician == 0, "TimingScore"], nonStandardData[nonStandardData$Musician == 1, "TimingScore"])
cat("\nT-tests for timing scores in nonStandard condition")
cat("\n", sprintf("Controls: %.3f (%.3f)", mean(nonStandardData[nonStandardData$Musician == 0, "TimingScore"]), sd(nonStandardData[nonStandardData$Musician == 0, "TimingScore"])))
cat("\n", sprintf("Musicians: %.3f (%.3f)", mean(nonStandardData[nonStandardData$Musician == 1, "TimingScore"]), sd(nonStandardData[nonStandardData$Musician == 1, "TimingScore"])))
cat("\n", sprintf("t(%.0f) = %.3f, p = %.3f", comparison$parameter, comparison$statistic, comparison$p.value), "\n\n")

comparison <-t.test( nonStandardData[nonStandardData$Musician == 0, "EndpointErrorScore"], nonStandardData[nonStandardData$Musician == 1, "EndpointErrorScore"])
cat("\nT-tests for endpoint error scores in nonStandard condition")
cat("\n", sprintf("Controls: %.3f (%.3f)", mean(nonStandardData[nonStandardData$Musician == 0, "EndpointErrorScore"]), sd(nonStandardData[nonStandardData$Musician == 0, "EndpointErrorScore"])))
cat("\n", sprintf("Musicians: %.3f (%.3f)", mean(nonStandardData[nonStandardData$Musician == 1, "EndpointErrorScore"]), sd(nonStandardData[nonStandardData$Musician == 1, "EndpointErrorScore"])))
cat("\n", sprintf("t(%.0f) = %.3f, p = %.3f", comparison$parameter, comparison$statistic, comparison$p.value), "\n\n")

comparison <-t.test( nonStandardData[nonStandardData$Musician == 0, "CPL"], nonStandardData[nonStandardData$Musician == 1, "CPL"])
cat("\nT-tests for corrective path length scores in nonStandard condition")
cat("\n", sprintf("Controls: %.3f (%.3f)", mean(nonStandardData[nonStandardData$Musician == 0, "CPL"]), sd(nonStandardData[nonStandardData$Musician == 0, "CPL"])))
cat("\n", sprintf("Musicians: %.3f (%.3f)", mean(nonStandardData[nonStandardData$Musician == 1, "CPL"]), sd(nonStandardData[nonStandardData$Musician == 1, "CPL"])))
cat("\n", sprintf("t(%.0f) = %.3f, p = %.3f", comparison$parameter, comparison$statistic, comparison$p.value), "\n\n")

comparison <-t.test( nonStandardData[nonStandardData$Musician == 0, "X..Dir.Rev"], nonStandardData[nonStandardData$Musician == 1, "X..Dir.Rev"])
cat("\nT-tests for direction reversals (%) in nonStandard condition")
cat("\n", sprintf("Controls: %.3f (%.3f)", mean(nonStandardData[nonStandardData$Musician == 0, "X..Dir.Rev"]), sd(nonStandardData[nonStandardData$Musician == 0, "X..Dir.Rev"])))
cat("\n", sprintf("Musicians: %.3f (%.3f)", mean(nonStandardData[nonStandardData$Musician == 1, "X..Dir.Rev"]), sd(nonStandardData[nonStandardData$Musician == 1, "X..Dir.Rev"])))
cat("\n", sprintf("t(%.0f) = %.3f, p = %.3f", comparison$parameter, comparison$statistic, comparison$p.value), "\n\n")

p.adjust(c(0.072, 0.894, 0.611, NaN, 0.934, 0.001, 0.002, 0.296), method = "bonferroni")

comparison <-t.test( nonStandardData[nonStandardData$Musician == 0, "Age"], nonStandardData[nonStandardData$Musician == 1, "Age"])
cat("\nT-tests for age")
cat("\n", sprintf("Controls: %.3f (%.3f)", mean(nonStandardData[nonStandardData$Musician == 0, "Age"]), sd(nonStandardData[nonStandardData$Musician == 0, "Age"])))
cat("\n", sprintf("Musicians: %.3f (%.3f)", mean(nonStandardData[nonStandardData$Musician == 1, "Age"]), sd(nonStandardData[nonStandardData$Musician == 1, "Age"])))
cat("\n", sprintf("t(%.0f) = %.3f, p = %.3f", comparison$parameter, comparison$statistic, comparison$p.value), "\n\n")

comparison <-t.test( nonStandardData[nonStandardData$Musician == 0, "Musician_Years"], nonStandardData[nonStandardData$Musician == 1, "Musician_Years"])
cat("\nT-tests for musician years")
cat("\n", sprintf("Controls: %.3f (%.3f)", mean(nonStandardData[nonStandardData$Musician == 0, "Musician_Years"]), sd(nonStandardData[nonStandardData$Musician == 0, "Musician_Years"])))
cat("\n", sprintf("Musicians: %.3f (%.3f)", mean(nonStandardData[nonStandardData$Musician == 1, "Musician_Years"]), sd(nonStandardData[nonStandardData$Musician == 1, "Musician_Years"])))
cat("\n", sprintf("t(%.0f) = %.3f, p = %.3f", comparison$parameter, comparison$statistic, comparison$p.value), "\n\n")

# # Multiple Linear Regression with Musician Years
# # Timing Score
# fit <- lm(TimingScore ~ Condition + Musician_Years + Diagnosis, data=allData)
# print("Timing Score MLR")
# print(summary(fit))
# print(coefficients(fit))
# print(confint(fit, level=0.95))
# 
# # Endpoint Error
# fit <- lm(EndpointErrorScore ~ Condition + Musician_Years + Diagnosis, data=allData)
# print("Endpoint Error MLR")
# print(summary(fit))
# print(coefficients(fit))
# print(confint(fit, level=0.95))
# 
# # Corrective Path Length
# fit <- lm(CPL ~ Condition + Musician_Years + Diagnosis, data=allData)
# print("Corrective Path Length MLR")
# print(summary(fit))
# print(coefficients(fit))
# print(confint(fit, level=0.95))
# 
# # Direction Reversal
# fit <- lm(X..Dir.Rev ~ Condition + Musician_Years + Diagnosis, data=allData)
# print("Direction Reversal MLR")
# print(summary(fit))
# print(coefficients(fit))
# print(confint(fit, level=0.95))


# # Multiple Linear Regression with Musician Status
# # Timing Score
# fit <- lm(TimingScore ~ Condition + Musician + Diagnosis, data=allData)
# print("Timing Score MLR")
# print(summary(fit))
# print(coefficients(fit))
# print(confint(fit, level=0.95))
# 
# # Endpoint Error
# fit <- lm(EndpointErrorScore ~ Condition + Musician + Diagnosis, data=allData)
# print("Endpoint Error MLR")
# print(summary(fit))
# print(coefficients(fit))
# print(confint(fit, level=0.95))
# 
# # Corrective Path Length
# fit <- lm(CPL ~ Condition + Musician + Diagnosis, data=allData)
# print("Corrective Path Length MLR")
# print(summary(fit))
# print(coefficients(fit))
# print(confint(fit, level=0.95))
# 
# # Direction Reversal
# fit <- lm(X..Dir.Rev ~ Condition + Musician + Diagnosis, data=allData)
# print("Direction Reversal MLR")
# print(summary(fit))
# print(coefficients(fit))
# print(confint(fit, level=0.95))


#####
# Multiple Linear Regression ONLY on non-standard condition

# Timing Score
fit <- lm(TimingScore ~ Musician + Diagnosis, data=nonStandardData)
print("Timing Score MLR")
print(summary(fit))
print(coefficients(fit))
print(confint(fit, level=0.95))

# Endpoint Error
fit <- lm(EndpointErrorScore ~ Musician + Diagnosis, data=nonStandardData)
print("Endpoint Error MLR")
print(summary(fit))
print(coefficients(fit))
print(confint(fit, level=0.95))

# Corrective Path Length
fit <- lm(CPL ~ Musician + Diagnosis, data=nonStandardData)
print("Corrective Path Length MLR")
print(summary(fit))
print(coefficients(fit))
print(confint(fit, level=0.95))

# Direction Reversal
fit <- lm(X..Dir.Rev ~ Musician + Diagnosis, data=nonStandardData)
print("Direction Reversal MLR")
print(summary(fit))
print(coefficients(fit))
print(confint(fit, level=0.95))


#####
# Descriptive statistics
descriptives <- summarySE(allData, measurevar="Age", groupvars=c("Condition","Musician"))
descriptives
descriptives <- summarySE(allData, measurevar="Musician_Years", groupvars=c("Condition","Musician"))
descriptives

mean(allData[allData$Condition == "standard" & allData$Musician == 0, ]$Musician_Years, na.rm = TRUE)
sd(allData[allData$Condition == "standard" & allData$Musician == 0, ]$Musician_Years, na.rm = TRUE)


#####
## Plots
# Library
library(tidyverse)
library(ggbeeswarm)

errorBarSize = 1

PlotTiming <- function(){
  
  pd <- position_dodge(0.1) # move them .05 to the left and right (use in situations where error bars overlap (e.g. geom_line(p)))

  # Timing Score
  dataErrors <- summarySE(allData, measurevar="TimingScore", groupvars=c("Condition","Musician"))
  dataErrors
  
  ggplot(dataErrors, aes(x=Musician, y=TimingScore, colour=Musician)) + 
    theme_minimal()+
    geom_errorbar(aes(ymin=TimingScore-ci, ymax=TimingScore+ci), width=.15, size = errorBarSize) +
    geom_point(size = errorBarSize*2) +
    geom_beeswarm(alpha = 0.15, size = 2, groupOnX = TRUE, dodge.width = 1, data = allData, colour = "black") +
    facet_wrap(~ Condition, ncol = 4) +
    labs(y = "Timing Score", x = NULL) +
    theme(legend.position = "none") +
    scale_x_discrete(breaks=c("0","1"),
                     labels= NULL)
}

PlotEndpointError <- function(){
  # Repeat for Endpoint Error Score
  dataErrors <- summarySE(allData, measurevar="EndpointErrorScore", groupvars=c("Condition","Musician"))
  dataErrors
  
  ggplot(dataErrors, aes(x=Musician, y=EndpointErrorScore, colour=Musician)) + 
    theme_minimal()+
    geom_errorbar(aes(ymin=EndpointErrorScore-ci, ymax=EndpointErrorScore+ci), width=.15, size = errorBarSize) +
    geom_point(size = errorBarSize*2) +
    geom_beeswarm(alpha = 0.15, size = 2, groupOnX = TRUE, dodge.width = 1, data = allData, colour = "black") +
    facet_wrap(~ Condition, ncol = 4) +
    labs(y = "Endpoint Error Score", x = NULL) +
    theme(legend.position = "none") +
    scale_x_discrete(breaks=c("0","1"),
                     labels= NULL)
}  

PlotCPL <- function(){
  # Repeat for CPL
  dataErrors <- summarySE(allData, measurevar="CPL", groupvars=c("Condition","Musician"))
  dataErrors
  
  ggplot(dataErrors, aes(x=Musician, y=CPL, colour=Musician)) + 
    theme_minimal()+
    geom_errorbar(aes(ymin=CPL-ci, ymax=CPL+ci), width=.15, size = errorBarSize) +
    geom_point(size = errorBarSize*2) +
    geom_beeswarm(alpha = 0.15, size = 2, groupOnX = TRUE, dodge.width = 1, data = allData, colour = "black") +
    facet_wrap(~ Condition, ncol = 4) +
    labs(y = "Corrective Path Length", x = NULL) +
    theme(legend.position = "none") +
    scale_x_discrete(breaks=c("0","1"),
                     labels= NULL)
}

PlotDirReversal <- function(){
  # Repeat for Direction Reversal
  dataErrors <- summarySE(allData, measurevar="X..Dir.Rev", groupvars=c("Condition","Musician"))
  dataErrors

  ggplot(dataErrors, aes(x=Musician, y=X..Dir.Rev, colour=Musician)) + 
    theme_minimal()+
    geom_errorbar(aes(ymin=X..Dir.Rev-ci, ymax=X..Dir.Rev+ci), width=.15, size = errorBarSize) +
    geom_point(size = errorBarSize*2) +
    geom_beeswarm(alpha = 0.15, size = 2, groupOnX = TRUE, dodge.width = 1, data = allData, colour = "black") +
    facet_wrap(~ Condition, ncol = 4) +
    labs(y = "Direction Reversal (%)", x = NULL) +
    theme(legend.position = "none", text=element_text(size=11,  family="sans")) +
    scale_x_discrete(breaks=c("0","1"),
                     labels= NULL)
}

# Plot the data
PlotTiming()
PlotEndpointError()
PlotCPL()
PlotDirReversal()


#####
# Below: distributions

# Timing Score distributions (might change this to be like endpoint error. i.e. not use facet_wrap)
ggplot(allData, aes(x = Musician, y = TimingScore)) +
  theme_minimal() +
  facet_wrap(~ Condition, ncol = 4) +
  geom_violin(aes(fill = Musician)) +
  labs(x = "Musician Status", y = "Timing Score")

# Endpoint Error Score distribution
ggplot(allData, aes(Condition, EndpointErrorScore)) +
  theme_minimal() +
  geom_violin(aes(fill = factor(Musician )))

# Repeat for peak velocity and reversal