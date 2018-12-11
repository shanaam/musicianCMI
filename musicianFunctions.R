#############################
## MUSICIAN CMI EXPERIMENT ##
#############################

## BEFORE YOU  BEGIN
# If you havent already, uncomment and run the following lines of code to install required packages (only do this one time)
#install.packages("tidyverse") # for plotting / others
#install.packages("doBy") # for summary statistics function
#install.packages("eZ") # for ezANOVA
#install.packages("lsmeans") # for posthocs

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

# Set factors
allData$Condition <- as.factor(allData$Condition)
allData$Musician <- as.factor(allData$Musician)
allData$Sex <- as.factor(allData$Sex)
allData$Diagnosis <- as.factor(allData$Diagnosis)

# Descriptive Statistics

# Mean and SD for each composite z-score per condition
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

# test
allData <- allData[allData$Participant_ID != "MUS_AZ", ]

#set factor (categorical) variables as factors
allData <- setFactors(allData)

# Timing Score
timingScoreAOV <- ezANOVA(data=allData, dv=TimingScore, wid=Participant_Nr, within=Condition, between=Musician, return_aov=TRUE, type=3)
#timingScoreAOV$ANOVA$p <- roundANOVA(timingScoreAOV)
print("Timing Score ANOVA")
print(timingScoreAOV$ANOVA)


# Endpoint Error
endpointErrorAOV <- ezANOVA(data=allData, dv=EndpointErrorScore, wid=Participant_Nr, within=Condition, between=Musician, return_aov=TRUE, type=3)
#endpointErrorAOV$ANOVA$p <- roundANOVA(endpointErrorAOV)
print("Endpoint Error ANOVA")
print(endpointErrorAOV$ANOVA)


# corrected path length
CPLAOV <- ezANOVA(data=allData, dv=CPL, wid=Participant_Nr, within=Condition, between=Musician, return_aov=TRUE, type=3)
#CPL$ANOVA$p <- roundANOVA(CPLAOV)
print("Corrected Path Length ANOVA")
print(CPLAOV$ANOVA)

# direction reversal
DirRevAOV <- ezANOVA(data=allData, dv=X..Dir.Rev, wid=Participant_Nr, within=Condition, between=Musician, return_aov=TRUE, type=3)
#CPL$ANOVA$p <- roundANOVA(DirRevAOV)
print("Direction Reversal ANOVA")
print(DirRevAOV$ANOVA)


# Multiple Linear Regression
# Timing Score
fit <- lm(TimingScore ~ Condition + Musician_Years + Diagnosis, data=allData)
print("Timing Score MLR")
print(summary(fit))
print(coefficients(fit))
print(confint(fit, level=0.95))

# Endpoint Error
fit <- lm(EndpointErrorScore ~ Condition + Musician_Years + Diagnosis, data=allData)
print("Endpoint Error MLR")
print(summary(fit))
print(coefficients(fit))
print(confint(fit, level=0.95))

# Corrected Path Length
fit <- lm(CPL ~ Condition + Musician_Years + Diagnosis, data=allData)
print("Corrected Path Length MLR")
print(summary(fit))
print(coefficients(fit))
print(confint(fit, level=0.95))

# Direction Reversal
fit <- lm(X..Dir.Rev ~ Condition + Musician_Years + Diagnosis, data=allData)
print("Direction Reversal MLR")
print(summary(fit))
print(coefficients(fit))
print(confint(fit, level=0.95))




# Multiple Linear Regression with Musician Status
# Timing Score
fit <- lm(TimingScore ~ Condition + Musician + Diagnosis, data=allData)
print("Timing Score MLR")
print(summary(fit))
print(coefficients(fit))
print(confint(fit, level=0.95))

# Endpoint Error
fit <- lm(EndpointErrorScore ~ Condition + Musician + Diagnosis, data=allData)
print("Endpoint Error MLR")
print(summary(fit))
print(coefficients(fit))
print(confint(fit, level=0.95))

# Corrected Path Length
fit <- lm(CPL ~ Condition + Musician + Diagnosis, data=allData)
print("Corrected Path Length MLR")
print(summary(fit))
print(coefficients(fit))
print(confint(fit, level=0.95))

# Direction Reversal
fit <- lm(X..Dir.Rev ~ Condition + Musician + Diagnosis, data=allData)
print("Direction Reversal MLR")
print(summary(fit))
print(coefficients(fit))
print(confint(fit, level=0.95))





# Endpoint Error, with only the PC+FR condition
fit <- lm(EndpointErrorScore ~ Musician + Diagnosis, data=rawDataPCandFBR)
summary(fit)
coefficients(fit)
confint(fit, level=0.95)


#####
## Plots
# Library
library(tidyverse)

# Descriptive statistics
pd <- position_dodge(0.1) # move them .05 to the left and right (use in situations where error bars overlap (e.g. geom_line(p)))


# Timing Score
dataErrors <- summarySE(allData, measurevar="TimingScore", groupvars=c("Condition","Musician"))
dataErrors

ggplot(dataErrors, aes(x=Musician, y=TimingScore, colour=Musician)) + 
  theme_minimal()+
  geom_errorbar(aes(ymin=TimingScore-ci, ymax=TimingScore+ci), width=.1) +
  geom_line() +
  geom_point() +
  facet_wrap(~ Condition, ncol = 4)

# Repeat for Endpoint Error Score
dataErrors <- summarySE(allData, measurevar="EndpointErrorScore", groupvars=c("Condition","Musician"))
dataErrors

pd <- position_dodge(0.1) # move them .05 to the left and right

ggplot(dataErrors, aes(x=Musician, y=EndpointErrorScore, colour=Musician)) + 
  theme_minimal()+
  geom_errorbar(aes(ymin=EndpointErrorScore-ci, ymax=EndpointErrorScore+ci), width=.1) +
  geom_line() +
  geom_point() +
  facet_wrap(~ Condition, ncol = 4)

# Repeat for CPL
dataErrors <- summarySE(allData, measurevar="CPL", groupvars=c("Condition","Musician"))
dataErrors

pd <- position_dodge(0.1) # move them .05 to the left and right

ggplot(dataErrors, aes(x=Musician, y=CPL, colour=Musician)) + 
  theme_minimal()+
  geom_errorbar(aes(ymin=CPL-ci, ymax=CPL+ci), width=.1) +
  geom_line() +
  geom_point() +
  facet_wrap(~ Condition, ncol = 4)

# Repeat for Direction Reversal
dataErrors <- summarySE(allData, measurevar="X..Dir.Rev", groupvars=c("Condition","Musician"))
dataErrors

pd <- position_dodge(0.1) # move them .05 to the left and right

ggplot(dataErrors, aes(x=Musician, y=X..Dir.Rev, colour=Musician)) + 
  theme_minimal()+
  geom_errorbar(aes(ymin=X..Dir.Rev-ci, ymax=X..Dir.Rev+ci), width=.1) +
  geom_line() +
  geom_point() +
  facet_wrap(~ Condition, ncol = 4) +
  labs(y = "Direction Reversal %")



#####
# Below: distributions

# Timing Score distributions (might change this to be like endpoint error. i.e. not use facet_wrap)
p <- ggplot(allData, aes(x = Musician, y = TimingScore)) +
  theme_minimal() +
  facet_wrap(~ Condition, ncol = 4) +
  geom_violin(aes(fill = Musician)) +
  labs(x = "Musician Status", y = "Timing Score")
p

# Endpoint Error Score distribution
EndpointPlot <- ggplot(allData, aes(Condition, EndpointErrorScore)) +
  theme_minimal() +
  geom_violin(aes(fill = factor(Musician )))
# plot
EndpointPlot

# Repeat for peak velocity and reversal