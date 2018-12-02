#############################
## MUSICIAN CMI EXPERIMENT ##
#############################

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
allData <- rbind(rawDataStandard, rawDataPC, rawDataFBR, rawDataPCandFBR)

#####
# General Functions


#####
## Statistics

# Descriptive Statistics

# Median and SD for each composite z-score per condition
# Standard condition
meanTimingScoreStandard <- mean(rawDataStandard[rawDataStandard$Musician == 1, ]$TimingScore)
sdTimingScoreStandard <- sd(rawDataStandard[rawDataStandard$Musician == 1, ]$TimingScore)
meanTimingScorePC <- mean(rawDataPC[rawDataPC$Musician == 1, ]$TimingScore)
sdTimingScorePC <- sd(rawDataPC[rawDataPC$Musician == 1, ]$TimingScore)
meanTimingScoreFBR <- mean(rawDataFBR[rawDataFBR$Musician == 1, ]$TimingScore)
sdTimingScoreFBR <- sd(rawDataFBR[rawDataFBR$Musician == 1, ]$TimingScore)
meanTimingScorePCandFBR <- mean(rawDataPCandFBR[rawDataPCandFBR$Musician == 1, ]$TimingScore)
sdTimingScorePCandFBR <- sd(rawDataPCandFBR[rawDataPCandFBR$Musician == 1, ]$TimingScore)



#####
## Plots
# Library
library(ggplot2)

# Descriptive statistics

# Timing Score
timingScorePlot <- ggplot(allData, aes(Condition, TimingScore)) +
  geom_violin(aes(fill = factor(Musician )))
# plot
timingScorePlot

# Endpoint Error Score
EndpointPlot <- ggplot(allData, aes(Condition, EndpointErrorScore)) +
  geom_violin(aes(fill = factor(Musician )))

# plot
EndpointPlot