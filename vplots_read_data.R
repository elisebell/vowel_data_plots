require(ggplot2)
require(ggpubr)
require(plyr)
require(dplyr)
require(reshape2)

# Read in data from a CSV
data <- read.csv("voweldata.csv", header=T, encoding = "UTF-8")
data$vowel <- gsub("?", "ɨ", data$vowel, fixed=TRUE)
data$vowel <- gsub("<U+0259>", "ə", data$vowel, fixed=TRUE)


data$vowel <- as.factor(data$vowel)

head(data)
summary(data$vowel)
