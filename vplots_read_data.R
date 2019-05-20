# Read in data from a CSV

# sample data contains the following columns:
#  [1] "subject"  "word"     "vowel"    "start"    "end"      "duration" "f0_20"    "f0_50"    "f0_80"    "f1_20"    "f1_40"    "f1_50"    "f1_60"   
# [14] "f1_80"    "f2_20"    "f2_40"    "f2_50"    "f2_60"    "f2_80"    "f3_20"    "f3_40"    "f3_50"    "f3_60"    "f3_80"    "age"      "sex"     
# [27] "f1_norm"  "f2_norm"  "f3_norm"  "v_length"

# Minimally, your data should have the following columns ("duration", "f0_50", and "f3_50" are optional as well, if you only want to plot F1 ~ F2):
#  [1] "subject"  "word"     "vowel"    "duration" "f0_50""f1_50"    "f2_50"    "f3_50"

# You should also include columns for any additional categorical information you have about your data (for example, tone or phonemic length)

data <- read.csv("voweldata.csv", header=T, encoding = "UTF-8")

# Check that all vowel symbols were read in correctly
unique(data$vowel)

# Use regular expressions and gsub() to correct any mis-read values
data$vowel <- gsub("?", "ɨ", data$vowel, fixed=TRUE) # fixed=TRUE means that characters should be interpreted literally, not using regex interpretations
data$vowel <- gsub("<U+0259>", "ə", data$vowel, fixed=TRUE)

# Make sure that the vowel column acts as a factor, not as character strings (do the same for any other categorical variables in your data set)
data$vowel <- as.factor(data$vowel)

# Look at the first six rows to make sure that everything looks okay
head(data)

# Use summary() to get a summary of the entire dataframe, or a single column
summary(data$vowel)
