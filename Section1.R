#Part 1 Section 1 Assignment

#Load Libraries
library(readxl)
library(dplyr)

#import the line list
linelist <- read_excel("./linelist_raw.xlsx")

#confirm Line list dimension
dim(linelist)

#confirm Line list column and data types types
str(linelist)

#summarise Line list
summary(linelist)


