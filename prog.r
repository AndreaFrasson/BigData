rm(list = ls())

source("Normalizzare.r")
library(tm)
library(tibble)


SMSSpamCollection <- read.delim("smsspamcollection/SMSSpamCollection.txt", 
                                header=FALSE, quote = "")
normalizzare(SMSSpamCollection$V2, stopwords = stopwords("en"))

