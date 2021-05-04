rm(list = ls())
setwd("C:/Users/and99/Desktop/progetto BIDATA/")

source("Normalizzare.r")
library(tm)
library(tibble)


SMSSpamCollection <- read.delim("smsspamcollection/SMSSpamCollection.txt", 
                                header=FALSE, quote = "")
normalizzare(SMSSpamCollection$V2, stopwords = stopwords("en"))

