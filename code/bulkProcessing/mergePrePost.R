source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
library("stringr")
library(data.table)
library(dplyr)

pre = fread("/Users/pdealcan/Documents/github/dataSabara/masterFile/masterFile.csv")
post = fread("/Users/pdealcan/Documents/github/dataSabara/masterFile/masterFilePOST.csv")

post = post %>% filter(Recording.name %in% unique(pre$Recording.name))

pre$prePos = "pre"
post$prePos = "post"

df = bind_rows(pre, post)
dfPrePost = df
