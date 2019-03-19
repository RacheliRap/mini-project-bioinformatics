library("RISmed")
library("pubmed.mineR")
library(rentrez)

res1 <- EUtilsSummary("data + science, population + health", 
                      type = "esearch", 
                      db = "pubmed",
                      datetype = "pdat",
                      retmax = 20,
                      mindate = 2015, 
                      maxdate = 2016)
fetch <- EUtilsGet(res1, type = "efetch", db = "pubmed")



