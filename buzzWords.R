library(pubmed.mineR)
library(rentrez)

term <- "RNA-seq"
query <- paste(term, "AND (", 2018, "[PDAT])")
r_search <- entrez_search(db="pubmed", term=query, retmax=100)
ids <- r_search$ids
abs <- entrez_fetch(db="pubmed", id=ids, rettype= "text")
write(abs,file = "results.txt")
myabs <- readabs("results.txt")
m = cleanabs(myabs)


term <- "RNA-seq"
query <- paste(term, "AND (", 2015:2017, "[PDAT])")
r_search <- entrez_search(db="pubmed", term=query, retmax=100)
ids <- r_search$ids
abs1 <- entrez_fetch(db="pubmed", id=ids, rettype= "text")
write(abs1,file = "results1.txt")
myabs1 <- readabs("results1.txt")
m1 = cleanabs(myabs1)

