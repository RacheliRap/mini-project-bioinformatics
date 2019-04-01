library("RISmed")
library("pubmed.mineR")
library(tidytext)
library(dplyr)
library(rentrez)

make_Abstracts = function(i){
  Abstracts(
    Abstract = fetch@AbstractText[i],
    PMID = fetch@NlmUniqueID[i]
  ) -> myabs
  
}

res1 <- EUtilsSummary("RNASeq| RNA + seq| rnaseq | RNA-seq", 
                      type = "esearch", 
                      db = "pubmed",
                      datetype = "pdat",
                      retmax = 1000,
                      mindate = 2016, 
                      maxdate = 2018)
fetch <- EUtilsGet(res1, type = "efetch", db = "pubmed")

table = read.csv("MINI-PROJ_methods_and_companies_classification-key_words.csv", header = TRUE, sep = ",", quote = "\"",
                 dec = ".", fill = TRUE, comment.char = "", stringsAsFactors = FALSE)

companies = table[,1]
research = table[,2]
count_companies = 0
count_research = 0
c=0
companies_list = list()
research_list = list()

#### affiliations with companies terms ###
for (i in 1:length(fetch@Affiliation)) {
  for (j in 1:length(companies)) {
    if (c == 0)
    {
      llist = gregexpr(companies[j], fetch@Affiliation[[i]])
      len = length(llist)
      if (len != 0) {
        for (w in 1:length(llist)) {
          if (llist[[w]][1] != -1) {
            c = c + 1
          }
        }
      }
    }
    if (c!=0) {
      count_companies = count_companies+1
      companies_list=c(companies_list,fetch@Affiliation[[i]])
    }
    c=0
  }
}

#### affiliations with research terms ###
for (i in 1:length(fetch@Affiliation)) {
  for (j in 1:length(research)) {
    if (c == 0)
    {
      llist = gregexpr(research[j], fetch@Affiliation[[i]])
      len = length(llist)
      if (len != 0) {
        for (w in 1:length(llist)) {
          if (llist[[w]][1] != -1) {
            c = c + 1
          }
        }
      }
    }
    if (c!=0) {
      count_research = count_research+1
      research_list=c(research_list,fetch@Affiliation[[i]])
    }
    c=0
  }
}





####nevermind
for (l in 1:length(fetch@Affiliation)) {
  for (k in 1:length(research)) {
    count_research=sapply(gregexpr(research[k], fetch@Affiliation[[l]]), function(x) sum(x > 0))
  }
}

for (i in 1:length(fetch@Affiliation)) {
  if (fetch@Affiliation[[i]] %in% companies) {
    count_companies = count_companies+1
  }
  else if (fetch@Affiliation[i] %in% research) {
    count_research = count_research+1
  }
  
}


query <- "RNA-seq"
year <- 2019
query <- paste(term, "AND (", year, "[PDAT])")
r_search <- entrez_search(db="pubmed", term=query, retmax=200)
ids <- r_search$ids
abs <- entrez_fetch(db="pubmed", id=ids, rettype= "xml")
write(abs, "results.xml")
myabs <- xmlreadabs( "results.xml")
init_Abstract_object <- function(abstract, mPMID)
{
  if(is.numeric(mPMID) == FALSE)
  {
    mPMID = 0
  }
  s <- new("Abstracts", Abstract = abstract, PMID = mPMID)
}
#s = lapply(1:20, FUN = init_Abstract_object, abstract = fetch@AbstractText[x], mPMID = fetch@PMID[x])

v = character()
for(i in 1:20)
{
  s = init_Abstract(fetch@AbstractText, fetch@PMID)
  v = c(v ,s)
}



