library("RISmed")
library("pubmed.mineR")
library(rentrez)


make_Abstracts = function(i){
  Abstracts(
    Abstract = fetch@AbstractText[i],
    PMID = fetch@NlmUniqueID[i]
  ) -> myabs
 
}


res1 <- EUtilsSummary("data + science, population + health", 
                      type = "esearch", 
                      db = "pubmed",
                      datetype = "pdat",
                      retmax = 20,
                      mindate = 2015, 
                      maxdate = 2016)
fetch <- EUtilsGet(res1, type = "efetch", db = "pubmed")
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



