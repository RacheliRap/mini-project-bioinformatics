library(rentrez)
library("pubmed.mineR")
library(stringdist)

start <- Sys.time()
print(start)

#get abstracts ids from pubmed by term and year
getAbstracts <- function(myear) 
{
  term <- "RNA-seq"
  year <- myear
  query <- paste(term, "AND (", year, "[PDAT])")
  r_search <- entrez_search(db="pubmed", term=query, retmax=50)
  ids <- r_search$ids
}

#get diseases from each abstract
getDiseases <- function(ids)
{
  #send each id to pubtator_function to get information 
  d = lapply(ids, pubtator_function)
  
  #create empty vector
  v <- c()
  
  for (i in 1:length(d))
  {
    #if the function return information about the abstract
    if(d[[i]] != " No Data ")
    {
      #add the abstracts diseases to vector c
      v <- c(v,getElement(d[[i]], "Diseases"))
    }
  }
  return(v)
} 

#group simiiler diseases togther
groupDiseases <- function(v) {
  
  # Group selection by class numbers or height 
  num.class <- 5;
  num.height <-0.5;
  
  # calculate distances
  dv <- stringdistmatrix(v, method="soundex");
  
  # cluster the distance matrix
  h <- hclust(dv);
  
  # cut the cluster by num classes
  m <- cutree(h, k = num.class)
  
  # cut the cluster by height
  p <- cutree(h, h = num.height)
  
  # build the resulting frame
  mdf <- data.frame(names = v, 
                    group.class = m, 
                    group.prob = p)
}

#count the size of each diseases group
countGroups <- function(mdf)
{
  #create new data frame for all diseases
  mat = data.frame(ncol = max(mdf[,3]), nrow  = 2 )
  #group togther the diseases in the same cluster and count the size of each cluster
  for( i in 1:max(mdf[,3]))
  {
    count = 0 
    
    for (j in 1:nrow(mdf))
    {
      if(mdf[j,3] == i)
      {
        disease_name = as.character(mdf[j,1])
        count = count + 1
      }
      
    }
    #initalize the data frame withthe count
    mat[i,1] = disease_name
    mat[i,2] = count
  }
  #order the data frame by the size of the clusters
  mat = mat[order(mat$nrow),]
  
}


allDiseases = c()

pdf("topDiseases1_perYear.pdf")

for (i in 2010:2019)
{
  ids = getAbstracts(i)
  v <- getDiseases(ids)
  mdf <- groupDiseases(v)
  mat <- countGroups(mdf)
  allDiseases <- c(allDiseases, mat[,1])
  
  mid <- Sys.time() - start # calculate running time
  print(mid)
  
  #extract 10 most popular diseases
  topDiseases <- mat[(nrow(mat)-5):nrow(mat),]
  freq <- topDiseases$nrow
  names(freq) = topDiseases$ncol
  
  barplot(freq,main=year,ylab="Amount",las=2)
}
dev.off()

end <- Sys.time() - start # calculate running time

print(paste("genral runing time:" , end))

names(allDiseases) <- 2010:2019
write.csv(allDiseases, file = "allDiseasesTable_perYear.csv")
