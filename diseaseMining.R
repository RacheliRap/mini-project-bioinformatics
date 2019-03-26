library(rentrez)
library("pubmed.mineR")
library(stringdist)


diseases_freq<- function(myear) 
{
  term <- "RNA-seq"
  year <- myear
  query <- paste(term, "AND (", year, "[PDAT])")
  r_search <- entrez_search(db="pubmed", term=query, retmax=1000)
  ids <- r_search$ids
  
  #send each id to pubtator_function to get information 
  d = lapply(ids[1:20], pubtator_function)
  
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
  
  
  # Group selection by class numbers or height 
  num.class <- 5;
  num.height <-0.5;
  
  # calculate distances
  d <- stringdistmatrix(v, method="soundex");
  
  # cluster the distance matrix
  h <- hclust(d);
  
  # cut the cluster by num classes
  m <- cutree(h, k = num.class)
  
  # cut the cluster by height
  p <- cutree(h, h = num.height)
  
  # build the resulting frame
  mdf <- data.frame(names = v, 
                    group.class = m, 
                    group.prob = p)
  
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
    #initalize the data frame
    mat[i,1] = disease_name
    mat[i,2] = count
  }
  #order the data frame by the size of th clusters
  mat = mat[order(mat$nrow),]
  numRow <- nrow(mat)
  #extract 10 most popular diseases
  topDiseases <- mat[(numRow-3):numRow,]
  freq <- topDiseases$nrow
  names(freq) = topDiseases$ncol
  barplot(freq,main=year,ylab="Amount",las=2)
}


pdf("topDiseases_perYear.pdf")
for (i in 2010:2019)
{
  diseases_freq(i)
}
dev.off()


