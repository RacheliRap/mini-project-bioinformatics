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
  r_search <- entrez_search(db="pubmed", term=query, retmax=5000)
  ids <- r_search$ids
}

#get genes from each abstract
getGenes <- function(ids)
{
  #send each id to pubtator_function to get information 
  d = lapply(ids, pubtator_function)
  
  #create empty vector
  v <- c()
  
  for (i in 1:length(d))
  {
    #if the function return information about the abstract
    if(d[[i]] != " No Data " )
    {
      
      #add the abstracts diseases to vector c
      v <- c(v,getElement(d[[i]], "Genes"))
    }
  }
  return(v)
} 

#group simiiler genes togther
groupGenes <- function(v) {
  
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
  print(mdf)
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
        gene_name = as.character(mdf[j,1])
        count = count + 1
      }
      
    }
    #initalize the data frame withthe count
    mat[i,1] = gene_name
    mat[i,2] = count
  }
  #order the data frame by the size of the clusters
  mat = mat[order(mat$nrow),]
  
}


allGenes = c()

pdf("topGenes_perYear.pdf", height = 9, width = 12)

for (i in 2010:2019)
{
  ids = getAbstracts(i)
  v <- getGenes(ids)
  mdf <- groupGenes(v)
  mat <- countGroups(mdf)
  newGenes <- rev(mat[,1])
  allGenes <- cbind(allGenes,newGenes)
  print(allGenes)
  
  mid <- Sys.time() - start # calculate running time
  print(mid)
  
  #extract 10 most popular diseases
  topGenes <- mat[(nrow(mat)-7):nrow(mat),]
  freq <- topGenes$nrow
  names(freq) = topGenes$ncol
  
  barplot(freq,main=i,ylab="Amount",las=2)
}
dev.off()

#end <- Sys.time() - start # calculate running time

#print(paste("general running time:" , end))
allGenes = data.frame(allGenes)
names(allGenes) <- 2010:2019
write.csv(allGenes, file = "allGenesTable_perYear.csv", col.names = TRUE)
