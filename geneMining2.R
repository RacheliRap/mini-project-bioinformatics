library(rentrez)
library(RISmed)
library(pubmed.mineR)
library(ggplot2)
library(forecast)


getAbs<- function(id)
{
  abs <- entrez_fetch(db="pubmed", id=id, rettype= "text")
  return(abs)
}

for(y in 2010:2018)
{
  term <- "RNA-Seq"
  year <- y
  query <- paste(term, "AND (", y, "[PDAT])")
  r_search <- entrez_search(db="pubmed", term=query, retmax=10000)
  ids <- r_search$ids
  abs <- sapply(ids, function(x) getAbs(x))
  fileName <- paste("pubmedAbs_rnaSeq_",y, ".txt", sep = "")
  write(abs, file = fileName)
  myabs <- readabs(fileName)
 
  
}
genesByYear = c()
genesNames <- read.table("geneNames_refseq_final.txt")
ls = as.vector(genesNames[,1])



for(i in 2010:2019)
{
  fileName <- paste("pubmedAbs_rnaSeq_",i, ".txt", sep = "")
  myabs <- readabs(fileName)
  geneFreq <- gene_atomization(myabs)
  v <- c()
  v = c(ifelse(length(as.numeric(geneFreq[geneFreq[,1] == "SOX2",3])) > 0
                 ,as.numeric(geneFreq[geneFreq[,1] == "SOX2",3]), 0),
        ifelse(length(as.numeric(geneFreq[geneFreq[,1] == "GC",3])) > 0 
                      ,as.numeric(geneFreq[geneFreq[,1] == "GC",3]), 0),
        ifelse(length(as.numeric(geneFreq[geneFreq[,1] == "T",3])) > 0,
                       as.numeric(geneFreq[geneFreq[,1] == "T",3]), 0),
        ifelse(length(as.numeric(geneFreq[geneFreq[,1] == "TP53",3])) > 0,
                       as.numeric(geneFreq[geneFreq[,1] == "TP53",3]), 0),
        ifelse(length(as.numeric(geneFreq[geneFreq[,1] == "GATA6",3])) >0,
                       as.numeric(geneFreq[geneFreq[,1] == "GATA6",3]), 0),
        ifelse(length(as.numeric(geneFreq[geneFreq[,1] == "CS",3])) >0,
                      as.numeric(geneFreq[geneFreq[,1] == "CS",3]), 0),
        ifelse(length(as.numeric(geneFreq[geneFreq[,1] == "EGFR",3])) > 0,
                      as.numeric(geneFreq[geneFreq[,1] == "EGFR",3]), 0),
        ifelse(length(as.numeric(geneFreq[geneFreq[,1] == "DUX4",3])) > 0,
                      as.numeric(geneFreq[geneFreq[,1] == "DUX4",3]), 0))
  
  genesByYear <- cbind(genesByYear, v)
  
}
rownames(genesByYear) <- c("SOX2", "GC", "T", "TP53", "GATA6", "CS", "EGFR", "DUX4")

colnames(genesByYear) <- 2010:2019
tmp = genesByYear[-c(3),]


require(reshape2)
df <- data.frame(x=1:nrow(t(tmp)), (data.frame(t(tmp))))
df.melted <- melt(df, id="x")
df.melted$x = rep(2010:2019, times = 7)
require(ggplot2)
#qplot(x=x, y=value, color=variable, data=df.melted, geom="line", xlim = c(2010, 2019))
pdf("genes-perYear.pdf", width = 12, height = 10)
ggplot(df.melted, aes(x=x, y = value, group = variable, colour = variable)) + geom_line() +
  scale_x_discrete(name ="year",limits=2010:2019)
dev.off()









