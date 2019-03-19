install.packages("assertr")
install.packages("rentrez")

library(assertr)
library(rentrez)

#--------------serch the a term + year in pubmed, return the number of ------------------
#-----------------------articels published in the same year------------------------------
search_year <- function(year, term){
  
  query <- paste(term, "AND (", year, "[PDAT])")
  entrez_search(db="pubmed", term=query, retmax=0)$count
}

table = read.csv("C:/Users/Racheli/Documents/MINI-PROJ_ methods and companies classification - By methods.csv", header = TRUE, sep = ",", quote = "\"",
         dec = ".", fill = TRUE, comment.char = "")
#delete irrelevent columns
table = table[,-c(1,7,8)]
as.data.frame.matrix(table) 

#create query from each row
queryVector = col_concat(table, sep = "  ")
#trim spaces from the end
query_no_space_vector = trimws(queryVector, which = "right" )
#add or between terms
queryVector <-gsub("  ", " OR ", query_no_space_vector, fixed=TRUE)

#plot all the result from the function into pdf file
pdf("pubMed_search_results.pdf", width = 10)
for(i in 1:length(queryVector))
{
year <- 2010:2018
results <- sapply(year, search_year, term=queryVector[[i]], USE.NAMES=FALSE)
n = length(year)
plot(year,results, main = queryVector[[i]], xaxt = "n", type = "b", ylab = "Number of articels")
axis(side=1, at=year, labels=2010:2018, cex.axis=0.9)
}
dev.off()




#
pdf("pubmed_search_contries.pdf")
contries <- c("China", "Australia", "U.K OR united kingdom", "U.S.A or united states", "Spain", "Franch")
df <- matrix(, nrow=6, ncol=10)
for(i in 1:2)
{
  for(c in contries)
  {
    results <- sapply(year, search_year, term=paste(queryVector[[i]], "AND", c), USE.NAMES=FALSE)
    df = rbind(df, results)
  }
  plot(df)
  
}
dev.off()