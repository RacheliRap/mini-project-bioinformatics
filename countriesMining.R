library("RISmed")
library("pubmed.mineR")

start <- Sys.time()


#return table with the count of countries in abstracts
getCountries <- function(myear) {
  
  term = "RNA-Seq"
  year = myear
  res1 <- EUtilsSummary(term, 
                        type = "esearch", 
                        db = "pubmed",
                        datetype = "pdat",
                        retmax = 5000,
                        mindate = year, 
                        maxdate = year)
  fetch <- EUtilsGet(res1, type = "efetch", db = "pubmed")
  
  table = as.data.frame(table(fetch@Country))
  return(table)
  
}

freq_perCountry <- function(table, china, usa, uk, japan, swiss) {
  
  v <- c()
  tmp = 0
  tmp1 = 0
  if(length(table[table == "United Kingdom"] > 0) )
  {
    #v <- c(v, table[table == "United Kingdom" ,2])
    tmp = table[table == "United Kingdom" ,2]
  }
  if(length(table[table == "England"] > 0))
  {
    tmp = tmp + table[table == "England" ,2]
    
  }
  
  v <- c(v,tmp) 
  
  if(length(table[table == "United States"] > 0 ))
  {
    #v <- c(v, table[table == "United States" ,2] )
    tmp1 = table[table == "United States" ,2]
  }
  
  if(length(table[table == "USA"] > 0 ))
  {
    #v <- c(v, table[table == "USA" ,2] )
    tmp1 = tmp1 +  table[table == "USA" ,2] 
  }
  
  v <- c(v, tmp1)
  
  if(length(table[table == "China",2] ) > 0)
  {
    v <- c(v, table[table == "China",2] )
  }
  else
  {
    v <- c(v, 0)
  }
  #if(length(table[table == "Japan",2]) > 0 )
  #{
   # v <- c(v, table[table == "Japan",2] )
  #}
  #else
  #{
   # v <- c(v, 0)
  #}
  if(length(table[table == "Switzerland",2]) > 0 )
  {
    v <- c(v, table[table == "Switzerland", 2])
  }
  else
  {
    v <- c(v, 0)
  }
  if(length(table[table == "Netherlands",2]) > 0 )
  {
    v <- c(v, table[table == "Netherlands", 2])
  }
  else
  {
    v <- c(v, 0)
  }
  if(length(table[table == "Germany",2]) > 0 )
  {
    v <- c(v, table[table == "Germany", 2])
  }
  else
  {
    v <- c(v, 0)
  }
  
  
  return(v)
}


#empty vectors for each contry
china <- c()
usa <- c()
uk <- c()
japan <- c()
swiss <- c()
df <- c()

pdf("countries2_barPlots.pdf",width = 12, height = 8)
years <- 2010:2019

for (i in 2010:2019)
{
  table <- getCountries(i)
  freq = table$Freq
  names(freq) = table$Var1
  barplot(freq,main=i,ylab="Amount",las=2)
  
  v = freq_perCountry(table)
  df <- rbind(df,v)
  
}

dev.off()

pdf("countries_all.pdf", width = 12, height = 9)
df <- as.data.frame(df)

colnames(df) <- c("U.K","U.S.A", "China",  "Switzerland", "Netherlands", "Germany")
matplot (2010:2019, df, pch = 19, type = "l", xlab = "years" ,ylab = "countries", main = "RNA-Seq per country")
nn <- ncol(df)
legend("right", colnames(df),col=seq_len(nn),cex=0.8,fill=seq_len(nn))
dev.off()
end <- Sys.time() - start # calculate running time
print(end)
