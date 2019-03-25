library("RISmed")
library("pubmed.mineR")


#return table with the count of countries in abstracts
getCountries <- function(myear) {

term = "RNA-Seq"
year = myear
res1 <- EUtilsSummary(term, 
                      type = "esearch", 
                      db = "pubmed",
                      datetype = "pdat",
                      retmax = 1000,
                      mindate = year, 
                      maxdate = year)
fetch <- EUtilsGet(res1, type = "efetch", db = "pubmed")

table = as.data.frame(table(fetch@Country))
return(table)

}

check_count <- function(table, countries, countryVector) {
  
  if(length(table[is.element(table, countries),2]) > 0 )
  {
    
  }
  
}

pdf("countries2_barPlots.pdf",width = 12)
years <- 2010:2019
#empty vectors for rach contry
china <- c()
us <- c()
uk<- c()
japan <- c()
swiss <- c()

for (i in 2010:2019)
{
  table <- getCountries(i)
  freq = table$Freq
  names(freq) = table$Var1
  barplot(freq,main=i,ylab="Amount",las=2)
  #if(length(table[table == "United Kingdom" || table == "England",2]) > 0)
  #{
   #uk <- c(uk, table[table == "United Kingdom" || table == "England",2])
  #}
  #else
  #{
   # uk <- c(uk,0)
  #}
  if(length(table[table == "United States" ,2 || table == "U.S.A"]) > 0 )
  {
    us <- c(us, table[table == "United States" ,2] )
  }
  else
  {
    us <- c(us, 0)
  }
  if(length(table[table == "China",2] ) > 0)
  {
    china <- c(china, table[table == "China",2] )
  }
  else
  {
    china <- c(china, 0)
  }
  if(length(table[table == "Japan",2]) > 0 )
  {
    japan <- c(japan, table[table == "Japan",2] )
  }
  else
  {
    japan <- c(japan, 0)
  }
  if(length(table[table == "Switzerland",2]) > 0 )
  {
    swiss <- c(swiss, table[table == "Switzerland", 2])
  }
 else
  {
    swiss <- c(swiss, 0)
  }
}

dev.off()

pdf("countries_all.pdf")
df <- cbind( us, china, japan, swiss)
df <- as.data.frame(df)
colnames(df) <- c("U.S", "China", "Japan", "Switzerland")
matplot (2010:2019,cbind(japan, us, china, swiss), pch = 19, type = "l", xlab = "years" ,ylab = "countries")
nn <- ncol(df)
legend("right", colnames(df),col=seq_len(nn),cex=0.8,fill=seq_len(nn))
dev.off()

