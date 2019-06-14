
# Count the number of rnaseq abstracts related to cancer types, and plot the results.
# The cancer types are grouped by body location or system.
#-------cancer types list from :https://www.cancer.gov/types/by-body-location-----------------

library(assertr)
library(rentrez)

table = read.csv("C:/Users/Racheli/Documents/mini-project-bioinformatics/cancers-by-Body-Location-System.csv", header = TRUE, check.names = F)
table$Skin[8] = "Sezary Syndrome"
names(table)[1] = "Breast"
t = t(table)
t[t == ""] <- NA

#create query from each row
queryVector = col_concat(t, sep = "  ")
query_no_na <- gsub("NA", "", queryVector)
#trim spaces from the end
query_no_space_vector = trimws(query_no_na, which = "both" )
#add or between terms
queryVector <-gsub("  ", " OR ", query_no_space_vector, fixed=TRUE)

rnaseq_str = "(RNA seq[Title/Abstract] OR RNA-seq[Title/Abstract] OR rna sequencing[Title/Abstract] OR rnaseq[Title/Abstract]) "

# for each year, iterate all cancer types and count the number of relevent abstracts
typesCount = c()
for(y in 2010:2018)
{
  for(i in 1:length(queryVector))
  {
    query = paste(rnaseq_str," AND (", queryVector[i],") AND ", y, "[PDAT]", sep = "")
    count = entrez_search(db="pubmed", term=query, retmax=0)$count
    tmp = c(y,names(queryVector)[i],count)
    typesCount = rbind(typesCount, tmp)
  }
  print(typesCount)
}

# create data frame
colnames(typesCount) <- c("Year", "Type", "Value")
df <- as.data.frame(typesCount)
df[,3] <- as.numeric(as.character(df[,3]))

#colors for plot
m_colors = c("#df4a7a", "#c97b7a","#de5137","#d08935", "#a78d57", "#d2d23e","#67993f",
             "#76d854", "#66db9f", "#529477", "#6387d7", "#777ba7", "#b159e0", "#d6aad9",
             "#bd6cac","#db49ba")

# plot the results
pdf("cancerByOrgan.pdf", width = 13, height = 10)

theme_set(theme_classic())

g <- ggplot(df, aes(x = Year, y = Value))
g + geom_bar(aes(fill=fct_reorder(Type, Value, sum, desc=TRUE)), width = 0.5, stat="identity") + 
  theme(axis.title = element_text(family = "Times", face = "italic", size = 25),
        plot.title = element_text(family = "Times", face = "bold", size = 30),
        axis.text.x = element_text(angle=0, vjust=0.6), 
        axis.text =   element_text(family = "Times",size = 20),
        legend.text = element_text(family = "Times",size = 22),
        legend.title = element_text(family = "Times",size = 25),
        legend.key.size = unit(0.9, "cm")) +
  scale_fill_manual(values=m_colors) +
  labs(title="Cancer Locations", 
       subtitle="", fill = "Location/System", y = "Number Of Abstracts", x = "Year",
       family = "Times", face = "italic") 


dev.off()


m_colors = c("brown4", "brown3", "chocolate1","yellow2", "lemonchiffon", "lightgreen",  "green3",
             "seagreen4", "royalblue4","lightslateblue",
             "lightskyblue",  "mediumorchid2", "magenta4",
             "maroon2", "plum1")




