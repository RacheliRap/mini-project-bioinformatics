library(ggplot2)
library(dplyr)
library(rentrez)
#----------------create graph for All published articles pre year VS. All RNA related articles per year-LOGARYTHMIC SCALE---------------------------

#Initialize count:
artCount = c()

#Find all published articles per year:
for(y in 2010:2018)
{
  query = paste( y, "[PDAT]", sep = "")
  count = entrez_search(db="pubmed", term=query, retmax=0)$count
  tmp = c(y, "All",count)
  artCount = rbind(artCount, tmp)
  print(artCount)
}

#Initialize count:
rnaArtCount = c()

#Find all RNA related published articles per year:
for(y in 2010:2018)
{
  query = paste( "(RNA-Seq[Title/Abstract]|RNAseq[Title/Abstract]|RNA seq[Title/Abstract]|RNA sequencing[Title/Abstract]|RNAsequencing[Title/Abstract]|RNA-sequencing[Title/Abstract]) AND ", y, "[PDAT]", sep = "")
  count = entrez_search(db="pubmed", term=query, retmax=0)$count
  tmp = c(y, "RNA-seq",count)
  artCount = rbind(artCount, tmp)
  print(artCount)
}

#Create dataframe for the graph:
colnames(artCount) <- c("Year", "Kind", "Value")
df <- as.data.frame(artCount)
df[,3] <- as.numeric(as.character(df[,3]))
df[,1] <- as.numeric(as.character(df[,1]))
df[,3] <- log(df[,3])


pdf("All Articles VS. RNA-seq Articles-logarithmic scale.pdf", width = 13, height = 10)

theme_set(theme_classic())

ggplot(df, aes(x=df[,1], y=df[,3], col = Kind)) + 
  theme(axis.text = element_text(family = "Times", size = 20),
        axis.text.x = element_text(family = "Times", vjust=0.6),
        axis.title = element_text(family = "Times", face = "italic", size = 25),
        plot.title = element_text(family = "Times", face = "bold", size = 30),
        legend.text = element_text(family = "Times", size = 25))+
  scale_fill_manual(values=m_colors) +
  geom_point(size=6, alpha=0.6)+ geom_path()+
  labs(title="All Articles VS. RNA-seq Articles (logarithm scale)", 
       subtitle="",
       y = "Number of Articles",
       x = "Years")

dev.off()