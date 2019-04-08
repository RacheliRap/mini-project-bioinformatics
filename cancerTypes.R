library(RISmed)
library(magrittr)
library(dplyr)
library(wordcloud)
library(tidytext)
library(knitr)
library(tm)
library(topicmodels)
library(MASS)


#------------------------------create pie chart for cancer type---------------------------
res1 <- EUtilsSummary("RNA-seq", 
                      type = "esearch", 
                      db = "pubmed",
                      datetype = "pdat",
                      retmax = 5000,
                      mindate = i, 
                      maxdate = i)

fetch <- EUtilsGet(res1, type = "efetch", db = "pubmed")


abstracts <- data.frame(title = fetch@ArticleTitle,
                        abstract = fetch@AbstractText, 
                        journal = fetch@Title,
                        DOI = fetch@PMID, 
                        year = fetch@YearPubmed)
## ensure abstracts are character fields (not factors)
abstracts <- abstracts %>% mutate(abstract = as.character(abstract))
abstracts %>%
  head()
cancersCount = c()

cancerAbs <- abstracts[grepl("cancer|tumor",  abstracts$abstract),]
cancerAbs$abstract <- sapply(cancerAbs$abstract, tolower)


cancerTypes = c("carcinoma|breast|prostate|lung|pancreas|colon", 
                "sarcoma", "lymphoma", "leukemia", "germ cell tumor|ovary|ovarien|testicle", 
                "blastoma", "hepatocarcinoma", 
                "leiomyoma", "seminoma")
for(i in 1:length(cancerTypes))
{
g <- cancerAbs[grepl(cancerTypes[i],  abstracts$abstract),]
g1 <- g$DOI %>% list
abstracts <- abstracts %>% 
  mutate(DOI  = as.character(DOI))

tmp = c(cancerTypes[i],dim(g)[1])
cancersCount = rbind(cancersCount, tmp)

}
colnames(cancersCount) <- c("cancerType", "value")
rownames(cancersCount)<- 1:length(cancerTypes)
df <- as.data.frame(cancersCount)
#colnames(df) <- c("cancer type", "count")


types = c("carcinoma", 
                "sarcoma", "lymphoma", "leukemia", "germ cell tumor", 
                "blastoma", "hepatocarcinoma", 
                "leiomyoma", "seminoma")

pdf("cancerTypes_pieChart_2018.pdf")

library(ggplot2)

# Create a basic bar
pie = ggplot(df, aes(x="", y=value, fill=types)) + geom_bar(stat="identity", width=1)

# Convert to pie (polar coordinates) and add labels
pie = pie + coord_polar("y", start=0) + geom_text(aes(label = ""), position = position_stack(vjust = 0.5))

# Add color scale (hex colors)
#pie = pie + scale_fill_manual(values=c("#55DDE0", "#33658A", "#2F4858", "#F6AE2D", "#F26419", "#999999")) 

# Remove labels and add title
pie = pie + labs(x = NULL, y = NULL, fill = NULL, title = "Cancer Types - 2018")

# Tidy up the theme
pie = pie + theme_classic() + theme(axis.line = element_blank(),
                                    axis.text = element_blank(),
                                    axis.ticks = element_blank(),
                                    plot.title = element_text(hjust = 0.5, color = "#666666"))
pie
dev.off()

#--------------------------create cumulative bar graph for cancer types----------------------


library(ggplot2)

cancersCount = c()
for(y in 2010:2019)
{

res1 <- EUtilsSummary("RNA-seq", 
                      type = "esearch", 
                      db = "pubmed",
                      datetype = "pdat",
                      retmax = 5000,
                      mindate = y, 
                      maxdate = y)

fetch <- EUtilsGet(res1, type = "efetch", db = "pubmed")


abstracts <- data.frame(title = fetch@ArticleTitle,
                        abstract = fetch@AbstractText, 
                        journal = fetch@Title,
                        DOI = fetch@PMID, 
                        year = fetch@YearPubmed)
## ensure abstracts are character fields (not factors)
abstracts <- abstracts %>% mutate(abstract = as.character(abstract))
abstracts %>%
  head()
#cancersCount = c()

cancerAbs <- abstracts[grepl("cancer|tumor",  abstracts$abstract),]
cancerAbs$abstract <- sapply(cancerAbs$abstract, tolower)


cancerTypes = c("carcinoma|breast|prostate|lung|pancreas|colon", 
                "sarcoma", "lymphoma", "leukemia", "germ cell tumor|ovary|ovarien|testicle", 
                "blastoma", "hepatocarcinoma", 
                "leiomyoma", "seminoma")
types = c("carcinoma", 
          "sarcoma", "lymphoma", "leukemia", "germ cell tumor", 
          "blastoma", "hepatocarcinoma", 
          "leiomyoma", "seminoma")

for(i in 1:length(cancerTypes))
{
  g <- cancerAbs[grepl(cancerTypes[i],  abstracts$abstract),]
  
  tmp = c(y,types[i],dim(g)[1])
  cancersCount = rbind(cancersCount, tmp)
  
}
print(cancersCount)
}

colnames(cancersCount) <- c("year", "cancerType", "value")
#rownames(cancersCount)<- 1:length(cancerTypes)
cancersCount[,3] <- as.numeric(cancersCount[,3])
df <- as.data.frame(cancersCount)
#colnames(df) <- c("cancer type", "value")





pdf("stackedBarChart_cancerTypes.pdf")

theme_set(theme_classic())

# From on a categorical column variable
g <- ggplot(df, aes(x = year, y = value))
g + geom_bar(aes(fill=cancerType), width = 0.5, stat="identity") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title="Cancer Types", 
       subtitle="RNA-seq cancer related abstracts")

dev.off()


