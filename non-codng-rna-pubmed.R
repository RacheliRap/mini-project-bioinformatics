library(ggplot2)
library(dplyr)
library(rentrez)

# -------------search for rnaseq abstract related to non-coding rna molecules--------------------

types = c("rRNA OR ribosomal RNA OR 12S OR 16s OR 25S OR 25S OR 5S OR 5.8S",
          "tRNA OR mt-tRNA", "lincRNA OR lncRNA", 
          "miRNA OR miR OR microRNA", "piRNA","scRNA","snRNA", "sRNA", "snoRNA","misc_RNA",
          "circRNA OR Circular RNA", "Circulating")
types_for_plot = c("rRNA","tRNA", "lincRNA", 
          "microRNA", "piRNA",  "scRNA","snRNA", "sRNA", "snoRNA","misc_RNA",
          "circRNA", "circulating RNA")


typesCount = c()
types = gsub("OR", "[Title/Abstract] OR", types)
rnaseq_str = "(RNA seq[Title/Abstract] OR RNA-seq[Title/Abstract] OR rna sequencing[Title/Abstract]) "

# for each year, iterate through non-coding types and count the number of abstracts
for(y in 2010:2018)
{
  for(i in 1:length(types))
  {
    query = paste(rnaseq_str," AND (", types[i], "[Title/Abstract]) AND ", y, "[PDAT]", sep = "")
    count = entrez_search(db="pubmed", term=query, retmax=0)$count
    tmp = c(y,types_for_plot[i],count)
    typesCount = rbind(typesCount, tmp)
    
  }
  print(typesCount)
}

# create data frame
colnames(typesCount) <- c("Year", "Molecule", "Value")
df <- as.data.frame(typesCount)
df[,3] <- as.numeric(as.character(df[,3]))

m_colors = c("#df4a7a", "#c97b7a","#de5137", "#d08935","#a78d57","#d2d23e","#cfd88d",
             "#67993f","#76d854", "#529477","#6bb2d5","#6387d7", "#777ba7", "#8371df",
             "#b159e0", "#d6aad9", "#bd6cac", "#db49ba")

# plot the results 
pdf("nonCodingRNA.pdf", width = 13, height = 10)

theme_set(theme_classic())

g <- ggplot(df, aes(x = Year, y = Value))
g + geom_bar(aes(fill=fct_reorder(Molecule, Value, sum, desc=TRUE)), width = 0.5, stat="identity") + 
  theme(axis.title = element_text(family = "Times", face = "italic", size = 25),
        plot.title = element_text(family = "Times", face = "bold", size = 30),
        axis.text.x = element_text(angle=0, vjust=0.6), 
        axis.text =   element_text(family = "Times",size = 20),
        legend.text = element_text(family = "Times",size = 23),
        legend.title = element_text(family = "Times",size = 25),
        legend.key.size = unit(0.95, "cm")) +
  scale_fill_manual(values=m_colors) +
  labs(title="Non-Coding RNA Molecules", 
       subtitle="", fill = "RNA molecules", y = "Number Of Abstracts", x = "Year",
       family = "Times", face = "italic") 



dev.off()



