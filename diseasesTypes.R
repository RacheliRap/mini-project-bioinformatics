library(RISmed)
library(magrittr)
library(dplyr)
library(wordcloud)
library(tidytext)
library(knitr)
library(tm)
library(topicmodels)
library(MASS)
library(ggplot2)

#-----the script count for each year how many abstracts publicited for each disease,---------
#------------------and plot stacked bar plot and pie chart for each year-------------------------------

pdf("DisesesTypes_pieChart.pdf")

for(y in 2010:2018)
{
  #search for RNA-seq abstracts
res1 <- EUtilsSummary("RNA-seq", 
                      type = "esearch", 
                      db = "pubmed",
                      datetype = "pdat",
                      retmax = 5000,
                      mindate = y, 
                      maxdate = y)
#fetch the abstracts
fetch <- EUtilsGet(res1, type = "efetch", db = "pubmed")

#create data frame for the abstracts
abstracts <- data.frame(title = fetch@ArticleTitle,
                        abstract = fetch@AbstractText, 
                        journal = fetch@Title,
                        DOI = fetch@PMID, 
                        year = fetch@YearPubmed)

# ensure abstracts are character fields (not factors)
abstracts <- abstracts %>% mutate(abstract = as.character(abstract))
abstracts %>%
  head()

diseasesCount = c()

#make sure all abstracts are in lower case
abstracts$abstract <- sapply(abstracts$abstract, tolower)

#diseases types for search
diseasesTypes = c("", "cancer|prostate cancer|breast cancer|lymphoma|leukemic|leukemia|osteosarcoma|breast cancer|prostate cancers|ovarian cancer|colorectal carcinoma|lung cancer|
                  B-cell lymphoma|Hodgkin lymphoma|melanoma|sarcoma|lymphoma entities|carcinogenesis|Follicular lymphoma|carcinoma|hepatocarcinogenesis|glioblastoma|glioblastoma tumors|
                  esophageal cancer|sarcomas|liver cancer|carcinomas|clear cell renal cell carcinomas|squamous cell carcinomas|gastric cancer|Chronic myeloid leukemia|colon cancer|
                  lymphomas|gliomas|Buccal mucosal cancer|neuroblastoma tumour|cutaneous melanoma metastases|myeloid leukemia|glioma|Horn cancer|inflammatory breast cancer|colorectal adenocarcinoma|
                  acute myeloid leukemia|glioblastomas|squamous cell carcinoma|gastric carcinoma|lung adenocarcinoma|hepatocellular carcinoma|non-small-cell lung cancer (NSCLC) tumors|colon adenocarcinoma|
                  adenocarcinoma|carcinogenic|pancreatic adenocarcinoma cancer|Hepatocellular carcinoma (HCC) tumors|clear-cell renal cell carcinoma|colon carcinoma|non-small-cell lung cancer|
                  melanoma tumors|tuberculosis|renal cell carcinoma|Burkitt lymphoma|pancreatic ductal adenocarcinoma|clear cell renal cell carcinoma|melanoma eDMRs|non-small cell lung cancer|
                  Multiple Cancers|lung squamous cell carcinoma|malignant osteosarcoma|osteosarcomas|Esophageal Squamous Cell Carcinoma|cancers|colorectal cancer|pancreatic carcinogenesis|
                  Glioblastoma Atlas|gloiblastoma|neuroblastoma|head and neck cancers|neck squamous cell carcinoma|melanomas|oral squamous cell carcinomas|BRCA1 deficient|tumor|tumors|primary tumors|breast tumor|ovarian tumors|tumour|aggressive tumours|Metastasis-competent circulating tumour",
                  "NMD",
                  "neurodegenerative diseases|neurodegenerative brain diseases|neurodegenerative disease", 
                  "neurological disorders|neuropsychiatric diseases|neuropsychiatric disorders|psychiatric disorders",
                   "Salmonella-infected",
                   "malaria|malaria parasite",
                   "HCV|HCV infection|metabolic impact of HCV infection", 
                   "liver disease|liver cirrhosis|liver toxicity",
                   "SREs", 
                   "Huntington's disease", 
                   "Parkinson's disease",
                  "Alzheimer's disease|Alzheimer's", 
                   "mucosal infections", 
                  "atherosclerosis",
                   "pleiotropic developmental defects",
                   "cardiac hypertrophy|cardiac septation defects|myocardial ischemia|cardiovascular diseases|myocardial infarction|cardiac dysfunction|Congenital heart disease", 
                   "polyploidy",
                   "chromosomal aneuploidies", 
                   "GS", 
                   "LPS",
                   "URD",
                   "gastrointestinal nematode infections|gastrointestinal disease|gastrointestinal",
                  "MDS",
                   "genetic disorders|genetic disease|genetic diseases|genetic interaction|genetic abnormalities|inherited diseases", 
                   "AD|AD temporal lobe",
                   "pneumococcal",
                   "Myelodysplastic|myelodysplastic syndrome", 
                   "retinal disease|diabetic retinopathy and retinal inflammation|retinal leukostasis", 
                  "EBV",
                   "Dengue viruses|Dengue Fever Vector", 
                   "Zn deficiency", 
                  "N deficiency", 
                   "inorganic nutrient deficiency",
                   "parasitic diseases", 
                   "diabetes|diabetes-induced memory deficits", 
                   "lung disease|lung infection|lung diseases|aeruginosa causes chronic lung infection|respiratory syndrome", 
                   "nematode infection", 
                   "epilepsy", 
                  "chronic obstructive pulmonary disease",
                   "periodontitis and appendicitis", 
                   "TSSs", 
                  "cotton SE|SE", 
                   "LB",
                   "depression|depressive disorder", 
                  "viral infections|viral infection|virus infection",
                   "columnaris disease", 
                   "Paleopolyploidy",
                   "mitochondrial dysfunction|mitochondria dysfunction", 
                  "allopolyploidy", 
                   "P(i) deficiency-induced cluster root", 
                   "bacterial infection|bacteremia|bacterial disease infections|bacteria infection|parasitic nematodes and rhizobial bacteria",
                   "rheumatoid arthritis",
                   "inflammatory bowel disease", 
                   "hepatic RDD", 
                   "SCC", 
                   "DHAD", 
                  "asthma", 
                   "Tuberculosis Annotation Jamboree", 
                   "developmental asynchrony", 
                   "PK", 
                   "neuro-developmental disorders", 
                   "neurological diseases", 
                   "TSS",
                   "amyotrophic lateral sclerosis|multiple sclerosis", 
                   "metabolic diseases|metabolic syndrome|hepatic metabolism and liver disease|metabolic disorder|metabolic disease",
                  "chronic kidney disease", 
                   "NSCLC", 
                   "psoriasis", 
                  "aeruginosa infections",
                   "fungal meningitis",
                   "autism, schizophrenia, bipolar disorder|autism|schizophrenia and bipolar disorder|Autism Spectrum Disorders|schizophrenia",
                   "CRC",
                   "HCC",
                   "NPC", 
                   "TICs",
                   "PHD",
                   "sarcomatoid RCC", 
                  "Intrinsic valvular degeneration and dysfunction", 
                   "autoimmune disease",
                   "RCC",
                   "hyperdiploid/hypotriploid", 
                  "autoimmune disease systemic lupus erythematosus|systemic lupus erythematosus", 
                  "necrotic lesions of cerebral infarction", 
                   "pulmonary diseases",
                   "fungal infection",  
                   "colonic polyp lesion",
                   "neurodegeneration", 
                  "autosomal dominant hereditary cataracts",
                   "osteogenesis", 
                  "aureus infection",
                   "GBM")

#diseases types for show on the plot
types = c("cancer", 
          "NMD", 
          "neurodegenerative diseases", 
          "neurological disorders", 
          "salmonella", 
          "malaria", 
          "HCV", 
          "liver diseases",
          "SRE", 
          "Huntington's disease", 
          "Parkinson's disease",
          "Alzheimer's disease",
          "mucosal infections", 
          "atherosclerosis",
          "pleiotropic developmental defects",
          "cardiac diseases", 
          "polyploidy",
          "chromosomal aneuploidies", 
          "GS", 
          "LPS",
          "URD",
          "gastrointestinal infections", 
          "MDS",
          "genetic diseases",
          "AD", 
          "pneumococcal",
          "myelodysplastic syndrome", 
          "retinal disease", 
          "EBV", 
          "Dengue", 
          "Zn deficiency", 
          "N deficiency", 
          "inorganic nutrient deficiency",
          "parasitic diseases", 
          "diabetes", 
          "lung diseases", 
          "nematode infection", 
          "epilepsy", 
          "pulmonary disease",
          "periodontitis and appendicitis", 
          "TSSs", 
          "SE", 
          "LB",
          "depression", 
          "viral infections",
          "columnaris disease", 
          "Paleopolyploidy",
          "mitochondrial dysfunction", 
          "allopolyploidy", 
          "P(i) deficiency-induced cluster root", 
          "bacterial infection", 
          "rheumatoid arthritis",
          "inflammatory bowel disease", 
          "hepatic RDD", 
          "SCC", 
          "DHAD", 
          "asthma", 
          "tuberculosis", 
          "developmental asynchrony", 
          "PK", 
          "neuro-developmental disorders", 
          "neurological diseases", 
          "TSS",
          "sclerosis", 
          "metabolic diseases", 
          "kidney disease", 
          "NSCLC", 
          "psoriasis", 
          "aeruginosa infections",
          "fungal meningitis",
          "autism, schizophrenia, bipolar disorders", 
          "CRC",
          "HCC",
          "NPC", 
          "TICs",
          "PHD",
          "sarcomatoid RCC", 
          "Intrinsic valvular dysfunction", 
          "autoimmune disease",
          "RCC",
          "hyperdiploid/hypotriploid", 
          "lupus", 
          "cerebral infarction", 
          "pulmonary diseases",
          "fungal infection",  
          "colonic polyp lesion",
          "neurodegeneration", 
          "autosomal cataracts",
          "osteogenesis", 
          "aureus infection",
          "GBM")

diseasesCountYear = c()
#run over all the diseases
for(i in 1:length(diseasesTypes))
{
  #search for all the abstracts mention the disease
  g <- abstracts[grepl(diseasesTypes[i],  abstracts$abstract),]
  tmp = c()
  tmp = c(y,types[i],dim(g)[1])
  diseasesCount = rbind(diseasesCount, tmp)
  
  #for every year pie chart
  tmp1 = c()
  if(dim(g)[1] > 0)
  {
  tmp1 = c(types[i] ,dim(g)[1])
  diseasesCountYear = rbind(diseasesCountYear, tmp)
  }
}
dfPerYear = as.data.frame(diseasesCountYear)
# Create a basic bar
pie = ggplot(dfPerYear, aes(x="", y=value, fill=types)) + geom_bar(stat="identity", width=1)

# Convert to pie (polar coordinates) and add labels
pie = pie + coord_polar("y", start=0) + geom_text(aes(label = ""), position = position_stack(vjust = 0.5))

# Add color scale (hex colors)
#pie = pie + scale_fill_manual(values=c("#55DDE0", "#33658A", "#2F4858", "#F6AE2D", "#F26419", "#999999")) 

# Remove labels and add title
pie = pie + labs(x = NULL, y = NULL, fill = NULL, title = paste("Cancer Types - ",y))

# Tidy up the theme
pie = pie + theme_classic() + theme(axis.line = element_blank(),
                                    axis.text = element_blank(),
                                    axis.ticks = element_blank(),
                                    plot.title = element_text(hjust = 0.5, color = "#666666"))
pie

}

dev.off()

colnames(diseasesCount) <- c("year","diseaseTypes", "value")
rownames(diseasesCount)<- 1:length(diseasesTypes)
#convert duseases count to data frame
df <- as.data.frame(diseasesCount)



#create stacked bar plot
pdf("diseasesTypes_pieChart_2018.pdf")


theme_set(theme_classic())

g <- ggplot(df, aes(x = year, y = value))
g + geom_bar(aes(fill=diseasesTypes), width = 0.5, stat="identity") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6),
        axis.text.y = element_blank()) +
  labs(title="Diseases", 
       subtitle="RNA-seq diseases related abstracts")

dev.off()







