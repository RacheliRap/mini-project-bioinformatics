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


#------------------------------create pie chart for all diseases by areas---------------------------
res1 <- EUtilsSummary("RNA-seq", 
                      type = "esearch", 
                      db = "pubmed",
                      datetype = "pdat",
                      retmax = 5000,
                      mindate = 2010, 
                      maxdate = 2019)

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
diseasesCount = c()


diseasesAbs <- abstracts[grepl(v,  abstracts$abstract),]
diseasesAbs$abstract <- sapply(diseasesAbs$abstract, tolower)

diseasesTypes = c("", "cancer|prostate cancer|breast cancer|lymphoma|leukemic|leukemia|osteosarcoma|breast cancer|prostate cancers|ovarian cancer|colorectal carcinoma|lung cancer|
                  B-cell lymphoma|Hodgkin lymphoma|melanoma|sarcoma|lymphoma entities|carcinogenesis|Follicular lymphoma|carcinoma|hepatocarcinogenesis|glioblastoma|glioblastoma tumors|
  esophageal cancer|sarcomas|liver cancer|carcinomas|clear cell renal cell carcinomas|squamous cell carcinomas|gastric cancer|Chronic myeloid leukemia|colon cancer|
  lymphomas|gliomas|Buccal mucosal cancer|neuroblastoma tumour|cutaneous melanoma metastases|myeloid leukemia|glioma|Horn cancer|inflammatory breast cancer|colorectal adenocarcinoma|
  acute myeloid leukemia|glioblastomas|squamous cell carcinoma|gastric carcinoma|lung adenocarcinoma|hepatocellular carcinoma|non-small-cell lung cancer (NSCLC) tumors|colon adenocarcinoma|
  adenocarcinoma|carcinogenic|pancreatic adenocarcinoma cancer|Hepatocellular carcinoma (HCC) tumors|clear-cell renal cell carcinoma|colon carcinoma|non-small-cell lung cancer|
  melanoma tumors|tuberculosis|renal cell carcinoma|Burkitt lymphoma|pancreatic ductal adenocarcinoma|clear cell renal cell carcinoma|melanoma eDMRs|non-small cell lung cancer|
  Multiple Cancers|lung squamous cell carcinoma|malignant osteosarcoma|osteosarcomas|Esophageal Squamous Cell Carcinoma|cancers|colorectal cancer|pancreatic carcinogenesis|
  Glioblastoma Atlas|gloiblastoma|neuroblastoma|head and neck cancers|neck squamous cell carcinoma|melanomas|oral squamous cell carcinomas|BRCA1 deficient|tumor|tumors|primary tumors|
  breast tumor|ovarian tumors|tumour|aggressive tumours|Metastasis-competent circulating tumour|
  SCC|NSCLC|CRC|HCC|sarcomatoid RCC|RCC|GBM",
  
  "NMD|pleiotropic developmental defects|polyploidy|chromosomal aneuploidies|Paleopolyploidy|allopolyploidy|hepatic RDD|rosai dorfman disease|rd|PK|hyperdiploid/hypotriploid|
  autosomal dominant hereditary cataracts|osteogenesis|Huntington's disease|GS|genetic disorders|genetic disease|genetic diseases|genetic interaction|genetic abnormalities|inherited diseases|
  NPC|genetic",
  
  "neuro|brain|neurodegenerative diseases|neurodegenerative brain diseases|neurodegenerative disease|Parkinson's disease|Alzheimer's disease|Alzheimer's|AD|AD temporal lobe|LB|epilepsy|
  neurological disorders|neuropsychiatric diseases|neuropsychiatric disorders|psychiatric disorders|necrotic lesions of cerebral infarction|cerebral infarction|neurodegeneration|
  neuro-developmental disorders|neurological diseases|autism, schizophrenia, bipolar disorder|autism|schizophrenia and bipolar disorder|Autism Spectrum Disorders|schizophrenia|TICs", 
  
  "bacteria|bacterial|Salmonella-infected|TSSs|columnaris disease|bacterial infection|bacteremia|bacterial disease infections|bacteria infection|parasitic nematodes and rhizobial bacteria|Tuberculosis Annotation Jamboree|
  TSS|aureus infection",
  
  "viral|malaria|malaria parasite|HCV|HCV infection|metabolic impact of HCV infection|EBV|Dengue viruses|Dengue Fever Vector|viral infections|viral infection|virus infection",
  
  "liver|liver disease|liver cirrhosis|liver toxicity",
  
  "blood|atherosclerosis|MDS|Myelodysplastic|myelodysplastic syndrome|cotton SE|SE",
  
  "heart|cardiac|cardio|cardiac hypertrophy|cardiac septation defects|myocardial ischemia|cardiovascular diseases|myocardial infarction|cardiac dysfunction|Congenital heart disease|PHD", 
  
  "mitochondrial|mitochondrial dysfunction|mitochondria dysfunction",
  
  "lung|raspiratory|URD|lung disease|lung infection|lung diseases|aeruginosa causes chronic lung infection|respiratory syndrome|pneumococcal|chronic obstructive pulmonary disease|pulmonary diseases|
  asthma",
  
  "gastro|gastric|gastrointestinal disease|gastrointestinal|inflammatory bowel disease|metabolic diseases|metabolic syndrome|hepatic metabolism and liver disease|metabolic disorder|metabolic disease|colonic polyp lesion",
  
  "eye|retiba|retinal disease|diabetic retinopathy and retinal inflammation|retinal leukostasis", 
  
  "Zn deficiency|N deficiency|diabetes|diabetes-induced memory deficits", 
  
  "parasite|parasitic|parasitic diseases", 
  
  "appendicities|periodontitis and appendicitis", 
  
  "depression|depressive disorder", 
  
  "outoimmune|immune|immono|rheumatoid arthritis|amyotrophic lateral sclerosis|multiple sclerosis|psoriasis|autoimmune disease|autoimmune disease systemic lupus erythematosus|systemic lupus erythematosus|LPS",
  
  "kidney|chronic kidney disease", 
  
  "fungal meningitis|meningitis|fungal infection|fungal"
  
)

diseasesTypes <- sapply(diseasesTypes, tolower)

types = c("",
          "Cancer", 
          "Genetic Diseases", 
          "Brain diseases", 
          "Bacterial Diseases", 
          "Viral Diseases", 
          "Liver diseases",
          "Blood Diseases",
          "Cardiac Diseases", 
          "Mitocondrial Diseases",
          "Lungs Diseases", 
          "Gustric Diseases", 
          "Eye Diseases", 
          "Deficiencies", 
          "Parasitic Diseases", 
          "Abdominal Diseases", 
          "Mental Diseases", 
          "Immune Diseases", 
          "Kidney Diseases", 
          "Fungal Diseases")

for(i in 1:length(diseasesTypes))
{
  g <- diseasesAbs[grepl(diseasesTypes[i],  abstracts$abstract),]
  g1 <- g$DOI %>% list
  abstracts <- abstracts %>% 
    mutate(DOI  = as.character(DOI))
  
  tmp = c(diseasesTypes[i],dim(g)[1])
  diseasesCount = rbind(diseasesCount, tmp)
}

colnames(diseasesCount) <- c("diseaseTypes", "value")
rownames(diseasesCount)<- 1:length(diseasesTypes)
df <- as.data.frame(diseasesCount)
df[,2] = as.numeric(as.character(df[,2]))



pdf("DiseasesByAnatomy_Pie.pdf",width = 15)

library(ggplot2)

# Create a basic bar
pie = ggplot(df, aes(x="", y=value, fill=types)) + geom_bar(stat="identity", width=0.1)

# Convert to pie (polar coordinates) and add labels
pie = pie + coord_polar(theta = "y", direction = 1) + geom_text(aes(x=1.05, label = types), size=2, angle = 45, position = position_stack(vjust = 0.5))

# Remove labels and add title
pie = pie + labs(x = NULL, y = NULL, fill = NULL, title = "All Found Diseases, 2010-2019")

# Tidy up the theme
pie = pie + theme_classic() + theme(axis.line = element_blank(),
                                    axis.text = element_blank(),
                                    axis.ticks = element_blank(),
                                    plot.title = element_text(hjust = 0.1, color = "#666666"))

pie
dev.off()
