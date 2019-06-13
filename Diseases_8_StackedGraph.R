library(ggplot2)
library(dplyr)
library(rentrez)
#----------------create stacked bar graph for 9 diseases categories (after uniting 18 into 9)---------------------------

types = c("cancer|prostate cancer|breast cancer|lymphoma|leukemic|leukemia|osteosarcoma|breast cancer|prostate cancers|ovarian cancer|colorectal carcinoma|lung cancer|
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
          TSS|aureus infection|viral|malaria|malaria parasite|HCV|HCV infection|metabolic impact of HCV infection|EBV|Dengue viruses|Dengue Fever Vector|viral infections|viral infection|virus infection|
          parasite|parasitic|parasitic diseases|fungal meningitis|meningitis|fungal infection|fungal",
          
          "liver|liver disease|liver cirrhosis|liver toxicity|kidney|chronic kidney disease|gastro|gastric|gastrointestinal disease|gastrointestinal|inflammatory bowel disease|metabolic diseases|metabolic syndrome|
          hepatic metabolism and liver disease|metabolic disorder|metabolic disease|colonic polyp lesion",
          
          "blood|atherosclerosis|MDS|Myelodysplastic|myelodysplastic syndrome|cotton SE|SE|diabetes|diabetes-induced memory deficits",
          
          "heart|cardiac|cardio|cardiac hypertrophy|cardiac septation defects|myocardial ischemia|cardiovascular diseases|myocardial infarction|cardiac dysfunction|Congenital heart disease|PHD|
          lung|raspiratory|URD|lung disease|lung infection|lung diseases|aeruginosa causes chronic lung infection|respiratory syndrome|pneumococcal|chronic obstructive pulmonary disease|pulmonary diseases|
          asthma", 
          
          "mitochondrial|mitochondrial dysfunction|mitochondria dysfunction",
          
          "outoimmune|immune|immono|rheumatoid arthritis|amyotrophic lateral sclerosis|multiple sclerosis|psoriasis|autoimmune disease|autoimmune disease systemic lupus erythematosus|systemic lupus erythematosus|LPS"
          
)

#Initialize count:
diseases_count = c()

#Find all RNA related articles for each category per year:
for(y in 2010:2018)
{
  for(i in 1:length(types))
  {
    query = paste("(RNA-Seq[Title/Abstract]|RNAseq[Title/Abstract]|RNA seq[Title/Abstract]|RNA sequencing[Title/Abstract]|RNAsequencing[Title/Abstract]|RNA-sequencing[Title/Abstract]) AND (", types[i], ") AND ", y, "[PDAT]", sep = "")
    count = entrez_search(db="pubmed", term=query, retmax=0)$count
    tmp = c(y,types[i],count)
    diseases_count = rbind(diseases_count, tmp)
  }
  print(diseases_count)
}

#Create tags for the categories:
types = c(
  "Cancer", 
  "Genetic Diseases", 
  "Brain diseases", 
  "Bacterial,Viral,Parasitic and Fungal Diseases", 
  "Abdominal diseases",
  "Hematologic Diseases",
  "Cardio-Raspiratory Diseases", 
  "Mitocondrial Diseases",
  "Autoimmune Diseases")

#Create dataframe for the graph:
colnames(diseases_count) <- c("Year", "Diseases", "Value")
diseases_count[,2] <- types
df <- as.data.frame(diseases_count)
df[,3] <- as.numeric(as.character(df[,3]))
m_colors = c("#df4a7a",
             "#c97b7a",
             "#d2d23e",
             "#67993f",
             "#81dacf",
             "#6387d7",
             "#8371df",
             "#b159e0",
             "#d6aad9",
             "#bd6cac",
             "#db49ba")


pdf("Diseases Stacked Graph (Merged).pdf", width = 13, height = 10)

theme_set(theme_classic())

g <- ggplot(df, aes(x = Year, y = Value, fill = Diseases))
g + geom_bar(aes(fill=forcats::fct_reorder(Diseases, Value)), width = 0.5, stat="identity") + 
  theme(axis.text = element_text(family = "Times", size = 15),
        axis.text.x = element_text(family = "Times", vjust=0.6),
        axis.title = element_text(family = "Times", face = "italic", size = 20),
        plot.title = element_text(family = "Times", face = "bold", size = 25),
        legend.text = element_text(family = "Times", size = 20))+
  scale_fill_manual(values=m_colors) +
  labs(title="Diseases reseach by RNA-seq, through the years 2010-2018", 
       subtitle="",
       y = "Number Of Articles")

dev.off()