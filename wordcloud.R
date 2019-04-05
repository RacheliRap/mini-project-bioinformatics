library(RISmed)
library(magrittr)
library(dplyr)
library(wordcloud)
library(tidytext)
library(knitr)
library(tm)
library(topicmodels)
library(MASS)
library(flipMultivariates)

res1 <- EUtilsSummary("RNA-seq", 
                      type = "esearch", 
                      db = "pubmed",
                      datetype = "pdat",
                      retmax = 5000,
                      mindate = 2018, 
                      maxdate = 2018)

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

cloud <- abstracts %>%
  unnest_tokens(word, abstract) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) 

#word cloud for 2018
pdf("wordcloud_2018(5).pdf")
cloud %>%
  with(wordcloud(word, n, min.freq = 70, max.words = 100, colors = brewer.pal(8, "Dark2")), scale = c(8,.3), per.rot = 0.4)
dev.off()

#word cloud - journals
pdf("wordcloud_journals_2018.pdf", width = 13, height = 10)
cloud3 <- abstracts %>%
  select(journal) %>%
  group_by(journal) %>%
  count(sort = TRUE)
cloud3 %>%
  with(wordcloud(journal, n, min.freq = 10, random.order = FALSE, max.words = 80, colors = brewer.pal(9, "Set1")), rot.per = .6)
dev.off()

#bigram clouds - dosent work yet
#-------------------------------------------------------------------------------
bigrams_united <- abstracts %>%
  unnest_tokens(word, abstract, token = "ngrams", n = 2) %>%
  
  anti_join(stop_words) %>%
  count(word, sort = TRUE) 

bigrams_united %>%
  with(wordcloud(word, n, max.words = 1000, random.order = FALSE, colors = brewer.pal(9, "Set1"), scale = c(8, 0.3)), per.rot = 0.4)
#-----------------------------------------------------------------------------

g <- abstracts[grepl("rna", abstracts$abstract),]
g1 <- g$DOI %>% list
abstracts <- abstracts %>% 
  mutate(DOI  = as.character(DOI)) 

abstracts[abstracts$DOI %in% g1[[1]],] %>%
  select(title, journal, DOI) %>%
  knitr::kable()

abstracts %>%
  unnest_tokens(word, abstract) %>%
  anti_join(stop_words) %>%
  count(DOI, word, sort = TRUE) %>%
  cast_dtm(DOI, word, n) ->
  abstracts1

abs_lda <- LDA(abstracts1, k = 10, control = list(seed = 1234))
abs_lda_td <- tidytext:::tidy.LDA(abs_lda)

