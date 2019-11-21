library(tidyverse)
library(tidytext) #to use text mining
library(quRan)

data("quran_ar_min")

#select column to study
qr <- quran_ar_min %>% select(surah_id,ayah_id,surah_title_en,ayah =text)

#unnest every word in a row
qr_k <-  quran_ar_min %>% 
  select(surah_id,ayah_id,surah_title_en,ayah =text) %>%
  unnest_tokens(kalima,ayah)

#get occurence of words
qr_occ <-  quran_ar_min %>% 
  select(surah_id,ayah_id,surah_title_en,ayah =text) %>%
  unnest_tokens(kalima,ayah) %>% 
  group_by(kalima) %>% 
  summarise(marrat=n()) %>% 
  arrange (desc(marrat))
save(qr_occ,file="quran_occurences.rda")

#remove words in "stop words"
library(stopwords)
stp <- stopwords("arabic")
qr_occ_rmstp <- qr_occ %>% 
  filter (!kalima %in% stp) #stopwords is not effective,should have an other

#see all words starting with "waw"
waw <- qr_occ[str_detect(qr_occ$kalima,"^و.*"),]

#see identical occurence
ident <- qr_occ %>% group_by(marrat) %>% filter(n()>1) %>% ungroup()