library(tidyverse)
library(tidytext) #to use text mining
library(quRan)


data("quran_ar_min")

#select column to study
qr <- quran_ar_min %>% select(surah_id,ayah_id,surah_title_en,ayah =text)

#unnest every word in a row
qr_k <-  quran_ar_min %>% 
  select(surah_id,ayah_id,surah_title_en,ayah =text) %>%
  unnest_tokens(output=kalima,input=ayah,token="words")
write_csv(qr_k,"quran_by_words.csv")

#get occurence of words
qr_occ <-  quran_ar_min %>% 
  select(surah_id,ayah_id,surah_title_en,ayah =text) %>%
  unnest_tokens(output=kalima,input=ayah,token="words") %>% 
  group_by(kalima) %>% 
  summarise(marrat=n()) %>% 
  arrange (desc(marrat))
save(qr_occ,file="quran_occurences.rda")

pl_occ <- qr_occ %>% slice(1:20)%>%
  ggplot(aes(fct_rev(fct_inorder(kalima)),marrat))+ 
  #forcats fct to order factors
  geom_col(fill="red") +
  coord_flip()+
  labs(y="عدد المرات", x="الكلمة في القرآن",title="تكرر الكلمات في القرآن دون حذف أي كلمة")
pl_occ
ggsave("redondance_words.png",plot=pl_occ)


#remove words in "stop words"
#____1/ with stopwords::
library(stopwords)
stp <- stopwords("arabic")
qr_occ_cln1 <- qr_occ %>% 
  filter (!kalima %in% stp) 
#stopwords is not effective,should have an other:
#->  it’s not as long of a list as >>>>> arabicStemR::removeStopWords(), 
#and it includes important Qur’anic words like يوم (day).

#___2/ with arabicStemR
library(arabicStemR)  # Has a list of Arabic stopwords
arabic_stopwords <- tibble(word = removeStopWords("سلام")$arabicStopwordList)
qr_occ_cln2 <- qr_occ %>% 
  filter (!kalima %in% arabic_stopwords$word)
#plot 20 first occurences
pl_occ_cln2 <- qr_occ_cln2 %>% slice(1:20)%>%
  ggplot(aes(fct_rev(fct_inorder(kalima)),marrat))+ 
  #forcats fct to order factors
  geom_col(fill="blue") +
  coord_flip()+
  labs(y="عدد المرات", x="الكلمة في القرآن",title="تكرار الكلمات في القرآن بعد حذف كلمات الوقوف")
pl_occ_cln2
ggsave("redondance_without_stpwords.png",plot=pl_occ_cln2)

#see all words starting with "waw"
waw <- qr_occ_cln2[str_detect(qr_occ_cln2$kalima,"^و.*"),]

#see identical occurence
ident <- qr_occ %>% group_by(marrat) %>% filter(n()>1) %>% ungroup()



