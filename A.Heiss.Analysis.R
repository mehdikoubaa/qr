#_____Find analysis of Andrew Heiss who made quRan package
# URL= https://www.andrewheiss.com/blog/2018/12/28/tidytext-pos-arabic/
#
library(cleanNLP)
library(quRan)
library(tidytext)
library(arabicStemR)  # Has a list of Arabic stopwords

cnlp_download_corenlp() #needs to install first rjava or python
cnlp_init_corenlp(language="ar")

fatiha <- quran_ar_min %>% 
  filter(surah_id == 1)

# THIS MAYS TAKE SO LONG (36 min)
fatiha_annotated <- cnlp_annotate(fatiha, 
                                  as_strings = TRUE,
                                  text_var = "text", 
                                  doc_var = "ayah_title")

# SAVE THIS SO YOU NEVER HAVE TO RUN IT AGAIN
saveRDS(quran_annotated, "quran_annotated.rds")

fatiha_terms <- fatiha_annotated %>% 
  cnlp_get_token()

#The stopwords package also has a bunch of common Arabic words,
#accessible through stopwords::stopwords(language = "ar", source = "misc")), 
#but it’s not as long of a list as arabicStemR::removeStopWords(), 
#and it includes important Qur’anic words like يوم (day).
# So we’ll just use the arabicStemR words here.

# In order to get the full list of Arabic stopwords, we have to feed
# removeStopWorsd some sort of Arabic text, and then access the
# `arabicStopwordList` from the resulting object. It's a roundabout approach,
# but it works
arabic_stopwords <- tibble(word = removeStopWords("سلام")$arabicStopwordList)

top_nouns <- quran_terms %>% 
  filter(str_detect(pos, "NN")) %>% 
  count(word, sort = TRUE) %>% 
  # Get rid of tiny diacritic-like words
  filter(nchar(word) > 1) %>%
  # Get rid of stopwords
  anti_join(arabic_stopwords, by = "word") %>% 
  top_n(10, n) %>% 
  mutate(word = fct_inorder(word))

plot_top_nouns <- ggplot(top_nouns, aes(x = fct_rev(word), y = n, fill = n > 500)) +
  geom_col() + 
  scale_fill_manual(values = c("#8F562B", "#276B42"), guide = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  labs(y = "Frequency", x = NULL) +
  coord_flip() +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank())
plot_top_nouns