#Forschungspraktikum 1&2 Ritter 
#Teil 5: Key ATM mit grünen Schlüsselwörtern

packages <- c("dplyr", "magrittr", "quanteda", "keyATM")
install.packages(setdiff(packages, rownames(installed.packages()))) 

lapply(packages, library, character.only = TRUE)

Datensatz_Zeitungsartikel_btw_2025 <- read.csv("C:/Users/jarit/Desktop/Datensatz_Zeitungsartikel_btw_2025.csv")
Datensatz_Zeitungsartikel_btw_2025$artikelquelle <- tolower(Datensatz_Zeitungsartikel_btw_2025$artikelquelle)
Datensatz_Zeitungsartikel_btw_2025$filename <- tolower(Datensatz_Zeitungsartikel_btw_2025$filename)

#sample
set.seed(500)
btw_Datensatz_sample <- Datensatz_Zeitungsartikel_btw_2025[sample(1:nrow(Datensatz_Zeitungsartikel_btw_2025), size=nrow(Datensatz_Zeitungsartikel_btw_2025)*.10), ]

# text bereihnigen
btw_Datensatz_sample$text <- btw_Datensatz_sample$text %>%
  str_squish() %>%  
  str_replace_all("\n", " ")  
head(btw_Datensatz_sample$text)

#aus Tabelle mit 25 häufigsten Wörtern zusammengetragen 5 davon ausgeschlossen (viel, neue, zwei, drei, wahl)
btw_Datensatz_sample <- btw_Datensatz_sample %<>%
  mutate(
    genre = case_when(
      str_detect(text, "friedrich|merz|habeck|scholz|trump") ~ "politiker",
      str_detect(text, "prozent|bundestag|deutschen|wahlkampf|februar") ~ "btw2025",
      str_detect(text, "afd|union|link|fdp|parteien") ~ "parteien",   
      str_detect(text, "menschen|ukrain|berlin|politik|euro") ~ "inhalte",
      TRUE ~ NA_character_
    )
  )
table(btw_Datensatz_sample$genre)
corpus_tm <- corpus(as.character(btw_Datensatz_sample$text),
                    docvars = data.frame(genre = btw_Datensatz_sample$genre
                    ))

# Tokenisieren und bereinigen
tokens_tm <- tokens(corpus_tm, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE)

# Stopwords 
de_stopwords <- stopwords("de")
tokens_tm <- tokens_remove(tokens_tm, de_stopwords, case_insensitive = TRUE)
my_stopwords <- c(
  de_stopwords, 
  "der", "die", "das", "ein", "eine", "einer", "als", "in", "ber", "donnerstag", "freitag", "samstag",
  "sagt", "sagte", "jahr", "gibt", "wurde", "sollen", "machen", "montag", "dienstag", "mittwoch",
  "gerne", "uhr", "schon", "kennen", "immer", "müssen", "seit", "wrde", "sonntag",
  "wer", "werden", "mal", "konnte", "kommt", "müsse", "seien", "s",
  "knnen", "dafr", "grne", "mssen", "geht", "ab", "bereit", "beim", "zudem","weiterlesen"
)

tokens_tm <- tokens_remove(tokens_tm, my_stopwords, case_insensitive = TRUE)
dfm_Datensatz_Zeitungsartikel_tm <- dfm(tokens_tm)
dfm_Datensatz_Zeitungsartikel_tm <- dfm_Datensatz_Zeitungsartikel_tm[!rowSums(dfm_Datensatz_Zeitungsartikel_tm) == 0, ] # remove empty rows

#Key ATM
keyATM_docs <- keyATM_read(texts = dfm_Datensatz_Zeitungsartikel_tm)

keywords <- list(
  gemeinschaft  = c("kraft", "viele", "sorgen", "gesellschaft"),
  innenpolitik   = c("wirtschaft", "kommunen", "arbeit", "unternehmen"),
  außenpolitik    = c("eu", "europäische", "europa", "unterstützen"),
  werte            = c("demokratie", "schutz", "leben", "sicherheit")
)

k <-  20

key_topics <- keyATM(
  docs              = keyATM_docs,  
  no_keyword_topics = k-2,          
  keywords          = keywords,     
  model             = "base",      
  options           = list(seed = 1000)
)

top_words(key_topics)

plot_topicprop(key_topics, show_topic = 1:4)

#Visualisierung der Schlüsselwörter
key_viz <- visualize_keywords(docs = keyATM_docs, keywords = keywords)
key_viz

# genre als Spalte in den gesampelten Datensatz hinzufügen
btw_Datensatz_sample %>%
  mutate(
    genre = case_when(
      str_detect(text, "friedrich|merz|habeck|scholz|trump") ~ 1,         # Politiker
      str_detect(text, "prozent|bundestag|deutschen|wahlkampf|februar") ~ 2, # btw2025
      str_detect(text, "afd|union|link|fdp|parteien") ~ 3,                  # Parteien
      str_detect(text, "menschen|ukrain|berlin|politik|euro") ~ 4,          # Inhalte
      TRUE ~ NA_real_                                                        # Falls keine Kategorie zutrifft
    )
  ) %>%
  select(text, genre) %>%
  head()

# Häufigkeitstabelle der Genres anzeigen
view(btw_Datensatz_sample)
table(btw_Datensatz_sample$genre)

#Kategorien im Verhältnis
politiker_docs <- btw_Datensatz_sample %>%
  filter(genre == "politiker")
head(politiker_docs)
head(btw_Datensatz_sample %>% filter(genre == "politiker"))

btw2025_docs <- btw_Datensatz_sample %>%
  filter(genre == "btw2025")
head(btw2025_docs)
head(btw_Datensatz_sample %>% filter(genre == "btw2025"))

inhalte_docs <- btw_Datensatz_sample %>%
  filter(genre == "inhalte")
head(inhalte_docs)
head(btw_Datensatz_sample %>% filter(genre == "inhalte"))

parteien_docs <- btw_Datensatz_sample %>%
  filter(genre == "parteien")
head(parteien_docs)
head(btw_Datensatz_sample %>% filter(genre == "parteien"))

#Key Topics
#4-inhalte
btw_Datensatz_sample$filename[top_docs(key_topics, 10)[, 4]]

#2-btw
btw_Datensatz_sample$filename[top_docs(key_topics, 10)[, 2]]

#3-parteien
btw_Datensatz_sample$filename[top_docs(key_topics, 10)[, 3]]

#1-politiker
btw_Datensatz_sample$filename[top_docs(key_topics, 10)[, 1]]

#Vorbereitung auf key_topics_covar
table(btw_Datensatz_sample$genre)
sum(is.na(btw_Datensatz_sample$genre)) 
colnames(btw_Datensatz_sample)

str(btw_Datensatz_sample$genrebtw2025)
str(btw_Datensatz_sample$genreparteien)
str(btw_Datensatz_sample$genrepolitiker)
str(btw_Datensatz_sample$genreinhalte)

btw_Datensatz_sample <- btw_Datensatz_sample %>%
  mutate(
    genreparteien = case_when(
      str_detect(text, "afd|union|link|fdp|parteien") ~ 1,
      TRUE ~ 0
    ),
    genrepolitiker = case_when(
      str_detect(text, "friedrich|merz|habeck|scholz|trump") ~ 1,
      TRUE ~ 0
    ),
    genreinhalte = case_when(
      str_detect(text, "menschen|ukrain|berlin|politik|euro") ~ 1,
      TRUE ~ 0
    ),
    genrebtw2025 = case_when(
      str_detect(text, "prozent|bundestag|deutschen|wahlkampf|februar") ~ 1,
      TRUE ~ 0
    )
  )

btw_Datensatz_sample <- btw_Datensatz_sample %>%
  mutate(
    genreparteien = as.factor(genreparteien),
    genrepolitiker = as.factor(genrepolitiker),
    genreinhalte = as.factor(genreinhalte),
    genrebtw2025 = as.factor(genrebtw2025)
  )

str(btw_Datensatz_sample$genreparteien)
str(btw_Datensatz_sample$genrepolitiker)
str(btw_Datensatz_sample$genreinhalte)
str(btw_Datensatz_sample$genrebtw2025)

key_topics_covar <- keyATM(
  docs = keyATM_docs, 
  no_keyword_topics = k,  
  keywords = keywords,  
  model = "covariates", 
  model_settings = list(
    covariates_data = btw_Datensatz_sample,  
    covariates_formula = ~ genrepolitiker + genreinhalte + genreparteien + genrebtw2025  
  ),
  options = list(seed = 1000)
)

covariates_info(key_topics_covar) 

top_words(key_topics_covar)
#Schlüsselwörter  "europäische" und "unterstützen" kommen nicht vor

# Häufigkeit der Kovariaten
table(btw_Datensatz_sample$genre) / nrow(btw_Datensatz_sample) * 100

# Häufigkeit der Genres berechnen
genre_table <- table(btw_Datensatz_sample$genre) 
genre_percentage <- (genre_table / nrow(btw_Datensatz_sample)) * 100
genre_percentage

# ggplot2 laden
library(ggplot2)

btw_Datensatz_sample$key_topics <- sample(1:4, nrow(btw_Datensatz_sample), replace = TRUE)

# Visualisierung: Genres in Bezug auf Themen
ggplot(btw_Datensatz_sample, aes(x = factor(key_topics), fill = genre)) +
  geom_bar(position = "fill") +  
  scale_y_continuous(labels = scales::percent) +  
  labs(x = "key_topics", y = "Prozentualer Anteil der Genres", fill = "Genre") +
  theme_minimal() +
  ggtitle("Verteilung der Genres in den key_topics") 

# gemeinschaft 1, innenpolitik 2, außenpolitik 3, werte 4

#geschätzte Unterschide nach Themen und Kovariaten:

#Parteien
strata_topic <- by_strata_DocTopic(
  key_topics_covar,
  by_var = "genreparteien1",
  labels = c("other", "parteien")
)
#visualizing Differences by topics
plot(strata_topic, var_name = "genre",
     show_topic = c(1:4))

#visualizing Differences by covariate
plot(strata_topic, var_name = "genre",
     show_topic = c(1:4), by = "covariate")

#Politiker
strata_topic_2 <- by_strata_DocTopic(
  key_topics_covar,
  by_var = "genrepolitiker1",
  labels = c("other", "politiker")
)
#visualizing Differences by topics
plot(strata_topic_2, var_name = "genre",
     show_topic = c(1:4))
#visualizing Differences by covariate
plot(strata_topic_2, var_name = "genre",
     show_topic = c(1:4), by = "covariate")

#Bundestagswahl 2025
strata_topic_3 <- by_strata_DocTopic(
  key_topics_covar,
  by_var = "genrebtw20251",
  labels = c("other", "btw2025")
)
#visualizing Differences by topics
plot(strata_topic_3, var_name = "genre",
     show_topic = c(1:4))
#visualizing Differences by covariate
plot(strata_topic_3, var_name = "genre",
     show_topic = c(1:4), by = "covariate")

#Inhalte
strata_topic_4 <- by_strata_DocTopic(
  key_topics_covar,
  by_var = "genreinhalte1",
  labels = c("other", "inhalte")
)
#visualizing Differences by topics
plot(strata_topic_4, var_name = "genre",
     show_topic = c(1:4))
#visualizing Differences by covariate
plot(strata_topic_4, var_name = "genre",
     show_topic = c(1:4), by = "covariate")

#Key ATM Base
out <- keyATM(
  docs              = keyATM_docs,    
  no_keyword_topics = 5,              
  keywords          = keywords,       
  model             = "base",         
  options           = list(seed = 250)
)

saveRDS(out, file = "SAVENAME.rds")

out <- readRDS(file = "SAVENAME.rds")

top_docs(out)

out$theta  
out$phi   

fig_modelfit <- plot_modelfit(out)
fig_modelfit
#visualisiert die Log-Likelihood und Perplexität innerhalb der Stichprobe

#weiterer Forschungsansatz: Veränderung im Laufe der Zeit 
install.packages("lubridate")
library(lubridate)

btw_Datensatz_sample$datum <- c("20. Januar 2025", "01. Dezember 2024", "10. Jahnuar 2025")
btw_Datensatz_sample$datum <- dmy(btw_Datensatz_sample$datum)
btw_Datensatz_sample$day_month <- format(btw_Datensatz_sample$datum, "%d-%m") 
head(btw_Datensatz_sample$day_month)
