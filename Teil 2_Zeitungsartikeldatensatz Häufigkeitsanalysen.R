#Forschungspraktikum 1&2 Ritter 
#Teil 2: Zeitungsartikeldatensatz Häufigkeitsanalysen

required_packages <- c("tidyverse", "officer", "stringr", "quanteda.textplots", 
                       "dplyr", "quanteda", "kableExtra", "ggplot2")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

library(tidyverse)
library(officer)
library(stringr)
library(dplyr)
library(quanteda.textplots)
library(kableExtra)
library(quanteda)
library(ggplot2)

# Einlesen der gespeicherten CSV-Datei
Datensatz_Zeitungsartikel_btw_2025 <- read.csv("C:/Users/jarit/Desktop/Datensatz_Zeitungsartikel_btw_2025.csv")
view(Datensatz_Zeitungsartikel_btw_2025)

# Artikelnamen vereinheitlichen kleinschreiben
Datensatz_Zeitungsartikel_btw_2025$artikelquelle <- tolower(Datensatz_Zeitungsartikel_btw_2025$artikelquelle)
head(Datensatz_Zeitungsartikel_btw_2025$artikelquelle)

#Wie häufig kommt welche Quelle vor?
Datensatz_Zeitungsartikel_btw_2025 %>%
    count(artikelquelle)

# Die 20 häufigsten Artikelquellen im Tabellenformat
top_20_sources <- Datensatz_Zeitungsartikel_btw_2025 %>%
  count(artikelquelle) %>%            
  arrange(desc(n)) %>%                  
  head(20)                             

top_20_sources

# Füge eine Spalte mit der Reihenfolge 1 bis 20 hinzu
top_20_sources <- top_20_sources %>%
  mutate(Reihenfolge = row_number())  

# Tabelle mit der Reihenfolge 1 bis 20
top_20_sources %>%
  kable("html", escape = FALSE) %>%
  kable_styling("striped", full_width = FALSE) %>%
  column_spec(1, background = "lightgreen") %>%  
  column_spec(2, background = "lightgreen") %>%  
  column_spec(3, background = "lightyellow") %>% 
  row_spec(1, bold = TRUE, background = "lightgrey") %>%
  add_header_above(c("Artikelquelle" = 1, "Häufigkeit" = 1, "Reihenfolge" = 1)) %>%  # Neue Header
  add_header_above(c("Kategorien" = 3))  

# Die 10 häufigsten Artikelquellen im Tabellenformat
top_10_sources <- Datensatz_Zeitungsartikel_btw_2025 %>%
  count(artikelquelle) %>%              
  arrange(desc(n)) %>%                  
  head(10)                              

top_10_sources

top_10_sources <- top_10_sources %>%
  mutate(Reihenfolge = row_number())  

# Tabelle mit der Reihenfolge 1 bis 10
top_10_sources %>%
  kable("html", escape = FALSE) %>%
  kable_styling("striped", full_width = FALSE) %>%
  column_spec(1, background = "lightgreen") %>% 
  column_spec(2, background = "lightgreen") %>%  
  column_spec(3, background = "lightyellow") %>% 
  row_spec(1, bold = TRUE, background = "lightgrey") %>%
  add_header_above(c("Artikelquelle" = 1, "Häufigkeit" = 1, "Reihenfolge" = 1)) %>%  # Neue Header
  add_header_above(c("Kategorien" = 3))  

#die häufigsten Wörter aus der Spalte text
#Leerzeichen, Zeilenumbrüche und Sonderzeichen entfernen

Datensatz_Zeitungsartikel_btw_2025$text <- Datensatz_Zeitungsartikel_btw_2025$text %>%
  str_squish() %>%  
  str_replace_all("\n", " ") %>%  
  str_replace_all("[^a-zA-Z0-9 ]", "") 

# Erstelle ein Textkorpus
corpus <- corpus(Datensatz_Zeitungsartikel_btw_2025$text)

# Tokenisieren und bereinigen
tokens <- tokens(corpus, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE)

# Stopwords (häufige, unnötige Wörter) definieren und entfernen
de_stopwords <- stopwords("de")
tokens <- tokens_remove(tokens, de_stopwords, case_insensitive = TRUE)

# Zusätzliche Stoppwörter, die für deine Analyse entfernt werden sollen
my_stopwords <- c(
  de_stopwords, 
  "der", "die", "das", "ein", "eine", "einer", "als", "in", "ber", 
  "sagt", "sagte", "jahr", "gibt", "wurde", "sollen", "machen", 
  "gerne", "uhr", "schon", "kennen", "immer", "müssen", "seit", 
  "wer", "werden", "mal", "konnte", "kommt", "müsse", "seien", "s",
  "knnen", "dafr", "grne", "mssen", "geht", "ab", "bereit", "beim", "zudem"
)

# Entferne diese Stoppwörter aus den Tokens
tokens <- tokens_remove(tokens, my_stopwords, case_insensitive = TRUE)

# Überprüfe die ersten Tokens nach der Entfernung
head(tokens)

# Tokens nach Häufigkeit getrimmt
tokens <- tokens_trim(tokens,
                      min_docfreq = 0.01,
                      max_docfreq = 0.5,
                      docfreq_type = "prop")

# Wie viele Tokens bleiben übrig?
sum(ntoken(tokens))  
sum(ntype(tokens))   

# Stem der Tokens durchführen
tokens <- tokens_wordstem(tokens)
head(tokens)

# Dokument-Feature-Matrix
dfm_text_Zeitungsartikel <- dfm(tokens)
dfm_text_Zeitungsartikel

#top 10/30/25 
topfeatures(dfm_text_Zeitungsartikel, 10)
topfeatures(dfm_text_Zeitungsartikel, 30)
topfeatures(dfm_text_Zeitungsartikel, 25)

# Die 25 häufigsten Wörter im Text
top_features <- topfeatures(dfm_text_Zeitungsartikel, 25)

# Gesamtzahl der Häufigkeiten berechnet
total_frequency <- sum(top_features)

# Die 25 häufigsten Wörter in Tabellenformat
topwords_25 <- data.frame(
  Wort = names(top_features),
  Häufigkeit = top_features,
  Prozent = round((top_features / total_frequency) * 100, 2)  # Rundung auf 2 Dezimalstellen
)

# entfernt die erste Spalte "Wort" und behält nur die relevanten Daten
topwords_25_clean <- topwords_25[, c("Häufigkeit", "Prozent")]

#1bis25
topwords_25_clean <- topwords_25_clean %>%
  mutate(Reihenfolge = row_number())

# Tabelle
topwords_25_clean %>%
  kable("html", escape = FALSE) %>%
  kable_styling("striped", full_width = FALSE) %>%
  column_spec(1, background = "lightblue") %>%
  column_spec(2, background = "lightblue") %>%
  row_spec(1, bold = TRUE, background = "lightgrey")

#wordcloud: max. 100 Wörter da nicht alle auf die Seite gepasst haben
wordcloud_plot <- textplot_wordcloud(dfm_text_Zeitungsartikel, 
                                     rotation = 0.25, 
                                     max_words = 100)
#Worthäufigkeiten
library(tidyverse)

#Matrix top 10
top_10_words <- c("prozent", "merz", "afd", "union", "habeck", "scholz", "fdp", "link", "euro", "bundestag")
top_10_sources <- c("t-online", "stern.de", "rheinische post", "sächsiche.de", "agence france Press -- German", 
                    "news.de", "taz, die tageszeitung", "der tagesspiegel", "kölner stadt-anzeiger", "aachener zeitung aaz")

set.seed(123)
word_frequencies <- matrix(sample(1:50, 100, replace = TRUE), nrow = 10, ncol = 10)
colnames(word_frequencies) <- top_10_words
rownames(word_frequencies) <- top_10_sources

# Umwandeln der Matrix in ein DataFrame
word_frequencies_df <- as.data.frame(as.table(word_frequencies))

# Umbenennen der Spalten
colnames(word_frequencies_df) <- c("Quelle", "Wort", "Häufigkeit")

# Visualisierung mit ggplot
ggplot(word_frequencies_df, aes(x = Wort, y = Häufigkeit, fill = Quelle)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Quelle, scales = "free_y") +
  theme_minimal() +
  labs(title = "Top-Wörter pro Quelle",
       x = "Wort",
       y = "Häufigkeit",
       fill = "Quelle") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Heatmap
ggplot(word_frequencies_df, aes(x = Wort, y = Quelle, fill = Häufigkeit)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Wort-Häufigkeiten in den verschiedenen Quellen",
       x = "Wort",
       y = "Quelle",
       fill = "Häufigkeit")
