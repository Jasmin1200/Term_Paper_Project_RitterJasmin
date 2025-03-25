#Forschungspraktikum 1&2 Ritter 
#Teil 3: Wahlprogrammentwurf zur Btw 2025 der Grünen - Aufbereitung und Analyse

#erste packages
packages <- c("pdftools",
              "quanteda" 
)

install.packages(setdiff(packages, rownames(installed.packages())))

lapply(packages, library, character.only = TRUE)

packages <- c(
  "quanteda",             
  "quanteda.textstats",   
  "quanteda.textplots",   
  "stringr",              
  "dplyr",                
  "ggplot2"               
)

library(tidytext)
library(dplyr)
library(pdftools)

#Wahlprogramm der Grünen von PDF als Datensatz
grün_btw2025 <- pdf_text("C:/Users/jarit/Desktop/20241216_BTW25_Programmentwurf_Die Grünen.pdf")

#erstes herantasten an die Daten
str(grün_btw2025)    
head(grün_btw2025)

summary(grün_btw2025) 
length(grün_btw2025)  

nchar(grün_btw2025[1])
sum(nchar(grün_btw2025))

#Dokument bereihnigen

grün_btw2025_clean <- tolower(grün_btw2025)

grün_btw2025_clean <- gsub("\n", " ", grün_btw2025)

grün_btw2025_clean <- gsub("\\s+", " ", grün_btw2025)

grün_btw2025_clean <- gsub("[^a-zA-Z0-9ÄÖÜßäöü ]", "", grün_btw2025)

#erste 5 Seiten Deckblatt und Inhaltsverzeichnis & letzte 4 Seiten entfernen Notizen und Rücken vom Heft
grün_btw2025_clean <- grün_btw2025[6:(length(grün_btw2025) - 4)]

#Fußnote entfernen
grün_btw2025_clean <- gsub("6 BÜNDNIS 90 / DIE GRÜNEN", "", grün_btw2025_clean)

#Seitenzahl entfernen
grün_btw2025_clean <- gsub("\\s*[0-9]+\\s*$", "", grün_btw2025_clean)

#bereihnigter Datensatz
str(grün_btw2025_clean)    
head(grün_btw2025_clean)

summary(grün_btw2025_clean)
length(grün_btw2025_clean) 

nchar(grün_btw2025_clean[1]) 
sum(nchar(grün_btw2025_clean)) 

#Wort Token
toks_wort_grün_btw2025 <- tokens(grün_btw2025_clean)
head(toks_wort_grün_btw2025)

ntoken(toks_wort_grün_btw2025) 
sum(ntoken(toks_wort_grün_btw2025)) 
sum(ntype(toks_wort_grün_btw2025)) 

#Satz Token
library(tokenizers)
toks_satz_grün_btw2025 <- tokenize_sentences(grün_btw2025_clean)
print(toks_satz_grün_btw2025)
#Sätze zu lang für eine Auswertung?!

#Worttoken bereihnigen
toks_wort_grün_btw2025_clean <- tokens(grün_btw2025_clean,
                                       remove_punct = TRUE,
                                       remove_numbers = TRUE,
                                       remove_symbols = TRUE,
                                       remove_separators = TRUE) %>%
  tokens_select(pattern = stopwords("de"), selection = "remove")

class(grün_btw2025_clean)

#Document-Feature Matrix (DFM)
grün_btw2025_clean_tokens <- tokens(grün_btw2025_clean)
grün_btw2025_clean_dfm <- dfm(grün_btw2025_clean_tokens)
dfm(grün_btw2025_clean_tokens)

#entfernen von Frequent and Rare Tokens
grün_btw2025_clean_tokens <- tokens_trim(grün_btw2025_clean_tokens, 
                                         min_docfreq = 0.01, 
                                         max_docfreq = 0.5, 
                                         docfreq_type = "prop")

grün_btw2025_clean_dfm <- dfm(grün_btw2025_clean_tokens)
dfm(grün_btw2025_clean_tokens)

#übrige tokens
head(grün_btw2025_clean_dfm)
sum(ntoken(grün_btw2025_clean_dfm)) 
sum(ntype(grün_btw2025_clean_dfm))

#lemma für Wortgrundform neues package benötigt!
library(quanteda)
library(quanteda.textmodels)  

# Tokenisierung
toks <- tokens(grün_btw2025_clean)

#grundform mit Lemma - udpipe präzieser als textsteam
install.packages("udpipe")
library(udpipe)

#deutsches Sprachmodell 
ud_model <- udpipe_download_model(language = "german")
ud_model <- udpipe_load_model(ud_model$file_model)

text <- c("grün_btw2025_clean_dfm")

# udpipe Annotation (Lemmatisierung + POS-Tags)
anno <- udpipe_annotate(ud_model, x = text)
anno_df <- as.data.frame(anno)

# Zeige Token und die zugehörigen Lemmata
print(anno_df[, c("token", "lemma")])

#entfernen von sehr seltenen und sehr häufigen stoppwörtern
grün_btw2025_clean_dfm <- dfm_trim(grün_btw2025_clean_dfm, min_docfreq = 0.01, max_docfreq = 0.5, docfreq_type = "prop")

#DFM einzelene Aspekte
topfeatures(grün_btw2025_clean_dfm, 10)

# Beispielhafte Liste von Füllwörtern
stopwords <- c("und", "der", "die", "in", "zu", "mit", "auf", "für", "von", "dass", "ist", "um", "eine", "nach", "wenn", "muss", "unter", "dazu", "dabei","nur", 
               "und", "es","bis", "wie", "so", "weil", "ganz", "da", "diesen", "als", "ab", "ihnen", "statt", "er", "vom", "ob", "ins","gute", "etwa", "bei", 
               "aber","sein", "damit", "sollen")

# Entferne die Stoppwörter aus der Dokument-Term-Matrix
grün_btw2025_clean_dfm <- dfm_remove(grün_btw2025_clean_dfm, stopwords)

# Zeige die DFM ohne Stoppwörter an
print(grün_btw2025_clean_dfm)

# Bigramme
tokens_bigrams <- tokens_ngrams(toks_wort_grün_btw2025_clean, n = 2)

# DFM für die Bigramme
dfm_bigrams <- dfm(tokens_bigrams)

# Zeige die DFM mit Bigrammen an
print(dfm_bigrams)

# Die 10 häufigsten Bigramme anzeigen
top_bigrams <- topfeatures(dfm_bigrams, 20)

# Top-Bigramme anzeigen
print(top_bigrams)

#Wordcloud
library(quanteda.textplots)
textplot_wordcloud(grün_btw2025_clean_dfm, rotation = 0.25)

grün_btw2025_clean_dfm

install.packages("kableExtra")
install.packages("dplyr")
library("kableExtra")
library("dplyr")

# Die 25 häufigsten Wörter im Text berechnen
top_features <- topfeatures(grün_btw2025_clean_dfm, 25)

# Gesamtzahl der Häufigkeiten berechnet
total_frequency <- sum(top_features)

# Die 25 häufigsten Wörter in Tabellenformat
topwords_25 <- data.frame(
  Wort = names(top_features),
  Häufigkeit = top_features,
  Prozent = round((top_features / total_frequency) * 100, 2)  # Rundung auf 2 Dezimalstellen
)

#1bis25
topwords_25_clean <- topwords_25_clean %>%
  mutate(Reihenfolge = row_number())

# Tabelle
topwords_25_clean %>%
  kable("html", escape = FALSE) %>%
  kable_styling("striped", full_width = FALSE) %>%
  column_spec(1, background = "lightgreen") %>%
  column_spec(2, background = "lightgreen") %>%
  row_spec(1, bold = TRUE, background = "lightgrey")
