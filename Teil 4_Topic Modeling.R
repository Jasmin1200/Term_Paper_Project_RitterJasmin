#Forschungspraktikum 1&2 Ritter 
#Teil 4: Topic Modeling

packages <- c("dplyr", "magrittr", "quanteda", "stm","tidyverse","topicmodels")
install.packages(setdiff(packages, rownames(installed.packages())))  

lapply(packages, library, character.only = TRUE)

Datensatz_Zeitungsartikel_btw_2025 <- read.csv("C:/Users/jarit/Desktop/Datensatz_Zeitungsartikel_btw_2025.csv")

Datensatz_Zeitungsartikel_btw_2025$artikelquelle <- tolower(Datensatz_Zeitungsartikel_btw_2025$artikelquelle)
Datensatz_Zeitungsartikel_btw_2025$filename <- tolower(Datensatz_Zeitungsartikel_btw_2025$filename)

head(Datensatz_Zeitungsartikel_btw_2025$filename)
head(Datensatz_Zeitungsartikel_btw_2025$artikelquelle)

view(Datensatz_Zeitungsartikel_btw_2025)

#Topic Modeling
head(Datensatz_Zeitungsartikel_btw_2025$text)
nrow(Datensatz_Zeitungsartikel_btw_2025) #2008
colnames(Datensatz_Zeitungsartikel_btw_2025) #"filename", "text", "datum", "artikelquelle"

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
    themen = case_when(
      str_detect(text, "friedrich|merz|habeck|scholz|trump") ~ "politiker",
      str_detect(text, "prozent|bundestag|deutschen|wahlkampf|februar") ~ "btw2025",
      str_detect(text, "afd|union|link|fdp|parteien") ~ "parteien",   
      str_detect(text, "menschen|ukrain|berlin|politik|euro") ~ "inhalte",
      TRUE ~ NA_character_  # Optional: Falls keine Kategorie zutrifft
    )
  )

table(btw_Datensatz_sample$themen)  

str(btw_Datensatz_sample)

#corpus und tokens
corpus_tm <- corpus(as.character(btw_Datensatz_sample$text),
                    docvars = data.frame(themen = btw_Datensatz_sample$themen
                    ))

tokens_tm <- tokens(corpus_tm, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE)

# Stopwords definiert und entfernt
de_stopwords <- stopwords("de")
tokens_tm <- tokens_remove(tokens_tm, de_stopwords, case_insensitive = TRUE)

my_stopwords <- c(
  de_stopwords, 
  "der", "die", "das", "ein", "eine", "einer", "als", "in", "ber", 
  "sagt", "sagte", "jahr", "gibt", "wurde", "sollen", "machen", 
  "gerne", "uhr", "schon", "kennen", "immer", "müssen", "seit", 
  "wer", "werden", "mal", "konnte", "kommt", "müsse", "seien", "s",
  "knnen", "dafr", "grne", "mssen", "geht", "ab", "bereit", "beim", "zudem","weiterlesen"
)

tokens_tm <- tokens_remove(tokens_tm, my_stopwords, case_insensitive = TRUE)

# Trimme Tokens nach Häufigkeit
tokens_tm <- tokens_trim(tokens_tm,
                         min_docfreq = 0.01,
                         max_docfreq = 0.5,
                         docfreq_type = "prop")

tokens_tm <- tokens_wordstem(tokens_tm)

#dfm
dfm_Datensatz_Zeitungsartikel_tm <- dfm(tokens_tm)

dfm_Datensatz_Zeitungsartikel_tm <- dfm_Datensatz_Zeitungsartikel_tm[!rowSums(dfm_Datensatz_Zeitungsartikel_tm) == 0, ] # remove empty rows

nrow(dfm_Datensatz_Zeitungsartikel_tm) 
ncol(dfm_Datensatz_Zeitungsartikel_tm) 

head(dfm_Datensatz_Zeitungsartikel_tm)
summary(dfm_Datensatz_Zeitungsartikel_tm)

# Genre-Variable sollte als Vektor vorliegen, der die gleiche Länge wie die Dokumente hat
themen_vector <- as.factor(Datensatz_Zeitungsartikel_btw_2025$themen)  # Falls genre als Spalte im DataFrame vorliegt

#STM
k <- 20

datensatz_themen <- stm(
  dfm_Datensatz_Zeitungsartikel_tm,  
  K = k,  
  prevalence = ~ 1,  
  verbose = FALSE
)

btw_Datensatz_sample$themen <- factor(btw_Datensatz_sample$themen)
summary(datensatz_themen)

#Plot der themen
plot(datensatz_themen)

#mit weiteren k Werten getestet:

#k <- 15
#datensatz_themen <- stm(
  #dfm_Datensatz_Zeitungsartikel_tm,  # Die DFM
  #K = k,  # Anzahl der Themen
  #prevalence = ~ 1,  
  #verbose = FALSE  
#)
#btw_Datensatz_sample$themen <- factor(btw_Datensatz_sample$themen)
#summary(datensatz_themen)
#Plot der themen
#plot(datensatz_themen)

#k <- 10 # number of topics
#datensatz_themen <- stm(
  #dfm_Datensatz_Zeitungsartikel_tm,  # Die DFM
  #K = k,
  #prevalence = ~ 1,  
  #verbose = FALSE  
#)
#btw_Datensatz_sample$themen <- factor(btw_Datensatz_sample$themen)
#summary(datensatz_themen)
#Plot der themen
#plot(datensatz_themen)

#k <- 30 
#datensatz_themen <- stm(
  #dfm_Datensatz_Zeitungsartikel_tm,  
 #K = k,  
  #prevalence = ~ 1,  
  #verbose = FALSE  
#)
#btw_Datensatz_sample$themen <- factor(btw_Datensatz_sample$themen)
#summary(datensatz_themen)
#plot(datensatz_themen)

#10 kommt bei K=20 am häufigsten vor, welche Artikel arbeiten mit diesen Theman?
length(btw_Datensatz_sample$text)  
length(datensatz_themen$filename)  
head(btw_Datensatz_sample$filename)
summary(dfm_Datensatz_Zeitungsartikel_tm) 
nrow(dfm_Datensatz_Zeitungsartikel_tm)  
ncol(dfm_Datensatz_Zeitungsartikel_tm)  
docnames(dfm_Datensatz_Zeitungsartikel_tm)[1:10] 

# n=10 Artikel mit den Themen
findThoughts(datensatz_themen, texts = btw_Datensatz_sample$filename, n = 10, topics = 10)

# Alle Themenanteile extrahieren
topic_proportions <- datensatz_themen$theta  
str(datensatz_themen$theta)

# Indizes der Dokumente, bei denen Thema 10 am stärksten vertreten ist
top_documents_topic10 <- order(topic_proportions[, 10], decreasing = TRUE)[1:10]

# Anzeigen der entsprechenden Artikel
btw_Datensatz_sample$filename[top_documents_topic10]
view(btw_Datensatz_sample$filename[top_documents_topic10])

#Tabelle Top Thema 10
top_documents_topic10_filenames <- btw_Datensatz_sample$filename[top_documents_topic10]
top_documents_topic10_proportions <- topic_proportions[top_documents_topic10, 10]
top_documents_table <- data.frame(
  "Artikelname" = top_documents_topic10_filenames,
  "Themenanteil für Thema 10" = top_documents_topic10_proportions
)

print(top_documents_table)
view (top_documents_table)

#Unterschiede zwischen Genres testen
genre <- dfm_Datensatz_Zeitungsartikel_tm@docvars$Datensatz_themen  

#genre als Faktor
genre <- as.factor(genre)

# Effekte der Klasse auf die Themenverhältnisse
class_diff <- estimateEffect(1:k ~ genre,
                             datensatz_themen,
                             meta = dfm_Datensatz_Zeitungsartikel_tm@docvars)

#Visualisierung der Unterschiede
par(cex.axis = 0.7,    
    cex.lab = 0.7)

plot(class_diff,
     topics = 10,
     covariate = "genre",
     method = "pointestimate",
     main = "Topic 10"
)

#opitionale Anzahl an Themen (k-Wert) finden
out <- convert(dfm_Datensatz_Zeitungsartikel_tm, to = "stm")

set.seed(500)
kresult <- searchK(out$documents,
                   out$vocab,
                   seq(10, 30, 10), # 10 to 30, in 10
                   prevalence = ~ genre,
                   data = out$meta,
                   verbose = FALSE
)

plot(kresult)

#Für verschiedene Seeds und k-Werte getestet:

#set.seed(1000)
#kresult <- searchK(out$documents,
                   #out$vocab,
                   #seq(10, 30, 10), # 10 to 30, in 10
                   #prevalence = ~ genre,
                   #data = out$meta,
                   #verbose = FALSE
#)

#plot(kresult)

#set.seed(1000)
#kresult <- searchK(out$documents,
                   #out$vocab,
                   #seq(10, 20, 10), # 10 to 20, in 10
                   #prevalence = ~ genre,
                   #data = out$meta,
                   #verbose = FALSE
#)

#plot(kresult)


#set.seed(1000)
#kresult <- searchK(out$documents,
                   #out$vocab,
                   #seq(10, 40, 10), # 10 to 20, in 10
                   #prevalence = ~ genre,
                   #data = out$meta,
                   #verbose = FALSE
#)

#plot(kresult)

# Exklusivität und semantischer Kohärenz
plot(kresult$results$exclus, kresult$results$semcoh,
     xlab = "Semantic Coherence",
     ylab = "Exclusivity")

text(kresult$results$exclus, kresult$results$semcoh, 
     labels = kresult$results$K, pos = 4)

#Evaluate Actual Topics
topicQuality(model = datensatz_themen,
             documents = dfm_Datensatz_Zeitungsartikel_tm)
