#Forschungspraktikum 1&2 Ritter 
#Teil 1: Zeitungsartikel als Datensatz aufbereiten

install.packages("tidyverse")  
install.packages("officer")    
install.packages("stringr")    
install.packages("quanteda")   

library(quanteda)
library(tidyverse)
library(officer)
library(stringr)

#Dateien aus dem Ordner
files <- list.files(path = "C:/Users/jarit/Desktop/btw_2025_Zeitungsartikel_Daten", pattern = "\\.DOCX$", full.names = TRUE)
length(files)

files_backup <- files

#Text extrahieren
extract_text_from_word <- function(file) {
  doc <- read_docx(file)
  text <- docx_summary(doc)  
  text <- paste(text$text, collapse = " ") 
  return(text)
}

word_texts <- lapply(files, extract_text_from_word)


#Dataframe
btw_artikel <- tibble(
  filename = basename(files),
  text = unlist(word_texts)
)

btw_artikel
str(btw_artikel)

# FPÖ Artikel enfernen, interesse nur innerhalb von DE
btw_artikel <- btw_artikel %>%
  filter(!grepl("FPÖ|Österreich|Austria Presse Agentur|OÖ Nachrichten|Kronen Zeitung|NÖN Niederösterr\\.Nachrichten|Salzburger Nachrichten", filename, ignore.case = TRUE))
btw_artikel <- btw_artikel %>%
  filter(!grepl("FPÖ|Österreich|Austria Presse Agentur|OÖ Nachrichten|Kronen Zeitung|NÖN Niederösterr\\.Nachrichten|Salzburger Nachrichten", text, ignore.case = TRUE))

head(btw_artikel) 

substr(btw_artikel$text[1:5], 1, 300)
library(stringr)


btw_artikel <- btw_artikel %>%
  mutate(
    datum = str_extract(text, "\\d{1,2}\\.\\s*[A-Za-z]+\\s*\\d{4}")
  )

head(btw_artikel$datum)

library(dplyr)
library(stringr)

#Zeitungen
btw_artikel <- btw_artikel %>%
  mutate(
    zeitung = coalesce(
      str_extract(text, "(?i)[a-zäöüß][a-zäöüß\\s-]+(?=\\s+(Montag|Dienstag|Mittwoch|Donnerstag|Freitag|Samstag|Sonntag)\\s+\\d{1,2}\\.)"),
      str_extract(text, "(?i)[A-ZÄÖÜß][A-ZÄÖÜß\\s-]+(?=\\s+(Montag|Dienstag|Mittwoch|Donnerstag|Freitag|Samstag|Sonntag)\\s+\\d{1,2}\\.)")
    )
  )

na_count_zeitung <- sum(is.na(btw_artikel$zeitung))
print(na_count_zeitung) 

#online Artikel
btw_artikel <- btw_artikel %>%
  mutate(
    online = coalesce(
      str_extract(text, "(?i)(online|digital|web|netz|de)\\s+([\\w-]+)"),
      str_extract(text, "(?i)(t-online|focus-online|spiegel-online|zeit-online|faz-online|bild-online)")
    )
  )

na_count_online <- sum(is.na(btw_artikel$online))
print(na_count_online)

#Magazine
btw_artikel <- btw_artikel %>%
  mutate(
    magazine = coalesce(
      str_extract(text, "(?i)(magazin|zeitschrift|journal|blatt)\\s+([\\w-]+)"),
      str_extract(text, "(?i)(stern|spiegel|focus|zeit-magazin|der-zeit|bild|rundschau|anzeiger|kurier|dpa)")
    )
  )

na_count_magazine <- sum(is.na(btw_artikel$magazine))
print(na_count_magazine)

#sonstige
btw_artikel <- btw_artikel %>%
  mutate(
    sonstige = str_extract(
      text, 
      "(?i)(sächsische\\.de|Stern Plus|Focus Magazin|Börsen-Zeitung|Lebensmittel Zeitung|agrarzeitung|t-online|Kölnische Rundschau|News\\.de|lebensmittelzeitung\\.net|Agence France Presse -- German|Industriestrompreis\\.De|General-Anzeiger Bonn|Kurier|Rottalter Anzeiger|Main-Spitze|News\\.de|KNA Landesdienst NRW|Hochheimer Zeitung|Allgemeine Zeitung|Berliner Kurier|Nordwest-Zeitung|Stern\\.de|dpa-AFX ProFeed|Welt am Sonntag|Groß-Gerauer Echo|Frankfurter Rundschau|Die Welt|taz, die tageszeitung|Der Standard|Kölner Stadt-Anzeiger|Märkische Oderzeitung Frankfurter Stadtbote|dpa Infoline Politik und Wirtschaft etc\\.|Berliner Zeitung|Kfz Betrieb|Hamburger Morgenpost|dpa RegioLine|Passauer Neue Presse|Bild am Sonntag|Frankfurter Neue Presse|B\\.Z\\.|BILD|Der Tagesspiegel|Capital\\.de|Luxemburger Wort|Gießner Anzeiger|Alsfelder Allgemeine|Kölner-Express|Kölner Express|Aachener Zeitung AAZ|Usinger Anzeiger|Energie & Management|Nürnberger Zeitung|Darmstädter Echo|Alt-Neuöttinger Anzeiger|Lausitzer Rundschau|Südwest Presse|Oranienburger Generalanzeiger|Starkenburger Echo|Wetterauer Zeitung|Langenthaler Tagblatt|Kreis Anzeiger|Rheinische Post|Horizont\\.net|Jüdische Allgemeine)"
    )
  )

na_count_sonstige <- sum(is.na(btw_artikel$sonstige))
print(na_count_sonstige) #1665 auf 1411 auf 855 auf 848 auf 667 auf 233 auf 123 auf 94

view(btw_artikel)

# zusätzliche Spalte 'quelle_ursprung'
btw_artikel <- btw_artikel %>%
  mutate(
    quellen = coalesce(
      zeitung,       
      sonstige,      
      online,        
      magazine       
    ),
    quellen_ursprung = case_when(
      !is.na(zeitung) ~ "zeitung",
      !is.na(sonstige) ~ "sonstige",
      !is.na(online) ~ "online",
      !is.na(magazine) ~ "magazine",
      TRUE ~ "NA"   
    )
  )

head(btw_artikel)
na_count_quellen <- sum(is.na(btw_artikel$quellen))
print(na_count_quellen) 

# Zählen, wie viele Quellen aus jeder Kategorie stammen
quellen_ursprung_count <- btw_artikel %>%
  count(quellen_ursprung)

print(quellen_ursprung_count)

#Datenaufbereitung Quellen optimieren über Na Werte Artikel in Ordner nachschlagen welche Quelle und diese hinzufügen
# jetzt bei 0 
# Extrahieren der gewünschten Spalten und nur eindeutige Kombinationen
output_table <- btw_artikel %>%
  select(filename, quellen) %>%
  distinct()

# Speichern als CSV-Datei
write.csv(output_table, "quelle_filename.csv_6", row.names = FALSE)

# Ausgabe zur Bestätigung
cat("Die Tabelle wurde erfolgreich als 'quellen_filename.csv_6' gespeichert.\n")

# Pfad zur CSV-Datei angeben 
daten_filename_quellen <- read.csv("C:/Users/jarit/Desktop/quelle_filename.csv_6")
view(daten_filename_quellen)

na_count_5 <- sum(is.na(btw_artikel$quellen))
print(na_count_5) #3 auf 0
na_quellen <- btw_artikel[is.na(btw_artikel$quellen), ]
view(na_quellen) #Tabelle leer am Ende

#Dataframe bereihnigen text Spalte 
clean_text <- function(text) {
  text <- gsub("\n", " ", text) 
  text <- gsub("\\s+", " ", text)  
  text <- gsub("[^a-zA-Z0-9 ]", "", text)  
  text <- str_replace_all(text, "[[:punct:]]", "") 
  text <- tolower(text)  
  
  # Füllwörter entfernen
  fuellwoerter <- c("also", "eben", "doch", "dann", "halt", "ja", "wirklich", 
                    "auch", "vielleicht", "dass", "deshalb", "und", "oder", "aber", "deshalb")
  text <- str_replace_all(text, paste("\\b(", paste(fuellwoerter, collapse = "|"), ")\\b", sep = ""), "")
  
  return(text)
}

#neuer Datensatz bereihingte Text Spalte
btw_artikel_clear <- btw_artikel %>%
  mutate(text = sapply(text, clean_text)) 

head(btw_artikel_clear)
str(btw_artikel_clear)
head(btw_artikel_clear$text)


output_table <- btw_artikel_clear %>%
  select(filename, text, datum, quellen) %>%
  distinct()

view(btw_artikel_clear)

#vorläufige csv Datei zum speichern
write_csv(btw_artikel_clear, "datensatz_artikel_btw_Quellen unsortiert4.csv")

datensatz_artikel_btw_Quellen_unsortiert <- read.csv("C:/Users/jarit/Desktop/datensatz_artikel_btw_Quellen unsortiert4.csv")
view(datensatz_artikel_btw_Quellen_unsortiert)

# Kombinieren der Spalten
btw_artikel_clear$artikelquelle <- ifelse(!is.na(data$sonstige), data$sonstige, 
                               ifelse(!is.na(data$zeitung), data$zeitung,
                                      ifelse(!is.na(data$magazine), data$magazine,
                                             ifelse(!is.na(data$online), data$online, 
                                                    ifelse(!is.na(data$quellen), data$quellen, NA)))))

# Überprüfen der neuen Spalte
head(btw_artikel_clear)
view(btw_artikel_clear)

# Funktion zum Extrahieren der letzten beiden Wörter
extract_last_two_words <- function(text) {
  words <- unlist(strsplit(text, "\\s+"))  
  if (length(words) > 5) {
    return(paste(tail(words, 2), collapse = " "))  
  } else {
    return(text)  # Wenn es weniger als 6 Wörter gibt, bleibt der Text unverändert
  }
}


# Anwenden der Funktion auf die Spalte
btw_artikel_clear$artikelquelle <- sapply(data$artikelquelle, extract_last_two_words)

# Überprüfen des Ergebnisses
view(btw_artikel_clear$artikelquelle)

# Überprüfe, ob 'artikelquelle' im Datensatz existiert
colnames(btw_artikel_clear)

#speichern
# Erstellen des output_table mit den gewünschten Spalten und distinct()
output_table <- btw_artikel_clear %>%
  select(filename, text, datum, artikelquelle) %>%
  distinct()

# Speichern des gefilterten output_table als CSV-Datei
write_csv(output_table, "C:/Users/jarit/Desktop/Datensatz_Zeitungsartikel_btw_2025.csv")
