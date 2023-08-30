library(tidyverse)
library(tm)
library(tidytext)
library(wordcloud)
library(corpus)
library(ngram)
library(devtools)
library(tidyverse)
library(devtools)
library(dplyr)



standup20162017 <- read.table("20162017.txt", fill = TRUE,
                              col.names = c("autori", "titolo", "anno", "testo"), sep="@", quote = "")

standup20192021 <- read.table("20192021.txt", fill = TRUE,
                              col.names = c("autori", "titolo", "anno", "testo"), sep="@", quote = "")

standup20112015 <- read.table("20112015.txt", fill = TRUE,
                              col.names = c("autori", "titolo", "anno", "testo"), sep="@", quote = "")

standup20182019 <- read.table("20182019.txt", fill = TRUE,
                              col.names = c("autori", "titolo", "anno", "testo"), sep="@", quote = "")



standup <- rbind(standup20112015, standup20162017,
                 standup20182019, standup20192021)
standup <- as_tibble(standup)
standup <- standup %>% as_tibble()
standup$autori
standup$titolo
unique(standup$autori)


# Si divide ogni singolo spettacolo in modo da avere per ogni riga una parola
tidyStandup<- standup %>%
  unnest_tokens("word", testo)


# Parole più frequenti: poco utile
paroleFREQ <- tidyStandup %>%
  count(word) %>%
  arrange(desc(n))



# Si tolgono le stop words

custom <- add_row(stop_words, word = "im", lexicon = "custom")
custom <- add_row(custom, word = c("im", "id", "lets", "dont", "gonna","laugh", 
                                   "laughter", "youre", "wasnt","crowd", "voice",
                                   "audienc", "don", "hes", "theyr", "didnt",
                                   "didn", "doesn", "ive", "lot", "ill", "uh",
                                   "cheers", "cheering", "shes", "gotta", "applause",
                                   "wanna", "ve", "ll", "ladi", "bit", "doesnt", "youre",
                                   "theyre", "laughs", "audience", "laughing",
                                   "music plays", "piano plays", "song playing",
                                   "clap","claps","clapping","ah", "ha", "lai",
                                   "yeah", "hey", "blah", "chuckles",
                                   "sa","dn", "te", "announcer"), lexicon = "custom")
tidyStandup <- tidyStandup %>% anti_join(custom)


# Stemming
library(SnowballC)
tidyStandup<-tidyStandup %>%
  mutate_at("word", funs(wordStem((.), language="en")))




# è possibile fare meglio:
tidyStandup %>% count(word, sort = TRUE) %>% 
  filter(n > 1550) %>%  
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n, fill = "red")) + geom_col(show.legend = F) + coord_flip()




# Si fa il word cloud post stemming:
# con pull() si estrae un vettore
wordcloud(
  tidyStandup %>% count(word) %>% 
    pull(word),
  tidyStandup %>% count(word) %>% 
    pull(n), max.words = 80, fixed.asp = T, random.order = F, rot.per = 0.35,
  colors = brewer.pal(8, "Dark2"), res=170, min.freq = 25)




# BIGRAMMI

standup1 <- tibble(text = standup$testo, autori = standup$autori, 
                   numerazione = 1:nrow(standup))


bigrammi <- standup1 %>% 
  unnest_tokens(bigrammi, text, token = "ngrams", n = 2) %>% 
  count(bigrammi, sort = TRUE)



# si hanno diviso tutti i testi in bigrammi, ma ci sono le stop words
standup_bigrams <- standup %>% unnest_tokens(bigram, testo, token = "ngrams", 
                                             n = 2)
                           
bigrams_separated <- standup_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")


bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% custom$word) %>%
  filter(!word2 %in% custom$word)


bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

bigrams_filtered_unite <- unite(bigram_counts, "bigrammi", 1:2, remove = T, sep = " ")

View(bigrams_filtered_unite)


# Queste sono le persone famose maggiormente citate nei bigrammi:
bigrams_MostFrequentPeople <- bigrams_filtered_unite[c(5,27,31,48,68,86,
                                                        102,117,127,146),]
bigrams_MostFrequentPeople


# Grafico bigrammi vip negli anni:

tableTRUMP <- table(standup_bigrams$anno, standup_bigrams$bigram == "donald trump")
tableTRUMP

tableMJ <- table(standup_bigrams$anno, standup_bigrams$bigram == "michael jackson")
tableMJ

tableCOSBY <- table(standup_bigrams$anno, standup_bigrams$bigram == "bill cosby")
tableCOSBY


tableBJENNER <- table(standup_bigrams$anno, standup_bigrams$bigram == "bruce jenner")
tableBJENNER

tableCJENNER <- table(standup_bigrams$anno, standup_bigrams$bigram == "caitlyn jenner")
tableCJENNER

tableCLINTON <- table(standup_bigrams$anno, standup_bigrams$bigram == "bill clinton")
tableCLINTON
# NB: BILL CLINTON compare quasi esclusivamente in un singolo spettacolo. Sarà escluso dall'analisi successiva


tableOBAMA <- table(standup_bigrams$anno, standup_bigrams$bigram == "barack obama")
tableOBAMA

tableSUM <-  (tableTRUMP[,2]+tableTRUMP[,1])
tableSUM


# Si crea un tibble coi dati costruiti in precedenza:
grafTRUMP <- tibble(x = 2011:2021, trump = 100 *tableTRUMP[,2]/tableSUM,
                    obama = 100 *tableOBAMA[,2]/tableSUM, 
                    mj = 100*tableMJ[,2]/tableSUM,
                    cosby = 100*tableCOSBY[,2]/tableSUM,
                    jenner = (100*tableBJENNER[,2] + tableCJENNER[,2])/tableSUM)



# Si traccia il grafico relativo alla frequenza dei bigrammi dei VIP negli anni
ggplot(data=grafTRUMP, aes(x=x)) +
  geom_line(aes(y = trump, col = "Donald Trump"))+
  geom_point( aes(y = trump, col = "Donald Trump"))+
  geom_line(aes(y = obama, col = "Barack Obama")) +
  geom_point(aes(y = obama, col = "Barack Obama")) +
  geom_line(aes(y = mj, col = "Michael Jackson")) +
  geom_point(aes(y = mj, col = "Michael Jackson")) +
  geom_line(aes(y = cosby, col = "Bill Cosby")) +
  geom_point(aes(y = cosby, col = "Bill Cosby")) +
  geom_line(aes(y = jenner, col = "Caitlyn Jenner")) +
  geom_point(aes(y = jenner, col = "Caitlyn Jenner"))+
  scale_colour_manual(values=c("blue", "green","orange","red", "purple")) +
  labs(colour="Celebrità", x="Anno", y="Percentuale di presenza del bigramma nell'anno per 100")
  
  
  

# Si costruiscono i trigrammi:
trigrammi <- standup1 %>% 
  unnest_tokens(trigrammi, text, token = "ngrams", n = 3) %>% 
  count(trigrammi, sort = TRUE)




# Divisi tutti i testi in trigrammi, ma ci sono le stop words
standup_trigrams <- standup %>% unnest_tokens(trigrammi, testo, token = "ngrams", 
                                             n = 3) 


trigrams_separated <- standup_trigrams %>%
  separate(trigrammi, c("word1", "word2", "word3" ), sep = " ")


trigrams_filtered <- trigrams_separated %>%
  filter(!word1 %in% custom$word) %>%
  filter(!word2 %in% custom$word) %>% 
  filter(!word3 %in% custom$word)


trigram_counts <- trigrams_filtered %>%
  count(word1, word2, word3, sort = TRUE)

trigrams_filtered_unite <- unite(trigram_counts, "trigrammi", 1:3, remove = T, sep = " ")

View(trigrams_filtered_unite)

# I trigrammi sono poco informativi





# Costruzione metrica volgarità

tidyStandupV <- standup %>%
  unnest_tokens("word", testo)

nt <- standup$testo %>% length()

# SI prealloca vettore scores volgarità
volgarity_scores <- rep(NA, nt)

# Si carica blacklist
blacklist <- read.table("blacklist.txt", sep=",", header=FALSE)

blacklist <- blacklist %>% t %>% as_tibble() %>% rename(word = V1)

# Testi senza parole volgari
friendly.tidystandup <- tidyStandupV %>% anti_join(blacklist)

# Valore volgarità per ciascun testo
volgarity_scores <- (tidyStandupV %>% count(titolo) %>% pull(n) - 
                       friendly.tidystandup %>% count(titolo) %>% pull(n)) /
  tidyStandupV %>% count(titolo) %>% pull(n)

# Indici due spettacoli più volgari e due meno volgari
max <- which.max(volgarity_scores)
quasimax <- which.max(volgarity_scores[-208]) 
min <- which.min(volgarity_scores)
quasimin <- which.min(volgarity_scores[-200]) 

# Punteggio volgarità, nome autore e show di questi 4 spettacoli
volgarity_scores[c(max, quasimax, quasimin, min)] 
standup$titolo[c(max, quasimax, quasimin, min)] 
standup$autori[c(max, quasimax, quasimin, min)]

summary(volgarity_scores)
boxplot(volgarity_scores)





# Si prova a classificare considerando la volgarità come variabile in due livelli:

standupVolg <- standup %>% mutate(volgarita = volgarity_scores)
head(standupVolg)


standupVolgDISCR <- standupVolg %>% 
  mutate(volgaritaDISCR = cut(standupVolg$volgarita, breaks = 2)) %>% 
  select(-c(volgarita))
head(standupVolgDISCR) 





# Si crea la DTM, DOCUMENT TERM MATRIX.

corpusDISCR <- VectorSource(standupVolgDISCR$testo) %>% Corpus()

dtmDISCR <- DocumentTermMatrix(corpusDISCR, 
                          control = list(
                            stopwords = "english",
                            removeNumbers = TRUE
                          )) %>% 
  removeSparseTerms(.980)  
# si evita che venga sparsa, rimuovendo parole poco frequenti 
# con la riga precedente



tempDISCR <- as.matrix(dtmDISCR)
View(tempDISCR)
dim(tempDISCR)



# Si crea un conteggio sul numero di parole e caratteri dello spettacolo:


n.paroleDISCR<-NA
n.caratteriDISCR<-NA
for(i in 1:nrow(standupVolgDISCR)){
  n.paroleDISCR[i]<-wordcount(standupVolgDISCR$testo[i])
  n.caratteriDISCR[i]<-nchar(standupVolgDISCR$testo[i], 
                        type = "chars", allowNA = FALSE, 
                        keepNA = NA)
}
# per ogni tweet conto parole e caratteri. Insieme alla DTM le 
# userò per creare una tabella in cui includo la DTM, il numero
# di parole e caratteri, e 
# la y, che sarebbe la previsione della volgarità





bigdataDISCR <- data.frame(tempDISCR)
bigdataDISCR <- bigdataDISCR %>%
  mutate(volgaritaDISCR = factor(standupVolgDISCR$volgaritaDISCR)) %>%
  mutate(n.paroleDISCR = n.paroleDISCR) %>%
  mutate(n.caratteriDISCR = n.caratteriDISCR)

# Faccio training e test:
set.seed(123)
split2 <- initial_split(bigdataDISCR)
train2 <- training(split2)
test2 <- testing(split2)



t2 <- rpart(volgaritaDISCR ~ ., data = train2, method = "class")
plot(t2)
text(t2)
# Per nulla informativo






### SENTIMENT ANALYSIS

# Si cercano i tre spettacoli del 2019-2020 pù volgari e i tre meno volgari
sorted_standup <- standupVolg %>% arrange(volgarity_scores)

view(sorted_standup[,-4])

sstandup <- sorted_standup %>% filter(anno >= 2019)
view(sstandup[,-4])

# si caricano i tweet che parlano dei 6 spettacoli di interesse
SebastianManiscalco <- read.table("sebastianManiscalcoStayHungry.txt", fill = TRUE, sep = "*")

RobSchneider <- read.table("RobSchneiderAsianMommaMexicanKids.txt", fill = TRUE, sep = "*")

MarcMaron <- read.table("MarcMaronEndTimesFun.txt", fill = TRUE, sep = "*")

DaveChappelle <- read.table("DaveChappelleSticksAndStones.txt", fill = TRUE, sep = "*")

HannahGadsby <- read.table("HannahGadsbyDouglas.txt", fill = TRUE, sep = "*")

JimmyCarr <- read.table("JimmyCarrTheBestOfUltimateGoldGreatestHits.txt", fill = TRUE, sep = "*")

# si trasformano in tibble e si aggiungono le variabili che indicano il nome dell'auotre e dello show
SebastianManiscalco <- SebastianManiscalco %>% as_tibble %>% mutate(tweet = V1, V1 = NULL) %>% mutate(comedian = "SM", show = "StayHungry")
RobSchneider <- RobSchneider %>% as_tibble %>% mutate(tweet = V1, V1 = NULL) %>% mutate (comedian = "RS", show = "Asian Momma Mexican Kids")
MarcMaron <- MarcMaron %>% as_tibble %>% mutate(tweet = V1, V1 = NULL) %>% mutate(comedian = "MM", show = "End Times Fun")
DaveChappelle <- DaveChappelle %>% as_tibble %>% mutate(tweet = V1, V1 = NULL) %>% mutate (comedian = "DC", show = "Sticks And Stones")
HannahGadsby <- HannahGadsby %>% as_tibble %>% mutate(tweet = V1, V1 = NULL) %>% mutate(comedian = "HG", show = "Douglas")
JimmyCarr <- JimmyCarr %>% as_tibble %>% mutate(tweet = V1, V1 = NULL) %>% mutate (comedian = "JC", show = "The Best Of Ultimate Gold Greatest Hits")

# si uniscono i 6 tibble
data <- rbind(SebastianManiscalco, JimmyCarr, DaveChappelle, HannahGadsby, MarcMaron, RobSchneider)

tidydata <- data %>%
  unnest_tokens(word, tweet)

# parole più usate per show
words_by_show <- tidydata %>%
  count(show, word, sort=TRUE) %>%
  ungroup

# sentiment value dei 6 spettacoli
show_sentiments <- words_by_show %>%
  inner_join(get_sentiments("afinn"), by= "word") %>%
  group_by(show) %>%
  summarize(value = sum(value * n) / sum(n))

# grafico degli show per average sentiment value e volgarità
show_sentiments %>%
  mutate(volgarity = c(0.0888, 0.0497, 0.0553, 0.0006, 0.0025, 0.0013)) %>%
  mutate(show = reorder(show, value)) %>%
  ggplot(aes(value, show, fill = volgarity)) +
  geom_col(show.legend = FALSE) +
  geom_label(aes(label = volgarity), col = "white", nudge_x = -0.1) +
  labs(x = "Average sentiment value", y = NULL)

# analisi su quali parole contribuiscono più alla sentyment

# contributo singole parole ai sentiment value
contributions <- tidydata %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(word) %>%
  summarize(occurences = n(),
            contribution = sum(value))

# grafico parole che contribuiscono maggioromente
contributions %>%
  slice_max(abs(contribution), n = 25) %>%
  mutate(word = reorder(word, contribution)) %>%
  ggplot(aes(contribution, word, fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  labs(y = NULL)

# parole che contribuiscono di più per spettacolo
top_sentiment_words <- words_by_show %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  mutate(contribution = value * n / sum(n))

# grafico parole che contribuiscono di più per spettacolo
top_sentiment_words %>%
  group_by(show) %>%
  slice_max(abs(contribution), n = 12) %>%
  ungroup() %>%
  mutate(show = reorder(show, contribution),
         word = reorder_within(word, contribution, show)) %>%
  ggplot(aes(contribution, word, fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  facet_wrap(~ show, scales = "free") +
  labs(x = "Sentiment value * # of occurrences", y = NULL)

# si vede che rob influisce fortmente negativamente per lo spettacolo di rob maniscalco, ma è presente solo perchè
# è il nome dell'autore dello show, usato nella ricerca su twitter

# togliamo allora tutte le parole usate nella ricerca su twitter e altre parole confondenti e stopwors

customS <- add_row(stop_words, word = "rob", lexicon = "custom")
customS <- add_row(customS, word = c("comedy", "special", "netflix", "sebastian"
                                   , "maron", "gadsby", "jimmy", "carr", "dave", "chappelle", "marons", "hannah", "shit", "fuck"
                                   , "fucking", "fuking"), lexicon = "custom")

tidydata <- tidydata %>% anti_join(customS)

# analizziamo i trigrammi e i bigrammi

bigrammiS <- data %>% 
  unnest_tokens(bigrammiS, tweet, token = "ngrams", n = 2) %>% 
  count(bigrammiS, sort = TRUE)

bigramsS <- data %>% unnest_tokens(bigramS, tweet, token = "ngrams", 
                                  n = 2)

bigrams_separatedS <- bigramsS %>%
  separate(bigramS, c("word1", "word2"), sep = " ")


bigrams_filteredS <- bigrams_separatedS %>%
  filter(!word1 %in% custom$word) %>%
  filter(!word2 %in% custom$word)


bigram_countsS <- bigrams_filteredS %>%
  count(word1, word2, sort = TRUE)

bigram_counts1S <- bigrams_separatedS %>%
  count(word1, word2, sort = TRUE)

bigrams_filtered_uniteS <- unite(bigram_countsS, "bigrammi", 1:2, remove = T, sep = " ")

# lista parole che possono invertire polarità di una parola se la precedono
ambiguity_words <- c("not", "without", "no", "can't", "don't", "won't", "as", "in", "holy")

# grafico bigrammi che possono dare problemi nella sentiment
bigram_counts1S %>%
  filter(word1 %in% ambiguity_words) %>%
  count(word1, word2, wt = n, sort = TRUE) %>%
  inner_join(get_sentiments("afinn"), by = c(word2 = "word")) %>%
  mutate(contribution = value * n) %>%
  group_by(word1) %>%
  slice_max(abs(contribution), n = 3) %>%
  ungroup() %>%
  mutate(word2 = reorder_within(word2, contribution, word1)) %>%
  ggplot(aes(contribution, word2, fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ word1, scales = "free", nrow = 3) +
  scale_y_reordered() +
  labs(x = "Sentiment value * # of occurrences",
       y = "Words preceded by a negation")

# si nota che fonte di incertezza sembrano soprattutto not funny, not entertaining, not racist, 
# without success, as hell, in tears, tears in generale è in questo contesto sempre molto positiva; inoltre fucking/fuck non sono per forza negativi in questo contesto
# è probabilmente meglio non considerarli

view(get_sentiments("afinn") %>% filter(value == -4)) # fraud = -4 al posto di not funny(bilanciamo funny pari a 4)
get_sentiments("afinn") %>% filter(word == "entertaining")
view(get_sentiments("afinn") %>% filter(value == -2)) # awkWard = -2 al posto di not entertaining(bilanciamo entertaining pari a 2)
get_sentiments("afinn") %>% filter(word == "racist")
view(get_sentiments("afinn") %>% filter(value == +3)) # audacious = 3 al posto di not racist(bilanciamo racist pari a -3)
get_sentiments("afinn") %>% filter(word == "success")
view(get_sentiments("afinn") %>% filter(value == -2)) # awkard = -2 al posto di without success(bilanciamo success pari a 2)
get_sentiments("afinn") %>% filter(word == "hell")
view(get_sentiments("afinn") %>% filter(value == 4)) # funny = 4 al posto di as hell(bilanciamo hell pari a -4)
get_sentiments("afinn") %>% filter(word == "tears")
view(get_sentiments("afinn") %>% filter(value == 2)) # amaze = 2 al posto di in tears(bilanciamo tears pari a -2)

#applichiamo correzioni

data_c <- data %>% 
  mutate(tweet = sapply(tweet, FUN = gsub, pattern = "not funny", replace = "fraud")) %>%
  mutate(tweet = sapply(tweet, FUN = gsub, pattern = "not entertaining", replace = "awkWard")) %>%
  mutate(tweet = sapply(tweet, FUN = gsub, pattern = "not racist", replace = "audacious")) %>%
  mutate(tweet = sapply(tweet, FUN = gsub, pattern = "without success", replace = "awkard")) %>%
  mutate(tweet = sapply(tweet, FUN = gsub, pattern = "as hell", replace = "funny")) %>%
  mutate(tweet = sapply(tweet, FUN = gsub, pattern = "tears", replace = "amaze"))

#rifacciamo analisi dopo tutte le correzioni applicate

tidydata_c <- data_c %>%
  unnest_tokens(word, tweet)

tidydata_c <- tidydata_c %>% anti_join(customS)

# parole più usate per show
words_by_show_c <- tidydata_c %>%
  count(show, word, sort=TRUE) %>%
  ungroup

# sentiment value dei 6 spettacoli
show_sentiments_c <- words_by_show_c %>%
  inner_join(get_sentiments("afinn"), by= "word") %>%
  group_by(show) %>%
  summarize(value = sum(value * n) / sum(n))

# grafico parole che contribuiscono di più per spettacolo
show_sentiments_c %>%
  mutate(volgarity = c(0.0888, 0.0497, 0.0553, 0.0006, 0.0025, 0.0013)) %>%
  mutate(show = reorder(show, value)) %>%
  ggplot(aes(value, show, fill = volgarity)) +
  geom_col(show.legend = FALSE) +
  geom_label(aes(label = volgarity), col = "white", nudge_x = -0.1) +
  labs(x = "Average sentiment value", y = NULL)

### si rifanno analisi su contributo singole parole solo per verificare che le correzioni siano
### state correttamente effetuate

contributions_c <- tidydata_c %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(word) %>%
  summarize(occurences = n(),
            contribution = sum(value))

contributions_c %>%
  slice_max(abs(contribution), n = 25) %>%
  mutate(word = reorder(word, contribution)) %>%
  ggplot(aes(contribution, word, fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  labs(y = NULL)

top_sentiment_words_c <- words_by_show_c %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  mutate(contribution = value * n / sum(n))

view(top_sentiment_words_c)

top_sentiment_words_c %>%
  group_by(show) %>%
  slice_max(abs(contribution), n = 12) %>%
  ungroup() %>%
  mutate(show = reorder(show, contribution),
         word = reorder_within(word, contribution, show)) %>%
  ggplot(aes(contribution, word, fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  facet_wrap(~ show, scales = "free") +
  labs(x = "Sentiment value * # of occurrences", y = NULL)






### TOPIC MODELING

library(TextWiller)
library(lda)
library(data.table)
library(stringr)
library(topicmodels)

standup$testo <- as.character(standup$testo)
standup.topic.modeling <- standup

# Si tengono in considerazione i bigrammi maggiormente presenti nei testi e si eliminano le stop words
#che non sono state eliminate nell'analisi esplorativa in quanto, essendo poco frequenti, non sono
#state viste.
standup.topic.modeling <- standup.topic.modeling %>% 
  mutate(testo = sapply(testo, FUN = gsub, pattern = "aint", replace = "")) %>% 
  mutate(testo = sapply(testo, FUN = gsub, pattern = "ks", replace = "")) %>% 
  mutate(testo = sapply(testo, FUN = gsub, pattern = "kids", replace = "children")) %>% 
  mutate(testo = sapply(testo, FUN = gsub, pattern = "yall", replace = "")) %>% 
  mutate(testo = sapply(testo, FUN = gsub, pattern = "im", replace = "")) %>%
  mutate(testo = sapply(testo, FUN = gsub, pattern = "imitates", replace = "")) %>%
  mutate(testo = sapply(testo, FUN = gsub, pattern = "whats", replace = "")) %>%
  mutate(testo = sapply(testo, FUN = gsub, pattern = "isnt", replace = "")) %>%
  mutate(testo = sapply(testo, FUN = gsub, pattern = "didnt", replace = "")) %>%
  mutate(testo = sapply(testo, FUN = gsub, pattern = "dont", replace = "")) %>%
  mutate(testo = sapply(testo, FUN = gsub, pattern = "id", replace = "")) %>%
  mutate(testo = sapply(testo, FUN = gsub, pattern = "lets", replace = "")) %>%
  mutate(testo = sapply(testo, FUN = gsub, pattern = "youre", replace = "")) %>%
  mutate(testo = sapply(testo, FUN = gsub, pattern = "youve", replace = "")) %>%
  mutate(testo = sapply(testo, FUN = gsub, pattern = "wasnt", replace = "")) %>%
  mutate(testo = sapply(testo, FUN = gsub, pattern = "applauding", replace = "")) %>%
  mutate(testo = sapply(testo, FUN = gsub, pattern = "applaud", replace = "")) %>%
  mutate(testo = sapply(testo, FUN = gsub, pattern = "applauds", replace = "")) %>%
  mutate(testo = sapply(testo, FUN = gsub, pattern = "white people", replace = "whitepeople")) %>%
  mutate(testo = sapply(testo, FUN = gsub, pattern = "black people", replace = "blackpeople")) %>%
  mutate(testo = sapply(testo, FUN = gsub, pattern = "gay people", replace = "gaypeople")) %>%
  mutate(testo = sapply(testo, FUN = gsub, pattern = "asian people", replace = "asianpeople")) %>%
  mutate(testo = sapply(testo, FUN = gsub, pattern = "god damn", replace = "goddamn")) %>%
  mutate(testo = sapply(testo, FUN = gsub, pattern = "ice cream", replace = "icecream")) %>%
  mutate(testo = sapply(testo, FUN = gsub, pattern = "donald trump", replace = "trump")) %>%
  mutate(testo = sapply(testo, FUN = gsub, pattern = "donald trumps", replace = "trump")) %>%
  mutate(testo = sapply(testo, FUN = gsub, pattern = "president trump", replace = "trump")) %>%
  mutate(testo = sapply(testo, FUN = gsub, pattern = "jesus christ", replace = "jesuschrist")) %>%
  mutate(testo = sapply(testo, FUN = gsub, pattern = "music playing", replace = "")) %>%
  mutate(testo = sapply(testo, FUN = gsub, pattern = "normal voice", replace = "")) %>%
  mutate(testo = sapply(testo, FUN = gsub, pattern = "black guy", replace = "blackguy")) %>%
  mutate(testo = sapply(testo, FUN = gsub, pattern = "black guys", replace = "blackguys")) %>%
  mutate(testo = sapply(testo, FUN = gsub, pattern = "los angeles", replace = "losangeles")) %>%
  mutate(testo = sapply(testo, FUN = gsub, pattern = "york city", replace = "york")) %>%
  mutate(testo = sapply(testo, FUN = gsub, pattern = "san francisco", replace = "sanfrancisco")) %>%
  mutate(testo = sapply(testo, FUN = gsub, pattern = "feel bad", replace = "feelbad")) 

standup.topic.modeling <- standup.topic.modeling %>% 
  unnest_tokens("word", testo)

# Si Tolgono le stop words

standup.topic.modeling <- standup.topic.modeling %>% anti_join(custom)

# Stemming
standup.topic.modeling <- standup.topic.modeling %>%
  mutate_at("word", funs(wordStem((.), language="en")))

# Frequenza delle parole in ogni stand-up comedy
word_countsLDA <- standup.topic.modeling %>%
  count(titolo,word, sort = T) %>%
  ungroup()

# Document Term Matrix
standup_dtm <- word_countsLDA %>%
  cast_dtm(titolo, word, n)

# LDA con k=7 cluster.
standup_lda <- LDA(standup_dtm, k = 7, control = list(seed = 1234), method = "Gibbs")
standup_lda

standup_topics <- tidy(standup_lda, matrix = "beta")
standup_topics

top_terms <- standup_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 8) %>% 
  ungroup() %>%
  arrange(topic, -beta)
top_terms


top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

#NB: il risultato a cui si giunge nel topic modeling può variare in base alla versione di R
#utilizzata. Pertanto, si è allegato il file pdf contenente il grafico dei principali termini di
#ciascun topic che hanno permesso di definire i seguenti 7 topic.

# L'analisi a posteriori ci porta a definire i seguenti 7 topic
topic.names <- c( "Quotidianità","Esperienze di vita", "Famiglia", 
                  "Miscellanea", "Slang", "Relazioni sentimentali", 
                  "America")

# Calcoliamo, per ogni stand-up comedy, le proporzioni dei 7 topic
standup_gamma <- tidy(standup_lda, matrix = "gamma")
standup_gamma
numerazione <- rep(1:228,7)
standup_gamma <- cbind(standup_gamma, numerazione)
standup_gamma <- standup_gamma[order(standup_gamma$numerazione),]

documenti <- c("END TIMES FUN", "STAY HUNGRY",
               "DOUGLAS", "STICKS AND STONES",
               "ASIAN MOMMA, MEXICAN KIDS", 
               "THE BEST OF ULTIMATE GOLD GREATEST HITS")
topic.proportions <- matrix(rep(rep(NA,6),7), ncol = 6)
for (i in 1:6) {
  topic.proportions[,i] <- standup_gamma[which(standup_gamma$document == documenti[i]),]$gamma
}
topic.proportions <- t(topic.proportions)
colnames(topic.proportions) <- topic.names
documenti.min <- c("End Times Fun", "Stay Hungry", "Douglas","Sticks And Stones",
                   "Asian Momma, Mexican Kids", "The Best Of Ultimate Gold Greatest Hits")
topic.proportions.df <- data.table::melt(
  cbind(data.frame(topic.proportions),
        document= documenti.min),
  variable.name ="Topic",
  id.vars = "document")

ggplot(topic.proportions.df, aes(y = Topic, x = value, fill = factor(Topic)), ylab ="proportion") +
  geom_col(show.legend = FALSE) +
  theme(legend.position = "none")+
  geom_bar(stat = "identity")+
  facet_wrap(~ document)

