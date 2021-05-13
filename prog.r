source("librerie.r")
source("Normalizzare.r")
source("UtilityFun.r")

#Caricamento dei dati ed analisi esplorativa -------------------------------------------

SMSSpamCollection <- read.delim("smsspamcollection/SMSSpamCollection.txt", 
                                header=FALSE, quote = "")
SMS.text <- tibble(text = normalizzare(SMSSpamCollection$V2, stopwords = stopwords("en")),
                   Spam = SMSSpamCollection$V1 %>% as.factor(),
                   id = 1:nrow(SMSSpamCollection))

SMS.text <- SMS.text %>%
  mutate(text = sapply(text, FUN = gsub, pattern = "\\b+ll+\\b", replacement = " will "))

table(SMSSpamCollection$V1)/nrow(SMSSpamCollection)
#Il dataset ha una prevalenza di messaggi di ham, circa l'87% dell'intero dataset

sms_words<-SMS.text %>% unnest_tokens(word,text) %>% count(Spam,word,sort=TRUE) %>% ungroup()


# Wordcloud ---------------------------------------------------------------

sms_words %>% filter( n > 150) %>%
  mutate(word = reorder(word,n)) %>% 
  ggplot(aes(word,n)) +geom_col() +coord_flip()

freq.spam<- sms_words %>% filter(Spam == "spam") %>% filter(n>10) %>% print()
freq.ham<- sms_words %>% filter(Spam == "ham") %>% filter(n>30) %>% print()

wordcloud2(freq.ham %>% select(word,n))
wordcloud2(freq.spam %>% select(word,n))


# Bigrammi Trigrammi e visualizzazione ------------------------------------

#Bigrammi per i messaggi spam :
conta_bigrammi(SMS.text %>% filter(Spam == "spam"))
#Bigrammi per i messaggi ham: 
conta_bigrammi(SMS.text %>% filter(Spam == "ham"))

#Trigrammi per i messaggi spam :
conta_trigrammi(SMS.text %>% filter(Spam == "spam"))
#Trigrammi per i messaggi ham : 
conta_trigrammi(SMS.text %>% filter(Spam == "ham"))

#Visualizzazione dei bigrammi e trigrammi :

visualizza_bigrammi(conta_bigrammi(SMS.text %>% filter(Spam == "ham")))
visualizza_bigrammi(conta_bigrammi(SMS.text %>% filter(Spam == "spam")))

visualizza_trigrammi(conta_trigrammi(SMS.text %>% filter(Spam == "ham")))
visualizza_trigrammi(conta_trigrammi(SMS.text %>% filter(Spam == "spam")))


# Analisi tf-idf ----------------------------------------------------------

total_words<- sms_words %>% group_by(Spam) %>% summarize(total = sum(n))
freq_words <- sms_words$n/sms_words$total
sms_words$freq <- freq_words



ggplot(sms_words,aes(freq,fill = Spam)) +geom_histogram(show.legend = T) +xlim(NA,0.0009)+
  facet_wrap(~Spam,scales = "free_y")

#Plot degli spam
ggplot(subset(sms_words, freq > 0.005 & Spam == "spam", select = c(word,freq, Spam)), aes(x=word, y=freq, fill = Spam)) +
  geom_bar(stat="identity") + facet_wrap(~Spam)

#plot degli ham
ggplot(subset(sms_words, freq > 0.005 & Spam == "ham", select = c(word,freq, Spam)), aes(x=word, y=freq, fill = Spam)) +
  geom_bar(stat="identity", fill = "blue")


































#FAsE DI STEMMING NON RILEVANTE PER LA COSTRUZIONE DEL MODELLO PER ANALIZZARE I DATI
#Stemming 

stemmed.text <- sms.text$text %>% 
  text_tokens(.,stemmer = "eng") %>% lapply(FUN = paste, collapse = " ") %>% unlist

tidy.sms2 <- tibble(text = stemmed.text, type = sms.text$type) %>%
  unnest_tokens(word, text)

tidy.sms2 %>% count(as.character(word), sort = T)

wordcloud(tidy.sms2 %>%count(word) %>% pull(word),tidy.sms2 %>%
            count(word) %>%pull(n), min.freq = 100)



####################################################################################################################
#Versione Tidy delle analisi fatte.


# Analisi tf-idf ----------------------------------------------------------


total_words<- sms_words %>% group_by(Spam) %>% summarize(total = sum(n))

sms_words

head(sms_words,15)


# Analisi tf-idf :
# L'idea della tf-idf è quella di trovare le parole importanti di un documento danno peso 
# decrescente per le parole più comunemente usate e peso crescente per le parole 
# che non sono molto utilizzate in una collezione di file di testo.
# tf sta per "term frequency", mentre idf sta per "inverse-document-frequency" 
# ques'ultime due vengono moltiplicate assieme nella statistica tf-idf.
# l'idf in particolare è definita come :
# idf(parola) = log(n_testi/n_testicontenenti_parola)

sms_words<-sms_words %>% bind_tf_idf(word,Spam,n)
sms_words
# Se idf è quasi zero allo td-idf è a sua volta pari a zero ,
# ed il valore della statistica indica dunque che quella particolare
# parola è tra le parole più comuni.

# I termini con tf-idf più elevati tra tutti gli elementi di testo sono
# i seguenti :

sms_words %>% select(-total) %>% arrange(desc(tf_idf))

#Parole con tf_idf più elevato :
sms_words %>% 
  arrange(desc(tf_idf)) %>% 
  mutate(word = factor(word,levels = rev(unique(word))))%>%
  group_by(Spam)%>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word,tf_idf,fill = Spam))+
  geom_col(show.legend = FALSE)+
  labs(x = NULL,y = "tf-idf")+
  facet_wrap(~Spam,ncol = 2,scales = "free")+
  coord_flip()









