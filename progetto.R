token="EAACEdEose0cBAFthyZAIktyJNe48jFaF4KqKATULU0Lv3bZBhNfeo1k3nUCsRwhZCzG4Tlh3dZAZB3qSZCDxvk7S6ZAJu238O3RTvHnpThtTfqFw4oFKF2DPW7Ox8MvdftqrCDLembwnwHIy7CGYlTZCJQ2x4FfzMb8OyZA25Pgqwbl2xbbQYOa0b"
library(Rfacebook)
library(stringr)
library(tidytext)
library(ggplot2)
library(dplyr)
library(tm)
library(topicmodels)
library(tidyr)
library(wordcloud)

# function to extract facebook posts
estrai.post=function(politici) {
  result=rep(NA,length(politici))
  for (i in 1:length(politici)) {
    post=getPage(politici[i],token,since="2017/01/01",until="2017/05/01",n=3000)
    result[i]=str_replace_all(paste(post$message, sep=" ",collapse = " "),"[^[:graph:]]", " ") 
  }
  result
}

# 5 different political parties
m5s=c("beppegrillo.it","dibattista.alessandro","LuigiDiMaio","Paola.Taverna.M5S","roberto.fico.5",
      "nicola.morra.63","vitoclaudiocrimi","CarloMartelliM5sSenato","M5Scarlaruocco","BarbaraLezziPagina")
pd=c("matteorenziufficiale","boschimariaelena","monicacirinna.it","andreaorlandosp","paginaDarioFranceschini",
     "giannicuperlo","orfinimatteo","maumartina","szampa56","PietroGrasso")
lega=c("salviniofficial","massimilianofedriga","businfilippo","caparini","RobertoCalderoli",
       "deputatonicolamolteni","MarioBorghezioLegaNord","grimoldi.paolo","AngeloCioccaOfficial","barbara.saltamartini")
fi=c("SilvioBerlusconi","michelavittoriabrambilla","CarfagnaMara","MariastellaGelmini","renato.brunetta",
     "gasparri.it","nunziadegirolamo.ufficiale","simonebaldelli.it","deborah.bergamini","gabriella.giammanco")
altri=c("giorgiameloni.paginaufficiale","PierLuigiBersani.PaginaUfficiale","giuseppecivati","angelinoalfano.it","giulianopisapia1")

# each vector contains the posts of all the members of each party
m5s_corp=estrai.post(m5s)
pd_corp=estrai.post(pd)
lega_corp=estrai.post(lega)
fi_corp=estrai.post(fi)
altri_corp=estrai.post(altri)

################################
######## PRE-PROCESSING ########
################################

########################
######## STEP 1 ########
########################

# delete NA
m5s_corp=str_replace_all(m5s_corp,"NA",'')
pd_corp=str_replace_all(pd_corp,"NA",'')
lega_corp=str_replace_all(lega_corp,"NA",'')
fi_corp=str_replace_all(fi_corp,"NA",'')
altri_corp=str_replace_all(altri_corp,"NA",'')

################################
# the previous commands have been already executed on the Rdata file
###############################


# extract hashtags
hashtag_m5s=str_extract_all(m5s_corp, "#\\w+")
hashtag_pd=str_extract_all(pd_corp, "#\\w+")
hashtag_lega=str_extract_all(lega_corp, "#\\w+")
hashtah_fi=str_extract_all(fi_corp, "#\\w+")
hashtah_altri=str_extract_all(altri_corp, "#\\w+")

# delete hashtags
m5s_corp<-str_replace_all(m5s_corp, "#\\w+", '')
pd_corp<-str_replace_all(pd_corp, "#\\w+", '')
lega_corp<-str_replace_all(lega_corp, "#\\w+", '')
fi_corp<-str_replace_all(fi_corp, "#\\w+", '')
altri_corp<-str_replace_all(altri_corp, "#\\w+", '')

# assign names to documents
names(m5s_corp)=m5s
names(pd_corp)=pd
names(lega_corp)=lega
names(fi_corp)=fi
names(altri_corp)=altri

library(tm)

# to useful files for italians stopwords and proper nouns in italian language
load("itastopwords.rda")
load("vocabolarioNomiPropri.rda")

# unify in a single corpus
corp <- Corpus(VectorSource(c(m5s_corp,pd_corp,lega_corp,fi_corp,altri_corp)))

# all characters to lower-case
corp1 <- tm_map(corp, tolower)

# remove stopwords from "itastopwords.rda" file
corp1 <- tm_map(corp1, removeWords, itastopwords)

# remove default R stopwords for italian language
corp1 <- tm_map(corp1, removeWords, stopwords("italian"))

# remove proper nouns
corp1 <- tm_map(corp1, removeWords, row.names(vocabolarioNomiPropri))

# remove numbers
corp1 <- tm_map(corp1, removeNumbers)

# remove punctuation
corp1 <- tm_map(corp1, removePunctuation)

# remove all the punctuation which is not removed by removePunctuation
corp1 <- tm_map(corp1, removeWords,c("d'","l'","un'"))

# delete white spaces which originate from the removed strings
corp1 <- tm_map(corp1, stripWhitespace)

# remove political parties' names and other Web chars (http,www,ecc)
corp1 <- tm_map(corp1,removeWords,c("movimento stelle","forza italia","lega nord","partito democratico","fratelli ditalia","fratelli italia","ms","pd","http","https","www"))

# remove words which are useless in the bigrams
corp1 <- tm_map(corp1,removeWords,c("devono", "essere", "u", "fffd", "ancora", "volta", "tre", "due", "anni", "dopo", "aver","ultimi", "vuol","dire", "dovrebbe","qualche","giorno", "p", "vista", "punto", "n","mesi", "pochi", "migliaia", "milioni","piazza", "troppo", "tempo","streaming","stato","fatto","fare", "fra","poco","detto"))

# Document Term Matrix 1
dtm1=DocumentTermMatrix(corp1)
inspect(dtm1)
# 33925 (distinct) words at the STEP 1 of pre-processing

########################
######## STEP 2 ########
########################

# find bigrams
testo_unico<-as.character(corp1)[1]
bigrams<-textcnt(testo_unico, n=2L, method="string")
bigrams_freq<-bigrams[bigrams>30]
bigrams_freq

# useful functions

trasformaBigrammi<-function(testo, bigrammi)
{
  for (i in 1:length(bigrammi))
  {
    bigramma_originale<-names(bigrammi[i])
    bigramma_trattino<-paste(strsplit(bigramma_originale, " ")[[1]], collapse = "")
    testo<-gsub(bigramma_originale, bigramma_trattino, testo)
  }
  testo
}

pulisciCorpus<-function(corpus_politico)
{
  testo_pulito<-as.character(corpus_politico)[1]
  testo_pulito<-trasformaBigrammi(testo_pulito, bigrams_freq)
  testo_pulito
}

# add bigrams to every corpus
m5s_1<-pulisciCorpus(corp1[1])
m5s_2<-pulisciCorpus(corp1[2])
m5s_3<-pulisciCorpus(corp1[3])
m5s_4<-pulisciCorpus(corp1[4])
m5s_5<-pulisciCorpus(corp1[5])
m5s_6<-pulisciCorpus(corp1[6])
m5s_7<-pulisciCorpus(corp1[7])
m5s_8<-pulisciCorpus(corp1[8])
m5s_9<-pulisciCorpus(corp1[9])
m5s_10<-pulisciCorpus(corp1[10])
pd_1<-pulisciCorpus(corp1[11])
pd_2<-pulisciCorpus(corp1[12])
pd_3<-pulisciCorpus(corp1[13])
pd_4<-pulisciCorpus(corp1[14])
pd_5<-pulisciCorpus(corp1[15])
pd_6<-pulisciCorpus(corp1[16])
pd_7<-pulisciCorpus(corp1[17])
pd_8<-pulisciCorpus(corp1[18])
pd_9<-pulisciCorpus(corp1[19])
pd_10<-pulisciCorpus(corp1[20])
lega_1<-pulisciCorpus(corp1[21])
lega_2<-pulisciCorpus(corp1[22])
lega_3<-pulisciCorpus(corp1[23])
lega_4<-pulisciCorpus(corp1[24])
lega_5<-pulisciCorpus(corp1[25])
lega_6<-pulisciCorpus(corp1[26])
lega_7<-pulisciCorpus(corp1[27])
lega_8<-pulisciCorpus(corp1[28])
lega_9<-pulisciCorpus(corp1[29])
lega_10<-pulisciCorpus(corp1[30])
fi_1<-pulisciCorpus(corp1[31])
fi_2<-pulisciCorpus(corp1[32])
fi_3<-pulisciCorpus(corp1[33])
fi_4<-pulisciCorpus(corp1[34])
fi_5<-pulisciCorpus(corp1[35])
fi_6<-pulisciCorpus(corp1[36])
fi_7<-pulisciCorpus(corp1[37])
fi_8<-pulisciCorpus(corp1[38])
fi_9<-pulisciCorpus(corp1[39])
fi_10<-pulisciCorpus(corp1[40])
altri_1<-pulisciCorpus(corp1[41])
altri_2<-pulisciCorpus(corp1[42])
altri_3<-pulisciCorpus(corp1[43])
altri_4<-pulisciCorpus(corp1[44])
altri_5<-pulisciCorpus(corp1[45])

# group documents
m5s_corp<-c(m5s_1,m5s_2,m5s_3,m5s_4,m5s_5,m5s_6, m5s_7, m5s_8, m5s_9, m5s_10)
pd_corp<-c(pd_1,pd_2,pd_3,pd_4,pd_5,pd_6,pd_7,pd_8, pd_9,pd_10)
lega_corp<-c(lega_1,lega_2,lega_3,lega_4,lega_5,lega_6,lega_7,lega_8,lega_9,lega_10)
fi_corp<-c(fi_1,fi_2,fi_3,fi_4,fi_5,fi_6,fi_7,fi_8,fi_9,fi_10)
altri_corp<-c(altri_1,altri_2,altri_3,altri_4, altri_5)

# assign a name to documents
m5s<-c("Grillo","Di Battista", "Di Maio", "Taverna", "Fico", "Morra", "Crimi", "Martelli", "Ruocco", "Lezzi")
pd<-c("Renzi", "Boschi", "Cirinna", "Orlando", "Franceschini", "Cuperlo", "Orfini", "Martina", "Zampa","Grasso")
lega<-c("Salvini", "Fedriga","Busin","Caparini","Calderoli","Moleni","Boreghezio","Girmoldi","Ciocca","Saltamarini")
fi<-c("Berlusconi", "Brambilla", "Carfagna","Gelmini","Brunetta","Gasparri","De Girolamo","Baldelli","Bergamini","Giammanco")
altri<-c("Meloni","Bersani","Civati","Alfano","Pisapia")

names(m5s_corp)=m5s
names(pd_corp)=pd
names(lega_corp)=lega
names(fi_corp)=fi
names(altri_corp)=altri

# corp2 has also the bigrams
corp2 <- Corpus(VectorSource(c(m5s_corp,pd_corp,lega_corp,fi_corp,altri_corp)))

# Document Term Matrix 2
dtm2=DocumentTermMatrix(corp2,control = list(minWordLength=2))
inspect(dtm2)
# 34051 (distinct) words at the STEP 2 of pre-processing
# 126 more than STEP 1

########################
######## STEP 3 ########
########################

library(tm)
library(topicmodels)

# remove sparse terms

dtm2.sparse<-removeSparseTerms(dtm2, 1-(10/45))
inspect(dtm2.sparse)

# UPPER-BOUND: remove the most frequent words (i.e. those whose percentile is greater than 0.975)
mat_dtm<-as.matrix(dtm2.sparse)
freq_parole<-colSums(mat_dtm)
soglia=quantile(freq_parole, prob=0.975)
soglia

hist(freq_parole, breaks = 10000)

freq_parole[order(freq_parole, decreasing = T)][1:100]
troppo_freq<-names(freq_parole[freq_parole>soglia])
troppo_freq

# remove words contained in troppo_freq from corp2
corp3 <- tm_map(corp2,removeWords,troppo_freq)

# Document Term Matrix 3
dtm3 <- DocumentTermMatrix(corp3,control = list(minWordLength=2))
inspect(dtm3)

# LOWER-BOUND: remove rare words (i.e. those who appear in less than 10 documents)
dtm3<-removeSparseTerms(dtm3, 1-(10/45))
inspect(dtm3)
# 2814 (distinct) words at the end of the STEP 3 of pre-processing
# 72 removed from UPPER-BOUND
# 31163 removed from LOWER-BOUND

## the final corpus has 2814 words
dtm2list <- apply(dtm3, 1, function(x) {
  paste(rep(names(x), x), collapse=" ")
})

corp3 <- Corpus(VectorSource(dtm2list))
inspect(corp3)
inspect(DocumentTermMatrix(corp3,control=list(minWordLength=2)))

################################
########## LDA MODEL ########### 
################################

library(tidytext)
library(ggplot2)
library(dplyr)

# for the sake of simplicity
corp.temp=corp3
dtm=dtm3

# useful functions

stampa.risultati=function(dtm_lda) {
  
  dtm_topics <- tidy(dtm_lda, matrix = "beta")
  dtm_topics
  
  dtm_top_terms <- dtm_topics %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
  
  dtm_top_terms %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free",ncol=3) +
    theme(text = element_text(size=20)) +
    coord_flip()
}

aggiorna.risultati=function(parole,k,aumenta=F) {
  if (aumenta==F) {
    corp.temp <<- tm_map(corp.temp,removeWords,parole)
  }
  dtm.temp <- DocumentTermMatrix(corp.temp,control = list(minWordLength=2))
  dtm.temp<- removeSparseTerms(dtm.temp, 1-(10/45))
  
  dtm_lda.temp=LDA(dtm.temp,k,control = list(seed = 1234))
  dtm_lda.temp
}

top_words_politici=function(partito) {
  for (i in 1:length(partito)) {
    tidy(dtm3) %>%
      filter(document == partito[i]) %>%
      arrange(desc(count))
  }
}

dtm_lda_walter=aggiorna.risultati(c("secondo","dice","settimana","fino","avanti","dobbiamo","adesso","nuovo","viene","avere","caso", "aspetto","sabato","quel","venite","fratellid","forzaitalia","vogliamo","quel","sotto","diciamo", "dobbiamo","nuova","importante","soprattutto","quindi","alcuni","quasi","fonte","sera","amici","vogliono","tanto","possiamo","tante","merito","nessuno","stesso","subito","buona","presentato","infatti","primo","circa","stesso","possono","grandi","dichiara","intervista","bisogno","senso","vengono","qualcuno","progetto","chiede","tema","intervento","vediamo","sfida","tornare","proposta","domenica","forse","molti","davvero","tutta","partire","nuove","risposta","post","questione","nulla","fine","mettere","voce","parlare","propria","vero","chiedere","chiediamo","massima","italiana","minori","italiano","pensare","ospite","senatore","ovvero","posto","poter","altra","persona","battaglia","prodotti","spero","ci?","mila","ovvero","napoli","serve","rai","giornata","verona","idee","orlando","berlusconi","gennaio","ospite","pubblica","dato","verit?","maio", "raggi", "leggi", "attivit?", "centrodestra", "storia", "venerd?", "responsabilit?", "interrogazione","marzo", "gruppo","favore", "rete", "programma", "comunicato", "oppure", "cio?","almeno"),k=7)
dtm_lda_walter2=aggiorna.risultati(c("miliardieuro"),k=7)
stampa.risultati(dtm_lda_walter)

# Select model (starting with k=10)

# 1)
dtm_lda1=LDA(dtm,k=2,control = list(seed = 1234))
stampa.risultati(dtm_lda1)

# 2)
dtm_lda2=aggiorna.risultati(c("secondo","dice","settimana",
                              "fino","avanti","dobbiamo","adesso",
                              "nuovo","viene","avere","caso",
                              "aspetto","sabato","quel",
                              "venite","fratellid","forzaitalia",
                              "vogliamo","quel","sotto","diciamo",
                              "dobbiamo","nuova","importante","soprattutto",
                              "quindi"),k=2)
stampa.risultati(dtm_lda2)

# 3)
dtm_lda3=aggiorna.risultati(c("alcuni","quasi","fonte","sera","amici",
                              "vogliono","tanto","possiamo"),k=2)
stampa.risultati(dtm_lda3)

# 4)
dtm_lda4=aggiorna.risultati(c("tante","merito","nessuno","stesso","subito",
                              "buona","presentato","infatti","primo","circa",
                              "stesso","possono","grandi"),k=2)
stampa.risultati(dtm_lda4)

# 5)
dtm_lda5=aggiorna.risultati(c("dichiara","intervista","bisogno","senso",
                              "vengono","qualcuno","progetto"),k=2)
stampa.risultati(dtm_lda5)

# 6)
dtm_lda6=aggiorna.risultati(c("chiede","tema","intervento","vediamo",
                              "sfida","tornare","proposta","domenica"),k=2)
stampa.risultati(dtm_lda6)

# 7)
dtm_lda7=aggiorna.risultati(c("forse","molti","davvero","tutta","partire"),k=2)
stampa.risultati(dtm_lda7)

# 8)
dtm_lda8=aggiorna.risultati(c("nuove","risposta","post","questione",
                              "nulla","fine","mettere","voce","parlare"),k=2)
stampa.risultati(dtm_lda8)

# 9)
dtm_lda9=aggiorna.risultati(c("propria","vero"),k=2)
stampa.risultati(dtm_lda9)


# 10)
dtm_lda11=aggiorna.risultati(c("chiedere","chiediamo","massima","italiana","minori"),k=2)
stampa.risultati(dtm_lda11)

# 11)
dtm_lda12=aggiorna.risultati(c("italiano","pensare"),k=2)
stampa.risultati(dtm_lda12)

# 12)
dtm_lda13=aggiorna.risultati(c("ospite","senatore","ovvero"),k=2)
stampa.risultati(dtm_lda13)

# 13)
dtm_lda14=aggiorna.risultati(c("posto","poter","altra","persona","battaglia","prodotti","spero"),k=2)
stampa.risultati(dtm_lda14)

#14 
dtm_lda14=aggiorna.risultati(c("ci?","mila","ovvero","napoli","serve","rai","giornata","verona","idee","orlando","berlusconi","gennaio","ospite","pubblica","dato","verit?"),k=7)
stampa.risultati(dtm_lda14)

#15 
dtm_lda14=aggiorna.risultati(c( "leggi", "attivit?", "centrodestra", "storia", "venerd?", "maio", "raggi","responsabilit?", "interrogazione","marzo", "gruppo"),k=7)
stampa.risultati(dtm_lda14)

#16 
dtm_lda14=aggiorna.risultati(c("favore", "rete", "programma", "comunicato", "oppure", "cio?","almeno"),k=7)
stampa.risultati(dtm_lda14)


###########

# try with k=7
dtm_lda_7=aggiorna.risultati(c("grillo"),k=7)

stampa.risultati(dtm_lda_7)





###################################
# choose the model dtm_lda_7
####################################

dtm_lda_finale<-dtm_lda_7


# the function to compare topics
# parameters:
# - model
# - topics to be compared (between 1 and 7)
# - thresholds (lower-bound) for beta parameter
# - titles to be printed in the final plot
confrontaTopic<-function(modello, n1, n2, soglia1=0.001,soglia2=0.001, titolo1=n1, titolo2=n2)
{
  ap_topic<-tidy(modello, matrix="beta")
  beta_primotp<-ap_topic[ap_topic$topic==n1,"beta"]
  beta_secondotp<-ap_topic[ap_topic$topic==n2,c("beta","term")]
  beta<-cbind(beta_primotp,beta_secondotp)
  beta<-beta[beta[,1]>soglia1 | beta[,2]>soglia2,]
  log_ratio<-log2(beta[,1]/beta[,2])
  names(log_ratio)<-beta$term
  term_primotp<-log_ratio[order(log_ratio,decreasing=T)][1:15]
  term_secondotp<-log_ratio[order(log_ratio,decreasing=F)][1:15]*-1
  par(mfrow=c(1,2))
  wordcloud(names(term_primotp), freq=term_primotp,random.order = F, min.freq = 0, col="blue",scale=c(1.8,0.6))
  title(main=titolo1)
  wordcloud(names(term_secondotp), freq=term_secondotp,random.order = F, min.freq = 0, col="red",scale=c(1.8,0.6))
  title(main=titolo2)
  par(mfrow=c(1,1))
  
  log_ratio
}

confrontaTopic(dtm_lda_finale,4,2,0.0019,0.0089, "Topic 4", "Topic 2")
confrontaTopic(dtm_lda_finale,1,6,0.002,0.0018, "Topic 1", "Topic 6")
confrontaTopic(dtm_lda_finale,3,5,0.0013,0.0018, "Topic 3", "Topic 5")
confrontaTopic(dtm_lda_finale,1,7,0.0019,0.0021, "Topic 1", "Topic 7")

##################
# distributions of topics among politicians
#################

gamma <- tidy(dtm_lda_finale, matrix = "gamma")
gamma_mat=as.data.frame(gamma)

m5s<-c("Grillo","Di Battista", "Di Maio", "Taverna", "Fico", "Morra", "Crimi", "Martelli", "Ruocco", "Lezzi")
pd<-c("Renzi", "Boschi", "Cirinna", "Orlando", "Franceschini", "Cuperlo", "Orfini", "Martina", "Zampa","Grasso")
lega<-c("Salvini", "Fedriga","Busin","Caparini","Calderoli","Moleni","Boreghezio","Girmoldi","Ciocca","Saltamarini")
fi<-c("Berlusconi", "Brambilla", "Carfagna","Gelmini","Brunetta","Gasparri","De Girolamo","Baldelli","Bergamini","Giammanco")
altri<-c("Meloni","Bersani","Civati","Alfano","Pisapia")


documenti=c(m5s,pd,lega,fi,altri)

gamma_mat[match(rep(documenti,each=5),gamma_mat$document),]

df <- data.frame(matrix(ncol = 3, nrow = 315))
colnames(df) <- c("Politico","Topic","Gamma")

start=0
for (i in 1:45) {
  skip=0
  for (j in 1:7) {
    df[j+start,]=gamma_mat[i+skip,]
    skip=skip+45
  }
  start=start+7
}
df$Partito=c(rep(c("M5S","PD","LEGA","FI"),each=70),rep("ALTRI",35))
df$Politico=rep(c(m5s,pd,lega,fi,altri),each=7)
df$Topic=rep(mieitopic,45)

df$Partito=factor(df$Partito,levels=c("M5S","PD","LEGA","FI","ALTRI"))
df$Politico=factor(df$Politico,levels = c(m5s,pd,lega,fi,altri))
str(df)


# Stacked Percent
ggplot(df, aes(x=df$Politico, y=df$Gamma,fill=factor(df$Topic,levels=mieitopic))) + 
  geom_bar(position="dodge",stat = "identity") +
  geom_col(show.legend = FALSE) +
  facet_wrap(~df$Partito,scales = "free",ncol=3) +
  labs(x = "Politico", y = "Gamma") +
  guides(fill=guide_legend(title="Topic")) +
  theme(legend.justification=c(0.9,0.2), legend.position=c(0.95,0.2)) +
  #theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size=20)) +
  coord_flip()

################################
# distances between politicians (based on topics covered)
#############################

distanze<-c()
nomi<-c()
for (i in 1:44)
{
  for (j in (i+1):45)
  {
    politico1<-gamma_mat[seq(from=i, to=315, by=45),]
    politico2<-gamma_mat[seq(from=j, to=315, by=45),]
    nomi<-c(nomi, paste(politico1[1,1], politico2[1,1]))
    distanza<- -log(sum(sqrt(politico1[,3]*politico2[,3])))
    distanze<-c(distanze, distanza)
  }
}
distanze
nomi
names(distanze)<-nomi



distanzaEntroPartiti<-function(gamma, posizioneLeader)
{
  num=posizioneLeader
  gamma<-as.data.frame(gamma)
  leader<-gamma[seq(from=num, to=315, by=45),]
  distanza<-0
  for (i in (posizioneLeader+1):(posizioneLeader+9))
  {
    politico<-gamma[seq(from=(i), to=315, by=45),]
    distanzaLeaderPolitico<--log(sum(sqrt(leader[,3]*politico[,3])))
    distanza<-distanza+distanzaLeaderPolitico
  }
  distanza/9
}

distm5s<-distanzaEntroPartiti(gamma_mat, 1)
distpd<-distanzaEntroPartiti(gamma_mat, 11)
distlega<-distanzaEntroPartiti(gamma_mat, 21)
distfi<-distanzaEntroPartiti(gamma_mat, 31)

qplot(distanze,
      geom="histogram",
      binwidth = 0.1,
      xlab = "Distanza di Bhattacharyya",
      ylab= "Frequenza",
      fill=I("blue"),
      col=I("red"),
      alpha=I(.2),
      xlim=c(0,5))
