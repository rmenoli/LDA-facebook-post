# Analysis of Italian Politicians' Facebook Posts via LDA (Latent Dirichlet Allocation)
Project for the course "Statistical Methods for Big Data", University of Padua.

## Introduction
The dataset consists of the Facebook posts of 45 Italian politicians from 1st of January 2017 to 30th of April 2017.
- The first corpus (just after downloading) has size 3.9 MB and 39035 words.
- The final corpus (after pre-processing) has size 1.3 MB and 2814 words.

## Downloading the data
Data has been downloaded using the R Facebook API, i.e. the function "getpage" implemented in the library "Rfacebook".

## Preprocessing
1. Convert all characters to lower cases.
2. Delete numbers, punctuation marks, white spaces, proper names, political parties names, Web special characters and stopwords.
3. Create bigrams with high frequency (greater than 30).
4. Delete the most frequent words, i.e. words with frequency greater than 0.975 percentile.
5. Delete the least frequent words, i.e. words which appear in less than 10 documents.

Stemming was not performed, in order to improve the interpretability of the LDA model. Another reason for not using stemming was the poor stemming function capabilities with Italian language.

## Model fitting
The function used for fitting the LDA model was "LDA" implemented in the R library "topicmodels".

## Results
### Distribution of the top-10 words for each topic
The final LDA model has 7 topics. 
The figure below shows the distribution of the top-10 words among the 7 topics. As an example take the word "clandestini" ("stowaways" in english), which is the most probable word in Topic 4 (beta = 0.00643) and in Topic 2 (beta = 0.0118).
![alt text](https://github.com/rmenoli/LDA-facebook-post/blob/master/images/Result1.png)
Given the top-10 words for each topic, we can naturally assign a label to each topic:
- Topic 1: EU affairs
- Topic 2: Protest against immigration
- Topic 3: Democratic primary elections
- Topic 4: Organization of migration flows
- Topic 5: Protest against establishment
- Topic 6: Social issues
- Topic 7: Italian politics

### Distribution of topics for each politician
The figure below plots the distribution of topics (i.e. gamma) among the 45 politicians, grouped by their political party (Movimento 5 Stelle, Partito Democratico, Lega Nord, Fratelli d'Italia, Others). Politicians belonging to Movimento 5 Stelle homogeneously focus on Topic 4 (Organization of migration flows). Moreover, Topic 6 (Social issues) is addressed by Brambilla (animal rights activist), Meloni (nationalist) and Saltamarini (maternity protection activist).
![alt text](https://github.com/rmenoli/LDA-facebook-post/blob/master/images/Result2.png)

### Most representative words for each topic
It is useful to compare micro-topics which address different issues of the same macro-topic, e.g. Topic 2 and Topic 4 both regard immigration issues, but from different perspectives.
Therefore, the first task was to understand which are the most discriminative words for Topic 2, when compared to Topic 4. This can be obtained by computing, for each word, the log-ratio of the two values stored in the beta matrix, taking the biggest and finally plotting them in a paired word-cloud.
![alt text](https://github.com/rmenoli/LDA-facebook-post/blob/master/images/Result3.png)

## Conclusions
Advantages of LDA:
- Flexibility (the same word can be assigned to multiple topics)
- Generality (high-level picture of politicians' debate in Italy) 
- Granularity (lower-level picture of differences in topics among politicians belonging to the same party)

Disadvantages of LDA:
- Choose number of topics (since LDA is an unsupervised method)
- Model assumptions (LDA assumes independence between topics, while Correlated Topic Model assumes correlation between topics, which is more reasonable in this context)

