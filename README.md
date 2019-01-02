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
![alt text](https://github.com/rmenoli/LDA-facebook-post/blob/master/images/Result1.png)
