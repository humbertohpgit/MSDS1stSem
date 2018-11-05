##install.packages("tm")
##install.packages("tidytext")
##install.packages("RTextTools")

## Defining spam and ham directory paths and building function for extracting only the message content (no email headers)

setwd("C:/DATA/HHP/Personal/Degrees/Ms. Data Science (CUNY)/R Working Dir")
spam_path <- "C:/DATA/HHP/Personal/Degrees/Ms. Data Science (CUNY)/R Working Dir/spam_2/"
ham_path <- "C:/DATA/HHP/Personal/Degrees/Ms. Data Science (CUNY)/R Working Dir/easy_ham/"
get_msg <- function(path) {
  con <- file(path,open="rt",encoding="latin1")
  text <- readLines(con)
  msg <- text[seq(which(text=="")[1]+1,length(text))]  
  close(con)
  return(paste(msg,collapse="\n"))
}

## Building spam and ham email datasets
spam_files <- dir(spam_path)
spam_ds <- sapply(spam_files, function(p) get_msg(paste(spam_path, p, sep="")))

ham_files <- dir(ham_path)
ham_ds <- sapply(ham_files, function(p) get_msg(paste(ham_path, p, sep="")))

head(spam_ds,1)
length(spam_ds)
length(ham_ds)

## Create Corpus and Term-Document Matrices for spam and ham data sets
library(tm)
control <- list(stopwords=TRUE, removePunctuation=TRUE, removeNumbers=TRUE, minDocFreq=2)

spam_corpus <- Corpus(VectorSource(spam_ds))
spam_tdm <- TermDocumentMatrix(spam_corpus, control)

ham_corpus <- Corpus(VectorSource(ham_ds))
ham_tdm <- TermDocumentMatrix(ham_corpus, control)

#summary(spam_corpus)
spam_tdm
ham_tdm

## Removing sparse terms (80% of sparse percentage of empty)
spam_tdm_unsprsd <- removeSparseTerms(spam_tdm, 0.8)
ham_tdm_unsprsd <- removeSparseTerms(ham_tdm, 0.8)
spam_tdm_unsprsd
ham_tdm_unsprsd

## Inspecting matrix
inspect(spam_tdm_unsprsd[1:10,1:5])
## Top terms by frequency (mentioned at least 50 times)
## Spam
length(findFreqTerms(spam_tdm_unsprsd,50))
spam_topterms <- findFreqTerms(spam_tdm_unsprsd,50)
spam_topterms
## Ham
length(findFreqTerms(ham_tdm_unsprsd,50))
ham_topterms <- findFreqTerms(ham_tdm_unsprsd,50)
ham_topterms

## Find top associations for the top 10 terms (lower correlation limit of 0.4)
spam_assocs <- findAssocs(spam_tdm_unsprsd, spam_topterms[1:10], 0.4) ## more consistent association patterns found in spam
spam_assocs
ham_assocs <- findAssocs(ham_tdm_unsprsd, ham_topterms[1:10], 0.4) ## no consistent association patterns found in ham
ham_assocs

## Generate Master list of spam identification terms and create a wordcloud
##intersect(spam_topterms, ham_topterms)
spam_id_terms <- setdiff(spam_topterms, ham_topterms)
spam_id_terms

library(wordcloud)
spam_tdm_cloud <- as.matrix(spam_tdm_unsprsd)
v <- sort(rowSums(spam_tdm_cloud),decreasing=TRUE)
d <- data.frame(word=names(v),freq=v)	
wordcloud(d$word,d$freq,max.words=50, min.freq=50, colors=brewer.pal(8, 'Dark2'))


## Classification of emails (Spam and Ham)

## Create Corpuses, Assign Labels and Create Combined Document-Text Matrix for Spam and Ham with only relevant terms
## Use VCorpus for the labels assignment and the classifiers to work properly. Corpus does not allow meta information to be assigned to corpus (for labels) plus SVM and Maxent classifiers do not work
spam_corpus <- VCorpus(VectorSource(spam_ds))
for (i in 1:length(spam_corpus)) {
  meta(spam_corpus[[i]], tag="emailtype") <- c("Spam")
}

ham_corpus <- VCorpus(VectorSource(ham_ds))
for (i in 1:length(ham_corpus)) {
  meta(ham_corpus[[i]], tag="emailtype") <- c("Ham")
}

email_corpus <- c(spam_corpus, ham_corpus)
email_corpus <- email_corpus[sample(c(1:length(email_corpus)))]

controldtm <- list(stopwords=TRUE, removePunctuation=TRUE, removeNumbers=TRUE)
email_dtm <- DocumentTermMatrix(email_corpus, controldtm)
email_dtm

## Removing sparse terms (80% of sparse percentage of empty)
email_dtm_unsprsd <- removeSparseTerms(email_dtm, 0.8)
email_dtm_unsprsd
inspect(email_dtm_unsprsd[1:10,1:10])

## Create Container with all relevant information to use in the classification procedure
library(RTextTools)
email_labels <- unlist(meta(email_corpus, "emailtype"))
N <- length(email_labels)
splitN <- round(0.7*N)
table(email_labels)
container_email <- create_container(email_dtm_unsprsd, labels = email_labels, trainSize = 1:splitN, testSize = splitN:N, virgin = FALSE)

## Training the models (Support Vector Machines, Random Forest and MaxEntropy)
svm_model <- train_model(container_email, "SVM")
tree_model <- train_model(container_email, "TREE")
maxent_model <- train_model(container_email, "MAXENT")

## Scoring Emails (Spam and Ham)
svm_score <- classify_model(container_email, svm_model)
tree_score <- classify_model(container_email, tree_model)
maxent_score <- classify_model(container_email, maxent_model)

## Presenting Results
library(knitr)

table(email_labels[splitN:N])
table(as.character(svm_score[,1]))
table(as.character(tree_score[,1]))
table(as.character(maxent_score[,1]))

results <- data.frame(
  email_labels[splitN:N],
  as.character(svm_score[,1]),
  as.character(tree_score[,1]),
  as.character(maxent_score[,1]))

colnames(results) <- c("CorrectLabel", "SVM", "DTree", "MaxEntropy")
kable(summary(results))
      