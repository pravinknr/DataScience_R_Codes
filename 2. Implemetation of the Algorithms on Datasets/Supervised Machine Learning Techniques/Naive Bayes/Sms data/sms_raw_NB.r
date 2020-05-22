#Naive Bayes Classifier

#Build a naive Bayes model on the data set for classifying the ham and spam

library(gmodels)
library(tm)
library(e1071)
library(wordcloud)

sms_raw <- read.csv(file.choose(), stringsAsFactors = FALSE)

str(sms_raw)
# Lets Convert the Type Column to factor.
sms_raw$type <- factor(sms_raw$type)

str(sms_raw$type)
table(sms_raw$type)

#Lets build a corpus for the Raw data

sms_corpus <- Corpus(VectorSource(sms_raw$text))
print(sms_corpus)
inspect(sms_corpus[1:3])

# Lets clean up the corpus (Data Transformation)
corpus_clean <- tm_map(sms_corpus, tolower)
corpus_clean <- tm_map(corpus_clean, removeNumbers)
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())
corpus_clean <- tm_map(corpus_clean, removePunctuation)
corpus_clean <- tm_map(corpus_clean, stripWhitespace)


inspect(sms_corpus[1:5])
inspect(corpus_clean[1:5])

# create a document-term matrix
sms_dtm <- DocumentTermMatrix(corpus_clean)
sms_dtm

# creating training and test for raw datasets
sms_raw_train <- sms_raw[1:4169, ]
sms_raw_test  <- sms_raw[4170:5559, ]

sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test  <- sms_dtm[4170:5559, ]

sms_corpus_train <- corpus_clean[1:4169]
sms_corpus_test  <- corpus_clean[4170:5559]

# check that the proportion of spam is similar
prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))

# word cloud visualization
wordcloud(sms_corpus_train, min.freq = 20, random.order = FALSE)

# subset the training data into spam and ham groups
spam <- subset(sms_raw_train, type == "spam")
ham  <- subset(sms_raw_train, type == "ham")

wordcloud(spam$text, max.words = 50, scale = c(3, 0.5),colors = 'red')
wordcloud(ham$text, max.words = 40, scale = c(3, 1), colors = "pink")

# indicator features for frequent words
sms_dict<-findFreqTerms(sms_dtm_train, 3)

sms_train <- DocumentTermMatrix(sms_corpus_train, list(dictionary = sms_dict))
sms_test  <- DocumentTermMatrix(sms_corpus_test, list(dictionary = sms_dict))

# convert counts to a factor
convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
}

# apply() convert_counts() to columns of train/test data
sms_train <- apply(sms_train, MARGIN = 2, convert_counts) #Here Margin =2 means COlumns
sms_test  <- apply(sms_test, MARGIN = 2, convert_counts)

#Lets create a classification model
sms_classifier <- naiveBayes(sms_train, sms_raw_train$type)

#Lets Predict for the test set
sms_test_pred <- predict(sms_classifier, sms_test)

#Lets Check the Accuracy 
CrossTable(sms_test_pred, sms_raw_test$type,prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,dnn = c('predicted', 'actual'))

sms_classifier2 <- naiveBayes(sms_train, sms_raw_train$type, laplace = 1)
sms_test_pred2 <- predict(sms_classifier2, sms_test)
CrossTable(sms_test_pred2, sms_raw_test$type,prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,dnn = c('predicted', 'actual'))
