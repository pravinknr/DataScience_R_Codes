#Text Mining
#Sentiment Analysis

#Extract movie reviews for any movie from IMDB and perform sentimental analysis


library(magrittr)
library(rvest)
library(XML)

#I am Going to extract the reviews of "Paatal Lok" from IMDB
rurl <- "https://www.imdb.com/title/tt9680440/reviews?ref_=tt_urv"

IMDB_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(rurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".show-more__control") %>%
    html_text()
  IMDB_reviews <- c(IMDB_reviews,rev)
}

length(IMDB_reviews)

write.table(IMDB_reviews, "Paatal_Lok_Reviews.txt", row.names = F)

#Now Lets Import the Review file
rev <- readLines(file.choose())
rev
length(rev)

stpwrd <- readLines(file.choose()) #Read-In the stop.txt file

library(tm)
#lets Create the Corpus
rev.corpus <- Corpus(VectorSource(rev))
inspect(rev.corpus[2])

#Lets Do the Data Cleaning
rev.corpus1 <- tm_map(rev.corpus, tolower)
inspect(rev.corpus1[6])

rev.corpus1 <- tm_map(rev.corpus, removePunctuation)

rev.corpus1 <- tm_map(rev.corpus1, removeNumbers)

rev.corpus1 <- tm_map(rev.corpus1, removeWords, c(stpwrd,"the"))

rev.corpus1 <- tm_map(rev.corpus1, stripWhitespace) #Removes the WhiteSpaces in the Vector

#Lets Form the Term Document Matrix
tdm <- TermDocumentMatrix(rev.corpus1)
dtm <- t(tdm) #Converting the Trem Document Matrix into Document Term Matrix using Transponse Function
tdm <- as.matrix(tdm)

rows <-rowSums(tdm)
rows
rs_sub <- subset(rows, rows >=10)
rs_sub 

#Lets Plot it
barplot(rs_sub, las = 3, col = rainbow(20))
#The Word "watch" is used the most in the Reviews

#Lets Make a Word Cloud 
library(wordcloud)

windows()
wordcloud(words = names(rs_sub), freq = rs_sub) #This Creates a Wordcloud only with the rs_sub values
#In this WordCloud, the Terms with large Text are the most used words in the Reviews

#Lets Create a WordClou COnsiering all the Words in the rs
rs1 <- sort(rows, decreasing = T)
wordcloud(words = names(rs1), freq = rs1)

#lets make an Attractive Wordcloud by adding colors to it
wordcloud(words = names(rs1), freq = rs1, random.order = F, colors = rainbow(30), scale = c(2,1), rot.per = 0.2)

#Now lets do sentimental Analysis
#load the Positive and Negavtive words
pos.words = scan(file.choose(), what="character", comment.char=";")	# read-in positive-words.txt
neg.words = scan(file.choose(), what="character", comment.char=";") 	# read-in negative-words.txt

#Positive Wordcloud
pos.match <- match(names(rs1), pos.words)
pos.match <- !is.na(pos.match)

pos.freq <- rs1[pos.match]
pos.match.words <- names(pos.freq)
windows()
wordcloud(words = pos.match.words, freq = pos.freq, colors = rainbow(20), scale = c(3,1))
#Here the Term Good is used in many of the Reviews

#Lets make the Negative Wordcloud
neg.match <- match(names(rs1), neg.words)
neg.match <- !is.na(neg.match)
neg.freq <- rs1[neg.match]
neg.match.words <- names(neg.freq)
windows()
wordcloud(words = neg.match.words, freq = neg.freq, colors = rainbow(20))
#Here the word "propaganda is used in most of the Reviews

#Lets Do Emotion Mining
library(syuzhet)

review <- iconv(rev, "UTF-8")

x <- get_nrc_sentiment(review)
head(x)
head(x,100)

x1 <- get_sentences(rev)
class(x1)
str(x1)
head(x1)

sentiment_vector <- get_sentiment(x1, method = "bing")
head(sentiment_vector, 100)
sum(sentiment_vector)
mean(sentiment_vector)
summary(sentiment_vector)

afinn_s_v <- get_sentiment(x1, method = "afinn")
head(afinn_s_v,100)
sum(afinn_s_v)
mean(afinn_s_v)
summary(afinn_s_v)

nrc_vector <- get_sentiment(x1, method="nrc")
head(nrc_vector,100)
sum(nrc_vector)
mean(nrc_vector)
summary(nrc_vector)

plot(sentiment_vector,type='l',main ='Plot trajectory',xlab='Narative time',ylab='Emotional valence')
abline(h=0,col="red")

plot(sentiment_vector, type="h", main="Example Plot Trajectory", xlab = "Narrative Time", ylab= "Emotional Valence")

# To extract the sentence with the most negative emotional valence
negative <- x1[which.min(sentiment_vector)]
negative

# and to extract the most positive sentence
positive <- x1[which.max(sentiment_vector)]
positive

# percentage based figures
percent_vals <- get_percentage_values(sentiment_vector)

plot(percent_vals, type="l",main="Percentage-Based Means", xlab = "Narrative Time", ylab= "Emotional Valence", col="red")

#Shape smoothing and normalization using a Fourier based transformation and low pass filtering is achieved using the get_transformed_values function as shown below.
ft_values <- get_transformed_values(sentiment_vector, low_pass_size = 3, x_reverse_len = 100,padding_factor = 2,scale_vals = TRUE,scale_range = FALSE)
plot(ft_values, type ="l", main ="HP Pavilion Reviews using Transformed Values", xlab = "Narrative Time", ylab = "Emotional Valence", col = "red")

# categorize each sentence by eight emotions
nrc_data <- get_nrc_sentiment(x1)
nrc_score_sent <- get_nrc_sentiment(negative)

sad_items <- which(nrc_data$sadness > 0)
head(x1[sad_items])

# To view the emotions as a barplot
barplot(sort(colSums(prop.table(nrc_data[, 1:10]))), horiz = T, cex.names = 0.7,las = 1, main = "Emotions", xlab = "Percentage",col = 1:8)
#In this Review Set we infer that this WebSeries has Almost Same Positive and Negative Reviews, But The Positive Reviews are Slightly more than Negative Reviews. 