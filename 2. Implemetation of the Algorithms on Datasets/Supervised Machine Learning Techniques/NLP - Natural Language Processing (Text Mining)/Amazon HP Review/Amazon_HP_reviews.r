#Text mining

#1) Extract reviews of any product from ecommerce website like snapdeal and amazon
#2) Perform sentimental analysis

library(magrittr)
library(rvest)
library(XML)

#Lets Extract the Amazon Reviews

#I am going to Extract the Reviews of "HP Pavilion Gaming 9th Gen Intel Core i5 Processor 15.6-inch FHD Gaming Laptop (8GB/512GB SSD/Windows 10/NVIDIA GTX 1650 4GB Graphics/Shadow Black/2.17 Kg), 15-bc513TX" from amazon website
rurl <- "https://www.amazon.in/HP-Pavilion-15-6-inch-Graphics-15-bc513TX/product-reviews/B07S8VNK4T/ref=cm_cr_dp_d_show_all_btm?ie=UTF8&reviewerType=all_reviews"

amazon_reviews <- NULL
for (i in 1:20){
  murl <- read_html(as.character(paste(rurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}

length(amazon_reviews)

#Lets Write it as a txt file
write.table(amazon_reviews, file = "HP reviews.txt", row.names = F)

#Now Lets Import the Review file
review <- readLines(file.choose())
review
length(review)

stpwrd <- scan(file.choose(), what="character", comment.char=";") #Read-In the stop.txt file

library(tm)
#lets Create the Corpus
review.corpus <- VCorpus(VectorSource(review))
inspect(review.corpus[2])

#Lets Do the Data Cleaning
review.corpus1 <- tm_map(review.corpus, tolower)
inspect(review.corpus1[6])

review.corpus1 <- tm_map(review.corpus, removePunctuation)

review.corpus1 <- tm_map(review.corpus1, removeNumbers)

review.corpus1 <- tm_map(review.corpus1, removeWords, stpwrd)

review.corpus1 <- tm_map(review.corpus1, stripWhitespace) #Removes the WhiteSpaces in the Vector

#Lets Form the Term Document Matrix
tdm <- TermDocumentMatrix(review.corpus1)
dtm <- t(tdm) #Converting the Trem Document Matrix into Document Term Matrix using Transponse Function
tdm <- as.matrix(tdm)

rs <-rowSums(tdm)
rs
rs_sub <- subset(rs, rs >=40)
rs_sub
  
#Lets Plot it
barplot(rs_sub, las = 3, col = rainbow(20) )
#The Word Laptop is used the most in the Reviews

#Lets Make a Word Cloud 
install.packages("wordcloud", dependencies = T)
library(wordcloud)

windows()
wordcloud(words = names(rs_sub), freq = rs_sub) #This Creates a Wordcloud only with the rs_sub values
#In this WordCloud, the Terms with large Text are the most used words in the Reviews

#Lets Create a WordClou COnsiering all the Words in the rs
rs1 <- sort(rs, decreasing = T)
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
#Here the Error is used in most of the Reviews

#Lets Do Emotion Mining
library(syuzhet)

review <- iconv(review, "UTF-8")

x <- get_nrc_sentiment(review)
head(x)
head(x,100)

x1 <- get_sentences(review)
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
#So by Viewing this Plot we conclude that this Product has more Positive Reviews so a Person can Think of buying it