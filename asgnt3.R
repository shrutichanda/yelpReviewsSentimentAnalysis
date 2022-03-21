# Packages
install.packages("tidytext")
install.packages("SnowballC")
install.packages("textstem")
install.packages("readr")
install.packages("stringr")
install.packages("textdata")
install.packages("tidyr")
install.packages("xgboost")
install.packages("caret")
install.packages("rsample")
 
# Libraries
library(dplyr)
library(readr)
library(ggplot2)
library(tidytext)
library(SnowballC)
library(textstem)
library(textdata)
library(stringr) 
library(tidyr)
library(xgboost)
library(caret)
library(rsample)

############################## QUESTION 1 ######################################

################################ PART I ########################################
############################# preparing data

# access data 
resReviewsData <- read.csv2('yelpRestaurantReviews_sample_s21b.csv')
glimpse(resReviewsData)

#number of reviews by star-rating
starDist <- resReviewsData %>% group_by(starsReview) %>% count()
#graphical representation
ggplot(starDist, aes(x=starsReview, y=n)) + geom_bar(stat="identity")

# tokenize data
rrTokens <- resReviewsData %>% select(, -c(user_id, neighborhood, latitude, longitude, address, hours, is_open, city, name, date, postal_code))  %>% unnest_tokens(word, text)


head(rrTokens)

#distinct words
dim(rrTokens %>% distinct(word))


# remove stopwords
rrTokens <- rrTokens %>% anti_join(stop_words)
dim(rrTokens)

# frequency and sorting
rrTokens %>% count(word, sort=TRUE) %>% top_n(10)

# removing rare words with frequency less than 50
rareWords <- rrTokens%>% count(word, sort=TRUE) %>% filter(n<50)
glimpse(rareWords)

rrdf <-anti_join(rrTokens, rareWords)
glimpse(rrdf)

# remove the terms containing digits
rrdf <-rrdf %>% filter(str_detect(word,"[0-9]") == FALSE)
dim(rrdf)

# remaining distinct tokens
rrdf %>% distinct(word) %>% dim()

# grouping based on star rating 
wordset <- rrdf %>% group_by(starsReview) 

# proportion for each word 
wordsetprop <- wordset %>% count(word, sort=TRUE) %>% mutate(prop=n/sum(n)) 

wordsetprop %>% arrange(starsReview, desc(prop)) %>% filter(row_number(starsReview)<=20) %>% View()

wordsetprop %>% arrange(starsReview, desc(prop)) %>% filter(row_number(starsReview)<=20) %>% ggplot(aes(word, prop))+geom_col()+coord_flip()+facet_wrap((~starsReview))

# finding relation to funny, cool and useful
# FUNNY Reviews
funnyReview <- wordset %>% select(starsReview, funny) %>% count(funny, sort=TRUE)
# plot on graph
funnyReview %>% arrange(starsReview, desc(funny)) %>% ggplot(aes(starsReview, funny))+geom_col()


# COOL Reviews
coolReview <- wordset %>% select(starsReview, cool) %>% count(cool, sort=TRUE)
# plot on graph
coolReview %>% arrange(starsReview, desc(cool)) %>% ggplot(aes(starsReview, cool))+geom_col()

# USEFUL Reviews
usefulReview <- wordset %>% select(starsReview, useful) %>% count(useful, sort=TRUE)
# plot on graph
usefulReview %>% arrange(starsReview, desc(useful)) %>% ggplot(aes(starsReview, useful))+geom_col()

################################ PART II #######################################
busSet <- rrdf %>% group_by(business_id, starsBusiness)%>% count(starsReview) %>% mutate(contri=ifelse(starsReview<3.5, -1, 1), totContri=sum(n*contri))

# proportion of contribution towards business id
busSetProp <- busSet %>% distinct(totContri)

busSetProp %>% ungroup()

data <- busSetProp %>% arrange(starsBusiness, desc(totContri)) %>% View()
busSetProp %>% arrange(starsBusiness, desc(totContri)) %>% ggplot(aes(starsBusiness, totContri))+geom_col()

############################## QUESTION 2 ######################################
####################### pruning highest and lowest frequency of words
wrds <- wordsetprop %>% group_by(word) %>% summarise( totWS= sum(starsReview*prop))

########## highest
wrds %>% top_n(20) 

########## lowest
wrds %>% top_n(-20)

############################## QUESTION 3 ######################################
############################## BING
get_sentiments("bing") %>% View()

rrSenti_bing <- rrTokens %>% inner_join(get_sentiments("bing"), by="word")

rrSenti_bingOcc <- rrSenti_bing %>% group_by(word, sentiment) %>% count(sentiment) %>% summarise(totOcc=sum(n)) %>% arrange(sentiment, desc(totOcc))

#negate the counts for the negative sentiment words
rrSenti_bingOcc <- rrSenti_bingOcc %>% mutate(totOcc=ifelse(sentiment=="positive", totOcc, -totOcc))

# which are the most positive and most negative words in reviews
rrSenti_bingOcc <- ungroup(rrSenti_bingOcc)

rrSenti_bingOcc %>% top_n(25)
rrSenti_bingOcc %>% top_n(-25)

# plot them on graph
rbind(top_n(rrSenti_bingOcc, 25), top_n(rrSenti_bingOcc, -25)) %>% mutate(word=reorder(word,totOcc)) %>% ggplot(aes(word, totOcc, fill=sentiment)) +geom_col()+coord_flip()

# sentiment for reviews
# positive/negative sentiment words per review
rvSenti_bing <- rrSenti_bing %>% group_by(review_id, starsReview) %>% summarise(nwords=n(),posSum=sum(sentiment=='positive'), negSum=sum(sentiment=='negative'))

# calculate sentiment score based on proportion of positive, negative words
rvSenti_bing <- rvSenti_bing%>% mutate(posProp=posSum/nwords, negProp=negSum/nwords)

rvSenti_bing <- rvSenti_bing%>% mutate(sentiScore=posProp-negProp)

temp <- rvSenti_bing %>% filter(nwords > 20) %>% arrange(nwords, desc(sentiScore))

temp <- ungroup(temp)

temp %>% top_n(20) %>% View()

temp %>% top_n(-20) %>% View()

# plot them on graph
rbind(top_n(temp, 20), top_n(temp, -20)) %>% mutate(stars=reorder(starsReview, sentiScore)) %>% ggplot(aes(stars, sentiScore, fill=starsReview)) +geom_col()+coord_flip()

############################## NRC
get_sentiments("nrc") %>% View()

rrSenti_nrc <- rrTokens %>% inner_join(get_sentiments("nrc"), by="word")

rrSenti_nrcOcc <-rrSenti_nrc %>% group_by(word, sentiment) %>% count(sentiment) %>% summarise(totOcc=sum(n)) %>% arrange(sentiment, desc(totOcc))

#How many words are there for the different sentiment categories
rrSenti_nrcOcc %>% group_by(sentiment) %>% summarise(count=n(), sumn=sum(totOcc))

#top few words for different sentiments
rrSenti_nrcOcc%>% group_by(sentiment) %>% top_n(10) %>% View()

rrSenti_nrcOcc <- rrSenti_nrcOcc %>% mutate(goodBad=ifelse(sentiment %in% c('anger', 'disgust', 'fear', 'sadness', 'negative'), -totOcc, ifelse(sentiment %in% c('positive', 'joy', 'anticipation', 'trust', 'surprise'), totOcc, 0)))

rrSenti_nrcOcc <- ungroup(rrSenti_nrcOcc)

top_n(rrSenti_nrcOcc, 20)
top_n(rrSenti_nrcOcc, -20)

# plot them on graph
rbind(top_n(rrSenti_nrcOcc, 20), top_n(rrSenti_nrcOcc, -20)) %>% mutate(words=reorder(word, goodBad)) %>% ggplot(aes(word, goodBad, fill=sentiment)) +geom_col()+coord_flip()

# sentiment for reviews
# positive/negative sentiment words per review
rvSenti_nrc <- rrSenti_nrc %>% group_by(review_id, starsReview, sentiment) %>% count(sentiment) %>% summarise(totOcc=sum(n)) %>% mutate(goodBad=ifelse(sentiment %in% c('anger', 'disgust', 'fear', 'sadness', 'negative'), -totOcc, ifelse(sentiment %in% c('positive', 'joy', 'anticipation', 'trust', 'surprise'), totOcc, 0))) %>% arrange(sentiment, desc(totOcc))

rvSenti_nrc <- ungroup(rvSenti_nrc)

top_n(rvSenti_nrc, 20)
top_n(rvSenti_nrc, -20)

# plot them on graph
rbind(top_n(rvSenti_nrc, 20), top_n(rvSenti_nrc, -20)) %>% mutate(stars=reorder(starsReview, goodBad)) %>% ggplot(aes(stars, goodBad, fill=sentiment)) +geom_col()+coord_flip()

############################## AFINN
get_sentiments("afinn") %>% View()

rrSenti_afinn<- rrTokens%>% inner_join(get_sentiments("afinn"), by="word")

rrSenti_afinnOcc <- rrSenti_afinn %>% group_by(word, value) %>% count(value) %>% summarise(totOcc=sum(n)) %>% arrange(value, desc(totOcc))

rrSenti_afinnOcc <- ungroup(rrSenti_afinnOcc)

tempTop <- rrSenti_afinnOcc %>% filter(totOcc > 50 & value > 0) %>% top_n(20)
tempBtm <- rrSenti_afinnOcc %>% filter(totOcc > 50 & value <0) %>% top_n(-20)

# plot them on graph
rbind(tempTop, tempBtm) %>% mutate(words=reorder(word, value)) %>% ggplot(aes(words, value, fill=value)) +geom_col()+coord_flip()

# sentiment for reviews
# positive/negative sentiment words per review
rvSenti_afinn <- rrSenti_afinn %>% group_by(review_id, starsReview) %>% summarise(nwords=n(), sentiSum=sum(value)) %>% filter(nwords > 20) %>% arrange(nwords, desc(sentiSum))

rvSenti_afinn %>% group_by(starsReview) %>% summarise(avgLen=mean(nwords), avgSenti=mean(sentiSum))

#considering reviews with 1 to 2 stars as negative, and this with 4 to 5 stars as positive
rvSenti_afinn <- rvSenti_afinn %>% mutate(hiLo= ifelse(starsReview <= 2, -1, ifelse(starsReview >=4, 1, 0 )))
rvSenti_afinn <- rvSenti_afinn %>% mutate(pred_hiLo=ifelse(sentiSum> 0, 1, -1))
#filter out the reviews with 3 stars, and get the confusion matrix for hiLovs pred_hiLo
afinnCal <- rvSenti_afinn %>% filter(hiLo!=0)
table(actual=afinnCal$hiLo, predicted=afinnCal$pred_hiLo)

rvSenti_afinn <- ungroup(rvSenti_afinn)

tempTop_afinn <- rvSenti_afinn %>% filter(nwords > 20) %>% top_n(20)
tempBtm_afinn <- rvSenti_afinn %>% filter(nwords > 20) %>% top_n(-20)

# plot them on graph
rbind(tempTop_afinn, tempBtm_afinn) %>% mutate(stars=reorder(starsReview, sentiSum)) %>% ggplot(aes(stars, sentiSum, fill=hiLo)) +geom_col()+coord_flip()

############################## QUESTION 4 ######################################
################################  SVD ######################################
rvTokens <- resReviewsData %>% select(review_id,starsReview, text ) %>% unnest_tokens(word, text) %>% anti_join(stop_words)%>% mutate(word_lemma = textstem::lemmatize_words(word))

rvTokens<-rvTokens %>% mutate(word = textstem::lemmatize_words(word))
rvTokens<-rvTokens %>% filter(str_length(word)<=3 | str_length(word)<=15)
dim(rvTokens)
rvTokens<- rvTokens %>% group_by(review_id, starsReview) %>% count(word)
totWords<-rvTokens %>% group_by(review_id)%>% count(word, sort=TRUE) %>% summarise(total=sum(n))

rrtokens_prop<-left_join(rvTokens, totWords)

rrtokens_prop<-rrtokens_prop %>% mutate(tf=n/total)
head(rrtokens_prop)
rvTokens<-rvTokens %>% bind_tf_idf(word, review_id, n)

rvTokenSenti_bing<- rvTokens %>% inner_join( get_sentiments("bing"), by="word")

revDTM_sentiBing <- rvTokenSenti_bing %>% pivot_wider(id_cols = c(review_id, starsReview), names_from = word, values_from = tf_idf) %>% ungroup()

revDTM_sentiBing$hiLo <- 0
revDTM_sentiBing <- revDTM_sentiBing %>% filter(starsReview!=3) %>% mutate(hiLo=ifelse(starsReview<3, -1, 1)) %>% select(-starsReview)

revDTM_sentiBing <- sample_n(revDTM_sentiBing, 12000)

revDTM_sentiBing_split<- initial_split(revDTM_sentiBing, 0.5)
revDTM_sentiBing_trn<- training(revDTM_sentiBing_split)
revDTM_sentiBing_tst<- testing(revDTM_sentiBing_split)

library(e1071)

svmSenti_bing<- svm(hiLo~., data=revDTM_sentiBing_trn%>%select(-c(review_id)), kernel="radial",  gamma=1, cost=1)
svmPredTrn <- predict(svmSenti_bing, revDTM_sentiBing_tst)

############################## QUESTION 5 ######################################
############################ Data exploration ##################################
rrAttrDf <- rrTokens %>% select(review_id, attributes) %>% mutate(attrs = str_split(attributes, '\\|')) %>% unnest(attrs)

rrAttr <- rrAttrDf %>% cbind(str_split_fixed(rrAttrDf$attrs, ":", 2))
colnames(rrAttr)[4]<-'attName'
colnames(rrAttr)[5]<-'attValue'

rrAttr <- rrAttr %>% select(-c(attributes, attrs))

rrAttr <- rrAttr %>% pivot_wider(names_from = attName, values_from = attValue)
