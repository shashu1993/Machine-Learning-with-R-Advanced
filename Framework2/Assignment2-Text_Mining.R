#Assignment 2

RNGversion(vstr = 3.6)

review_data = read.csv('/Users/shashank/Desktop/Semester 2/Frameworks 2/Week 5/baby_reviews.csv',stringsAsFactors = F)


#Section 1

#q1
str(review_data)
#ans: 4978

#q2
mean(review_data$review_rating)
#ans: 4.227601

#q3
library(ggplot2); library(ggthemes)
ggplot(data=review_data,aes(x=review_rating))+
  geom_histogram(fill='sienna')+
  theme_economist()+
  coord_flip()
mean(nchar(review_data$review))

#ans: 441.8248

#q4
cor(nchar(review_data$review),review_data$review_rating)
cor.test(nchar(review_data$review),review_data$review_rating)
#ans: false

#q5
#install.packages("stringr")
library(stringr)
median(str_count(string = review_data$review,pattern = '\\S+'))
#ans: 57

#q6
library(dplyr); library(tidytext)
summary(review_data %>%
  select(id,review)%>%
  group_by(id)%>%
  unnest_tokens(output = word,input=review)%>%
  ungroup()%>%
  group_by(id)%>%
  summarize(count = n()))

summary(str_count(string = review_data$review,pattern = '\\S+'))
max(summary(str_count(string = review_data$review,pattern = '\\S+')))

#ans6:1041

#q7
#ans7: 2

#q8
library(tidytext)
review_data%>%
  unnest_tokens(input = review, output = word)%>%
  select(word)%>%
  group_by(word)%>%
  summarize(count = n())%>%
  ungroup()%>%
  arrange(desc(count))%>%
  top_n(10)

#ans:'the' 'and' 

#q9
review_data%>%
  unnest_tokens(input = review, output = word)%>%
  select(word)%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  summarize(count = n())%>%
  ungroup()%>%
  arrange(desc(count))%>%
  top_n(10)

#ans: 'baby' 'easy' 'love'

#Section 2

#q1
review_data %>%
  select(id,review)%>%
  group_by(id)%>%
  unnest_tokens(output = word,input=review)%>%
  ungroup()%>%
  count()

#ans: 421790

#q2
as.data.frame(get_sentiments('bing'))[1:50,]  

review_data%>%
  group_by(id)%>%
  unnest_tokens(output = word, input = review)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(sentiment)%>%
  count()

review_data%>%
  group_by(id)%>%
  unnest_tokens(output = word, input = review)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(sentiment)%>%
  count()
#ans: 24462


#q3
review_data %>%
  select(id,review)%>%
  group_by(id)%>%
  unnest_tokens(output=word,input=review)%>%
  ungroup()%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(sentiment)%>%
  summarize(n = n())%>%
  mutate(proportion = n/sum(n))
#ans: 0.737



#q4
review_data %>%
  select(id,review,review_rating)%>%
  group_by(id)%>%
  unnest_tokens(output=word,input=review)%>%
  ungroup()%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(review_rating,sentiment)%>%
  summarize(n = n())%>%
  mutate(proportion = n/sum(n))

#ans: 5

#q5
#install.packages("lexicon")
library(lexicon)
nrc = read.table(file = 'https://raw.githubusercontent.com/pseudorational/data/master/nrc_lexicon.txt',
                 header = F,
                 col.names = c('word','sentiment','num'),
                 sep = '\t',
                 stringsAsFactors = F)
nrc = nrc[nrc$num!=0,]
nrc$num = NULL
nrc%>%
  group_by(sentiment)%>%
  count()

table(nrc$sentiment)  

review_data%>%
  group_by(id)%>%
  unnest_tokens(output = word, input = review)%>%
  inner_join(nrc)%>%
  group_by(sentiment)%>%
  count()%>%
  ggplot(aes(x=reorder(sentiment,X = n),y=n,fill=sentiment))+geom_col()+guides(fill=F)+coord_flip()+theme_wsj()

review_data%>%
  group_by(id)%>%
  unnest_tokens(output = word, input = review)%>%
  inner_join(nrc)%>%
  group_by(sentiment)%>%
  count()

#ans: 3815

#q6
#ans: 8429

#q7
afinn = read.table('https://raw.githubusercontent.com/pseudorational/data/master/AFINN-111.txt',
                   header = F,
                   quote="",
                   sep = '\t',
                   col.names = c('word','value'), 
                   encoding='UTF-8',
                   stringsAsFactors = F)
review_data %>%
  select(id,review)%>%
  group_by(id)%>%
  unnest_tokens(output=word,input=review)%>%
  inner_join(afinn)%>%
  filter(id==617)%>%
  summarize(reviewSentiment = mean(value))

review_data %>%
  select(id,review)%>%
  group_by(id)%>%
  unnest_tokens(output=word,input=review)%>%
  inner_join(afinn)%>%
  summarize(reviewSentiment = mean(value))%>%
  ungroup()%>%
  summarize(min=min(reviewSentiment),max=max(reviewSentiment),median=median(reviewSentiment),mean=mean(reviewSentiment))

#ans: -3


#q8

review_data %>%
  select(id,review)%>%
  group_by(id)%>%
  unnest_tokens(output=word,input=review)%>%
  inner_join(afinn)%>%
  filter(id==91)%>%
  summarize(reviewSentiment = mean(value))

review_data %>%
  select(id,review)%>%
  group_by(id)%>%
  unnest_tokens(output=word,input=review)%>%
  inner_join(afinn)%>%
  filter(id==146)%>%
  summarize(reviewSentiment = mean(value))

review_data %>%
  select(id,review)%>%
  group_by(id)%>%
  unnest_tokens(output=word,input=review)%>%
  inner_join(afinn)%>%
  filter(id==238)%>%
  summarize(reviewSentiment = mean(value))

review_data %>%
  select(id,review)%>%
  group_by(id)%>%
  unnest_tokens(output=word,input=review)%>%
  inner_join(afinn)%>%
  filter(id==1432)%>%
  summarize(reviewSentiment = mean(value))

review_data %>%
  select(id,review)%>%
  group_by(id)%>%
  unnest_tokens(output=word,input=review)%>%
  inner_join(afinn)%>%
  filter(id==2598)%>%
  summarize(reviewSentiment = mean(value))

#ans: 91, 238, 2598

#q9
#ans: 1.38



#Section 3

#q1
#install.packages("tm")
library(tm)
corpus = Corpus(VectorSource(review_data$review))
length(corpus)
corpus = tm_map(corpus,FUN = content_transformer(tolower))
corpus = tm_map(corpus,FUN = removePunctuation)
corpus = tm_map(corpus,FUN = removeWords,c(stopwords('english')))
corpus = tm_map(corpus,FUN = stripWhitespace)

dict = findFreqTerms(DocumentTermMatrix(Corpus(VectorSource(review_data$review))),
                     lowfreq = 0)
dict_corpus = Corpus(VectorSource(dict))

corpus = tm_map(corpus,FUN = stemDocument)

dtm = DocumentTermMatrix(corpus)
dtm
inspect(dtm)

#ans 1: 11730

#q2
inspect(dtm[100,])
inspect(dtm[100,'amazon'])

#ans: 1

#q3
xdtm = removeSparseTerms(dtm,sparse = 0.90)
xdtm

inspect(xdtm)

#ans: 47

#q4
xdtm = as.data.frame(as.matrix(xdtm))
colnames(xdtm) = stemCompletion(x = colnames(xdtm),
                                dictionary = dict_corpus,
                                type='prevalent')

colnames(xdtm) = make.names(colnames(xdtm))

sort(colSums(xdtm),decreasing = T)

#ans: use

#q5
baby_data = cbind(review_rating = review_data$review_rating,xdtm)
sort(colSums(baby_data[baby_data$review_rating == 5,]),decreasing = T)

#ans: love 

#Section 4
#q1
set.seed(1031)
split = sample(1:nrow(baby_data),size = 0.7*nrow(baby_data))
train = baby_data[split,]
test = baby_data[-split,]

#a1: 1494

#q2
library(rpart); library(rpart.plot)
tree = rpart(review_rating~.,train)
rpart.plot(tree)
#a2: true

#q3
#a3: false


#q4
#a4: false

#q6
pred_tree = predict(tree,newdata=test)
rmse_tree = sqrt(mean((pred_tree - test$review_rating)^2)); rmse_tree
#q6: 1.114328

reg = lm(review_rating~.,train)
summary(reg)
#a5: no

#q7
pred_reg = predict(reg, newdata=test)
rmse_reg = sqrt(mean((pred_reg-test$review_rating)^2)); rmse_reg
#a7: 1.11626
