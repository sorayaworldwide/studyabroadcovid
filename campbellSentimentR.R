install.packages("vader")
library(dplyr)
library(vader)
library(tidyverse)
library(readr)
library(textdata)
library(rtweet)
library(writexl)
library(readxl)
library(textdata)
library(ggplot2)
library(scales)
library(tidytext)
library(twitteR)
install.packages(twitteR)
##store api keys
app_name <-"sentimentcovidintled"
api_key <-"NrLRwiMnpknvbMEwYXxB2Gw4A"
api_secret_key <-"meLIhMf9skmIbRUx0UssNtrYszWgKPU5pvCRxKEqBWKSMCYEPR"
access_token <-"293694037-bMnCGVK8wuEWsnksAaB7wpIYIdJAoNKmwWwJsP2q"
access_token_secret <-"MgqZXwR3RYTzU7kRL2Jipozy4d4hHcf7kswiEdwmxa2YW"
##authenticate
token<-create_token(
  app =app_name,
  consumer_key = api_key,
  consumer_secret=api_secret_key,
  access_token = access_token, 
  access_secret = access_token_secret)
##check token
get_token()
##import tweets
intled_covid_all <-search_tweets2(c("#intled", "#studyabroad OR studyabroad", "#covid19 OR covid19"),
                                  n=5000)
intled_covid_all
##import tweets - use this 1035 instances after filter for covid 
intled_covid_all2 <-search_tweets2(c('"study abroad"', "#covid19 OR covid"), n=1000, lang="en")
intled_covid_all2
table(intled_covid_all2$query)
## import tweets - test 3; best results after filtering for covid 16133 instances 
intled_covid_all3 <-search_tweets2(c("#studyabroad OR studyabroad", "#covid19 OR covid19"),
                                  n=5000, lang = "en", include_rts=FALSE, retryonratelimit=TRUE)
#import tweets - test 4
intled_covid_all4 <-search_tweets2(c("#studyabroad OR study abroad", "#covid19 OR covid-19"),
                                   n=5000, lang = "en", include_rts=FALSE, retryonratelimit=TRUE)
#import tweets - test 5
intled_covid_all5 <-search_tweets2(c('"study abroad"', "#covid19 OR covid"), n=5000, lang="en", 
                                   include_rts=FALSE, retryonratelimit=TRUE)
##save to excel
write_xlsx(intled_covid_all2, "intled_covid.xlsx")
table(intled_covid_all3$query)
##clean data using intled_covid_all2
intled_covid_text<-intled_covid_all2 %>%
  filter(lang=="en")%>%
  select(screen_name, created_at, text)
intled_covid_text<-filter(intled_covid_text, grepl('covid', ignore.case=TRUE, text))
intled_covid_tokens<-intled_covid_text%>%
  unnest_tokens(output=word,
                input=text,
                token="tweets")
##remove stop words
intled_covid_tidy<-intled_covid_tokens%>%
  anti_join(stop_words, by="word")
##count frequent words
count(intled_covid_tidy, word, sort=TRUE)
## redo 
study_covid <-c("'study abroad' AND #covid19 OR covid", 
                "#studyabroad AND #covid19 OR covid",
                '"study abroad" AND #covid19', 
                "'study abroad' AND covid",
                "#studyabroad AND covid",
                "#studyabroad AND #covid19")
##redo query
covidabroadtweets2 <-search_tweets2(study_covid, n=5000, include_rts=FALSE, lang="en", retryonratelimit=TRUE)
##try another dictionary
study_covid2<-c("study abroad AND covid")
##
covidabroadtweets6<-search_tweets2(study_covid2, n=5000, include_rts=FALSE, lang="en", retryonratelimit=TRUE)
##try filter %>%
filter(intled_covid_all5, )
##try study_covid again
covidabroadtweets1<-search_tweets2(study_covid, n=5000, include_rts=FALSE, lang="en", retryonratelimit=TRUE)
study_covid3<-c("#studyabroad AND covid19",
                "#studyabroad AND covid",
                '"study abroad" AND covid',
                '"study abroad" AND #covid19')
search_tweets2(study_covid3, n=5000, include_rts=FALSE, lang="en")
##try again
study_covid4<-c("'study abroad' AND covid")
covidabroad5<-search_tweets2(study_covid4, n=1000, include_rts=FALSE, lang="en")
##try again
study_covid5<-c("study abroad AND covid")

search_tweets2(study_covid5, include_rts=FALSE, lang="en")
studyabroadtweets<-search_tweets("'study abroad'", n=5000, include_rts=FALSE, lang="en")
studyabroadcovidtwt<-studyabroadtweets%>%
  filter(stringr::str_detect(text, 'covid|#covid19|covid19'))
intledresults<-search_tweets(q="#intled", include_rts=FALSE, lang="en")
higheredresults<-search_tweets("#highered", include_rts=FALSE, lang="en")
search_tweets("covid", include_rts=FALSE, lang="en")
study_covid1 <-c("'study abroad' #covid OR covid",
                "'study abroad' #covid19 OR covid19",
                "#studyabroad #covid19 OR covid19",
                "#studyabroad #covid OR covid",
                '"study abroad" #covid19',
                "'study abroad' covid" ,
                "#studyabroad covid",
                "#studyabroad #covid19",
                "#studyabroad COVID-19",
                "'study abroad' COVID-19",
                "coronavirus #studyabroad",
                "coronavirus 'study abroad'",
                "#overseaseducation #covid19 OR covid19",
                "#Study_Abroad #covid19 OR covid19", 
                "#intled #covid19 OR covid19")
studyabroadtwts <-search_tweets2(study_covid1, include_rts=FALSE, lang="en")
search_tweets2(q="trump", include_rts=FALSE, lang="en")
##write to xls
write_xlsx(studyabroadtwts, "studyabroadtwts.xlsx")
##unnest tokens
studyabroad_tokens<-studyabroadtwts%>%
  select(screen_name, created_at, text)%>%
  unnest_tokens(output=word,
                input = text,
                token ="tweets")
##remove columns and stop words
tidy_studyabroad <-studyabroad_tokens%>%
  anti_join(stop_words, by="word")%>%
  filter(!word=="amp")
##count words
count(tidy_studyabroad, word, sort=TRUE)
##use vader
studyabroad_vader<-studyabroadtwts%>%
  select(screen_name, created_at, text)
##vader
vader_abroad <-vader_df(studyabroad_vader$text)%>%
  select(text, compound, pos, neg, neu)
##results
vader_results<-vader_abroad%>%
  mutate(sentiment = ifelse(compound > 0, "positive",
                            ifelse(compound <0, "negative", "neutral")))%>%
  count(sentiment, sort=TRUE)%>%
  spread(sentiment, n)%>%
  relocate(positive)
vader_results
##vader results for ggplot
vader_resultsgg<-vader_abroad%>%
  mutate(sentiment = ifelse(compound > 0, "positive",
                            ifelse(compound <0, "negative", "neutral")))%>%
  count(sentiment, sort=TRUE)
##ggplot
library(forcats)
Vaderbarchart<-vader_resultsgg%>%
  ggplot(aes(fct_reorder(sentiment, n), n))+
  geom_col()+
  labs(x="Vader Sentiment Value", y="Vader Sentiment Score", title="Study Abroad and Covid Twitter Data")
##add sentiment values
nrc <-get_sentiments("nrc")
##join
sentiment_nrc<-inner_join(tidy_studyabroad, nrc, by="word")
##ts
tsplottweets<-ts_plot(studyabroadtwts, by="days")
##summary
summary_nrc<-count(sentiment_nrc, sentiment, sort=TRUE)
summary_nrc
nrc_bar<-summary_nrc%>%
  ggplot(aes(reorder(sentiment, n), n))+
  geom_col()+
  coord_flip()+
labs(x="NRC Sentiment", y="n", title="Study Abroad and Covid")
nrc_bar
##spread nrc
nrc_results<-summary_nrc%>%
  spread(sentiment, n)%>%
  relocate(positive)
nrc_results
#TOP tweets vader
positiveTweet <- vader_abroad %>% 
  filter(compound > 0.799) %>%
  mutate(tweet = text) %>% 
  sample_n(3) %>%
  select(tweet)
positiveTweet
#negative tweets vader
negativeTweet <-vader_abroad%>%
  filter(compound < -0.67)%>%
  mutate(tweet = text) %>% 
  sample_n(3) %>%
  select(tweet)
negativeTweet
