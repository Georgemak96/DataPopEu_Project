library(tidytext)
library(tidyverse)
library(readxl)
library(rio)
library(stats)
setwd("C:/Users/User/Desktop/Paper New")

###data importing
website_data1 <- read_excel("C:/Users/User/Desktop/Thesis_Makris/01-06 February.xlsx")
website_data2 <- read_excel("C:/Users/User/Desktop/Thesis_Makris/07-13 February.xlsx")
website_data3 <- read_excel("C:/Users/User/Desktop/Thesis_Makris/14-20 February.xlsx")
website_data4 <- read_excel("C:/Users/User/Desktop/Thesis_Makris/21-27 February.xlsx")
website_data <- bind_rows(website_data1, website_data2, website_data3, website_data4) %>% distinct(urls,.keep_all =  TRUE)

###cleaning (big part of the cleaning is made through the function unnest_tokens())####
stopwords <- read_tsv("C:/Users/User/Desktop/Thesis_Makris/stopwords-el-master/stopwords-el.txt")
website_data = website_data %>% mutate(title_enumeration = seq_along(titles))
most_common <- website_data %>% unnest_tokens(Words, articles) %>% count(Words, sort = TRUE) #check to detect garbage
text_cleaned <- website_data %>% unnest_tokens(Words, articles) %>% mutate(garbage = str_detect(Words, "^[A-W]|^[a-w]|\\d+|[:punct:]+")) %>%
  filter(garbage=='FALSE')  %>% select(-garbage) %>% anti_join(stopwords) 

#### SENTIMENT ANALYSIS #####
###greek word sentiment dataset importing#####
sentiments <- read_tsv("C:/Users/User/Desktop/Thesis_Makris/greek-sentiment-lexicon-master/greek_sentiment_lexicon.tsv")

###Sentiments dataset contains four polarity columns (4 evaluators) for each greek word. Each column have values "POS","NEG" and "N/A".
###Below i created a function for reassignment of polarity values to a scale of -1(negative) to 1(positive) and then i created a column
###with the average polarity for each word
polarity_value_reassigning <- function(x) {
  x <- x %>% mutate(Polarity5  = if_else(str_detect(Polarity1, "POS"), 1, 
                                         if_else(str_detect(Polarity1, "NEG"), -1, 
                                                 if_else(str_detect(Polarity1, "N/A"), 0, 0)))) %>%
    mutate(Polarity6 = if_else(str_detect(Polarity2, "POS"), 1, 
                               if_else(str_detect(Polarity2, "NEG"), -1, 
                                       if_else(str_detect(Polarity2, "N/A"), 0, 0)))) %>%
    mutate(Polarity7 = if_else(str_detect(Polarity3, "POS"), 1, 
                               if_else(str_detect(Polarity3, "NEG"), -1, 
                                       if_else(str_detect(Polarity3, "N/A"), 0, 0)))) %>%
    mutate(Polarity8 = if_else(str_detect(Polarity4, "POS"), 1, 
                               if_else(str_detect(Polarity4, "NEG"), -1, 
                                       if_else(str_detect(Polarity4, "N/A"), 0, 0))))
}
sentiments <- polarity_value_reassigning(sentiments) 
sentiments <- sentiments %>% mutate(Avg_Polarity = Polarity5 + Polarity6 + Polarity7 + Polarity8 / 4)

###οι λέξεις στο sentiments dataset είναι περιοριστικές γιατί είναι μόνο στο πρώτο πρόσωπο της οριστικής ενεστώτα αν είναι ρήμα (π.χ. κάνω) και
###μόνο στο αρσενικό γένος αν είναι επίθετο (π.χ. καλός) και μόνο στον ενικό αν είναι ουσιαστικό (π.χ. το πράγμα)
###So in order to optimize the matching between the article words and the sentiments words i used some string matching indexes as well as predictions
###from a baseline model which come up as single function (stringmatch()) in the stringmatch package of Kaufman and Klevs. The follow chunck of code takes 
###plenty of time to execute so i run it and stored the results in an excel file (matched_words.xlsx) in order to easily become available
#library(stringmatch)
#matched_words = data.frame()
#for (i in 1:122405) {
  #stringmatch <- as.data.frame(stringmatch(text_cleaned$Words[i], sentiments$Term))
  #stringmatch <- stringmatch %>% filter((osa <= 1) & (cosine <= 0.25) & (jw <= 0.20) & (pred >= 0.90) & (jaccard <= 0.20))
  #matched_words = bind_rows(matched_words, stringmatch)
#}

###polarity assignment  to website articles
matched_words <- read_excel("C:/Users/User/Desktop/Thesis_Makris/matched_words.xlsx")
sentiments_words <- sentiments %>% inner_join(matched_words, by = c("Term" = "Var2")) %>% select(Var1, Avg_Polarity) %>% distinct()
sentiments_words <- text_cleaned %>% inner_join(sentiments_words, by = c("Words" = "Var1")) %>% select(sites, title_enumeration, Words, Avg_Polarity)
sentiments_articles <- sentiments_words %>% group_by(title_enumeration, sites) %>% summarise(Average_Polarity = mean(Avg_Polarity))
sentiments_articles <- sentiments_articles %>% inner_join(website_data) %>% select(title_enumeration, urls, sites, Average_Polarity)


##populism <- Tweets_February %>% mutate(upper = str_to_upper(text)) %>% filter(str_detect(upper, "??????|????????"))
###Only 24 Tweets did contain the words people, popul which is very few. so I joined them with website data (all of which contain the words people and popul) on the common urls 
###in order, at least, to detect which urls lead to articles containing the words people and popul
###This resulted in a common dataset of 527 common urls
Tweets_February <- read_excel("C:/Users/User/Desktop/Paper New/News websites tweets-February.xlsx") ####all of them
common_urls <- website_data %>% inner_join(Tweets_February, by = c("urls" = "expanded_url")) %>% distinct(urls, titles,.keep_all = TRUE) 

### METHODS TO DETECT POPULISM ######
###first method to detect populism (filtering of articles with negative polarity and random selection of 10)
sentiments_neg <- sentiments_articles %>% inner_join(common_urls) %>% filter(Average_Polarity <= -1 & Average_Polarity >= -3.25) ###filtering of the most negative articles
set.seed(1234)
sample <- sentiments_neg %>% ungroup() %>% slice_sample(n = 10)
data.entry(sample)
#export(sample, "Websites-Tweets Populist Articles.xlsx")
populist_articles_method_1 <- read_excel("Websites-Tweets Populist Articles.xlsx")
###5 out of 10 are populist articles
populist_articles_method_1 %>% count(populism)

#### second method to detect populism (Frequency of terms people and popul in articles, and random selection of 10 of the articles with the highest frequency)
populism_freq <- common_urls %>%
  unnest_tokens(Words, articles) %>%
  mutate(upper = str_to_upper(Words)) %>%
  filter(str_detect(upper, "ΛΑΟ|ΛΑ??Κ")) %>%
  mutate(populism = str_extract(upper, "ΛΑΟ|ΛΑ??Κ")) %>%
  group_by(title_enumeration, urls) %>%
  arrange(desc(title_enumeration)) %>% 
  count(populism) %>% 
  summarise(sum = sum(n)) %>%
  filter(sum>=5)
set.seed(1234)
sample <- populism_freq %>% ungroup() %>% slice_sample(n = 10)
#export(sample, "Websites-Tweets Populist Articles 2.xlsx")
populist_articles_method_2 <- read_excel("Websites-Tweets Populist Articles 2.xlsx")
###4 out of 10 are populist articles
populist_articles_method_2 %>% count(populism)

### seems like both methods have similar results and can be used alternatively or simultaneously



##### MACHINE LEARNING FOR CLASSIFICATION OF ARTICLES (HUMAN-IN-THE-LOOP ALGORITHM) ####
#### importing of manually labelled dataset
#pop_articles <- read_excel("C:/Users/User/Desktop/Paper New/Human Coded Articles.xlsx")

#### text pre-processing for machine learning
#text_cleaned_new <- text_cleaned %>% group_by(titles, urls, sites, title_enumeration) %>% summarise(text = str_c(Words, collapse = " "))
#pop_articles$populism <- factor(pop_articles$populism, levels = c("FALSE", "TRUE"), labels = c(0,1))

#text_preprocessing <- function(x) {
  #x <- x %>% inner_join(text_cleaned_new)
  #x <- x %>% unnest_tokens(words, text)
  # <- x %>% count(title_enumeration, words, sites) %>%
  #  inner_join(x)
  #pop_articles_new <- pop_articles_new %>% mutate(ID = as.character(title_enumeration))
  #pop_articles_new <- pop_articles_new %>% bind_tf_idf(words, ID, n )
  #pop_articles_new <- pop_articles_new %>% select(title_enumeration,sites,words,urls,populism,tf_idf)
  #return(pop_articles_new)
#}


#library(caret)
#x = TRUE
#while (x == TRUE) {
  
  #pop_articles_Processed <- text_preprocessing(pop_articles)
  ### evaluation of the initial model on k = 10 folds of the training set
  #train_control <- trainControl(method = "cv",
                            #    number = 10)
  #model <- train(populism ~  tf_idf+sites, data = pop_articles_Processed,
                # trControl = train_control,
                # method = "glm",family = binomial(link = "logit"))
  
  #results <- model[["results"]]["Accuracy"]
  ###preprocessing of test data
  #test_data <- common_urls %>% select(urls, sites, title_enumeration) %>% anti_join(pop_articles)
  #test_data <- test_data %>% filter(sites != "real")
  #new <- test_data %>% mutate(populism = NA)
  #new$populism <- as.factor(new$populism)
  #new_Processed <- text_preprocessing(new)
  ### training
  #model <- glm(populism ~  tf_idf+sites, data = pop_articles_Processed, family = binomial(link = "logit"))
  ### testing
  #predictions <- predict(model, new_Processed, type = "response") %>% tidy()
  #word_predictions <- cbind(new_Processed, predictions$x) %>% arrange((desc(predictions$x)))
  #word_predictions$populism <- if_else(word_predictions$`predictions$x` >= 0.50, 1,0)
  #word_predictions_new <- word_predictions %>% group_by(title_enumeration, urls) %>%
    #summarise(populism = mean(populism))
  #word_predictions_new$populism <- if_else(word_predictions_new$populism <0.5, 0, 1)
  #if (results$Accuracy>=0.85) {
   # x = FALSE}### continue loop until accuracy threshold reaches 0.85 or more. Then stop
  #sample <- word_predictions_new %>% ungroup %>% slice_sample(n = 10)
  ###human input
  #data.entry(sample)
  #sample <- as.data.frame(sample)
  #sample$populism <- factor(sample$populism, levels = c(0,1))
  #pop_articles <- bind_rows(sample, pop_articles)
#}
#word_predictions_new$populism <- factor(word_predictions_new$populism, levels = c(0,1))
#pop_articles <- bind_rows(word_predictions_new, pop_articles)
#export(pop_articles, "C:/Users/User/Desktop/Paper New/Populist Articles Coded.xlsx")



###third method to detect populism (machine learning. Random sample of 10 articles classified as populist)
coded_articles <- read_excel("C:/Users/User/Desktop/Paper New/Populist Articles Coded.xlsx")
set.seed(1234)
sample <- coded_articles %>% filter(populism == "1") %>%  slice_sample(n = 10)
#export(sample, "Websites-Tweets Populist Articles 3.xlsx")
populist_articles_method_3 <- read_excel("Websites-Tweets Populist Articles 3.xlsx")
###8 out of 10 are populist articles
populist_articles_method_3 %>% count(populism)

###seems like machine learning is the best of the three methods


#### POPULIST ARTICLES ANALYSIS ###
### Populist Articles Polarity Distribution
coded_articles <- read_excel("C:/Users/User/Desktop/Thesis_Makris/Populist Articles Coded.xlsx")
populist_articles <- coded_articles %>% filter(populism == 1)
text_cleaned_new <- text_cleaned %>% group_by(titles, urls, sites, title_enumeration) %>% summarise(text = str_c(Words, collapse = " "))
populist_articles <- populist_articles %>% inner_join(text_cleaned_new)
populist_articles <- populist_articles %>% inner_join(sentiments_articles)
p <- ggplot(populist_articles, aes(Average_Polarity)) +
  geom_histogram(binwidth = 0.1, color = "black")
p + scale_x_continuous(limits = c(-1,1)) +
xlab("Sentiment Scores") + ylab("Frequency of Sentiment Scores") +  
theme_bw()

### Topic Modeling
populist_articles <- coded_articles %>% filter(populism == 1)
populist_articles <- populist_articles %>% inner_join(text_cleaned) %>%
  mutate(Words = chartr("άέόύήώίϋϊ", "αεουηωιυι", Words)) %>% anti_join(stopwords)
dtm <- populist_articles  %>% count(Words,title_enumeration)
dtm <- cast_dtm(dtm, title_enumeration, Words, n)
library(topicmodels)
topic_modeling <- LDA(dtm, k = 2, control = list(seed = 1234))
topics <- tidy(topic_modeling, matrix = "beta")
terms <- topics %>% group_by(topic) %>% top_n(40, beta) %>% ungroup() %>% arrange(topic, -beta)
terms %>% mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip() +
  xlab("") +
  ylab("Per-topic-per-word probabilities")


### Some visuals for the populist articles
populist_articles <- coded_articles %>% filter(populism == 1)
pop_articles_frequency <- coded_articles %>% inner_join(text_cleaned_new) %>%
  count(populism, sites) %>% mutate(perc = n / sum(n)) %>% filter(populism == "1") %>%
  ggplot(aes(sites, perc)) + 
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  xlab("News Sites") +
  ylab("Relative Frequency of populist articles") +
  theme(text = element_text(size = 15))

wordcloud <- populist_articles %>% inner_join(text_cleaned_new) %>%
  unnest_tokens(words, text) %>% mutate(words_new = chartr("άέόύήώίϋϊ", "αεουηωιυι", words)) %>%
  count(words_new, sort = TRUE) %>% slice(-c(1:3)) %>% anti_join(stopwords, by = c("words_new" = "Words"))
library(wordcloud)
library(RColorBrewer)
wordcloud(wordcloud$words_new, wordcloud$n, scale=c(3,0) , max.words = 100, min.freq = 1, random.order=FALSE, rot.per=0.30, colors = brewer.pal(8,"Dark2"))

### comparison of data sources
common_urls <- website_data %>% inner_join(Tweets_February, by = c("urls" = "expanded_url")) %>% distinct(urls, titles,.keep_all = TRUE) %>%
  filter(sites != "real")
populist_articles_tweets <- populist_articles %>% inner_join(common_urls)
populist_articles_tweets <- populist_articles_tweets %>% unnest_tokens(words, text)
most_common <- populist_articles_tweets  %>% count(words, sort = TRUE) #check to detect garbage
text_tweets_cleaned <- populist_articles_tweets  %>% mutate(garbage = str_detect(words, "^[A-W]|^[a-w]|\\d+|[:punct:]+")) %>%
  filter(garbage=='FALSE') %>% mutate(words_new = chartr("άέόύήώίϋϊ", "αεουηωιυι", words))%>% anti_join(stopwords, by = c("words_new" = "Words")) 

tweets_word_count <- text_tweets_cleaned %>% count(words_new)
wordcloud(tweets_word_count$words_new, tweets_word_count$n, scale=c(3,0), max.words = 100,min.freq = 1, random.order=FALSE, rot.per=0.30, colors = brewer.pal(8,"Dark2"))


