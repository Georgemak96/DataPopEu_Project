start_time <- Sys.time()
library(tidytext)
library(tidyverse)
library(readxl)
library(rio)
library(stats)
library(wordcloud)
library(RColorBrewer)
library(topicmodels)
setwd("C:/Users/User/Desktop/Thesis_Makris")

###data importing
website_data1 <- read_excel("01-06 February.xlsx")
website_data2 <- read_excel("07-13 February.xlsx")
website_data3 <- read_excel("14-20 February.xlsx")
website_data4 <- read_excel("21-27 February.xlsx")
website_data <- bind_rows(website_data1, website_data2, website_data3, website_data4) %>% distinct(urls,.keep_all =  TRUE)


###cleaning (big part of the cleaning is made through the function unnest_tokens())####
stopwords <- read_tsv("stopwords-el-master/stopwords-el.txt")
website_data = website_data %>% mutate(title_enumeration = seq_along(titles))
most_common <- website_data %>% unnest_tokens(Words, articles) %>% count(Words, sort = TRUE) #check to detect garbage
text_cleaned <- website_data %>% unnest_tokens(Words, articles) %>% mutate(garbage = str_detect(Words, "^[A-W]|^[a-w]|\\d+|[:punct:]+")) %>%
  filter(garbage=='FALSE')  %>% select(-garbage) %>% anti_join(stopwords) 
text_cleaned_new <- text_cleaned %>% group_by(titles, urls, sites, title_enumeration) %>% summarise(text = str_c(Words, collapse = " "))

#### SENTIMENT ANALYSIS #####
###greek word sentiment dataset importing#####
sentiments <- read_tsv("greek-sentiment-lexicon-master/greek_sentiment_lexicon.tsv")

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


#library(stringmatch)
#matched_words = data.frame()
#for (i in 1:122405) {
  #stringmatch <- as.data.frame(stringmatch(text_cleaned$Words[i], sentiments$Term))
  #stringmatch <- stringmatch %>% filter((osa <= 1) & (cosine <= 0.25) & (jw <= 0.20) & (pred >= 0.90) & (jaccard <= 0.20))
  #matched_words = bind_rows(matched_words, stringmatch)
#}

###polarity assignment  to website articles
matched_words <- read_excel("matched_words.xlsx")
sentiments_words <- sentiments %>% inner_join(matched_words, by = c("Term" = "Var2")) %>% select(Var1, Avg_Polarity) %>% distinct()
sentiments_words <- text_cleaned %>% inner_join(sentiments_words, by = c("Words" = "Var1")) %>% select(sites, title_enumeration, Words, Avg_Polarity)
sentiments_articles <- sentiments_words %>% group_by(title_enumeration, sites) %>% summarise(Average_Polarity = mean(Avg_Polarity))
sentiments_articles <- sentiments_articles %>% inner_join(website_data) %>% select(title_enumeration, urls, sites, Average_Polarity)





### METHODS TO DETECT POPULISM ######
###SENTIMENT ANALYSIS 
###filtering of articles with negative polarity and random selection of 20
sentiments_neg <- sentiments_articles %>% filter(Average_Polarity < -1.5) ###filtering of the most negative articles
set.seed(1234)
sample <- sentiments_neg %>% ungroup() %>% slice_sample(n = 20)
#export(sample, "Websites Populist Articles.xlsx")
populist_articles_method_1 <- read_excel("Websites Populist Articles.xlsx")
###9 out of 20 are populist articles
populist_articles_method_1 %>% count(populism)
#table <- populist_articles_method_1 %>% left_join(text_cleaned_new)
#export(table, "Method 1 table.xlsx")

###proportion of negative words per site
words_sum <- sentiments_words %>% mutate(polarity_bin=if_else(Avg_Polarity<0,-1,if_else(Avg_Polarity>0,1,0))) %>% count(sites)
words_neg <- sentiments_words %>% mutate(polarity_bin=if_else(Avg_Polarity<0,-1,1)) %>% group_by(sites) %>% count(sites,polarity_bin) %>% filter(polarity_bin==-1)
words_sum %>% left_join(words_neg, by = "sites")%>% filter(sites !="real") %>% mutate(perc=(n.y/n.x)*100) %>%
  mutate(sites=reorder(sites,perc))%>%ggplot(aes(sites,perc)) +
  geom_col(fill="blue")+
  coord_flip()+
  xlab("Ιστοσελίδες") +
  ylab("Ποσοστό (%) αρνητικών λέξεων")+
  theme(text = element_text(size = 13))

###proportions of positive words per site
words_pos <- sentiments_words %>% mutate(polarity_bin=if_else(Avg_Polarity<0,-1,if_else(Avg_Polarity>0,1,0))) %>% group_by(sites) %>% count(sites,polarity_bin) %>% filter(polarity_bin==1)
words_sum %>% left_join(words_pos, by = "sites")%>% filter(sites !="real") %>% mutate(perc=(n.y/n.x)*100) %>%
  mutate(sites=reorder(sites,perc))%>%ggplot(aes(sites,perc)) +
  geom_col(fill="blue")+
  coord_flip()+
  xlab("Ιστοσελίδες") +
  ylab("Ποσοστό (%) θετικών λέξεων")+
  theme(text = element_text(size = 13))

###50 most negative articles
#most_neg_articles <- sentiments_articles %>% arrange(Average_Polarity) %>% ungroup() %>% slice(1:50) %>% left_join(text_cleaned_new) %>%
 # select(-c(title_enumeration,urls,text))
#export(most_neg_articles,"50 Most Negative Articles.xlsx")  
most_neg_articles <- sentiments_articles %>% arrange(Average_Polarity) %>% ungroup() %>% slice(1:50) %>% left_join(text_cleaned_new)

###wordcloud with the 50 most frequent words of the 50 negative articles 
wordcloud <- most_neg_articles %>%
  unnest_tokens(words, text) %>% mutate(words_new = chartr("άέόύήώίϋϊ", "αεουηωιυι", words)) %>%
  count(words_new, sort = TRUE) %>% anti_join(stopwords, by = c("words_new" = "Words"))
wordcloud(wordcloud$words_new, freq = wordcloud$n,scale=c(2,1), min.freq = 1,max.words=50, random.order=FALSE, random.color=FALSE,rot.per=0.20,colors=brewer.pal(8,"Dark2"), fixed.asp=TRUE)


###topic modeling to the 50 most negative articles
top_model <- most_neg_articles %>% 
  unnest_tokens(words, text) %>%
  mutate(Words = chartr("άέόύήώίϋϊ", "αεουηωιυι", words)) %>% anti_join(stopwords, by = "Words")
dtm <- top_model  %>% count(Words,title_enumeration)
dtm <- cast_dtm(dtm, title_enumeration, Words, n)
topic_modeling <- LDA(dtm, k = 4, control = list(seed = 1234))
topics <- tidy(topic_modeling, matrix = "beta")
terms <- topics %>% group_by(topic) %>% top_n(10, beta) %>% ungroup() %>% arrange(topic, -beta)
terms %>% mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip() +
  xlab("Λέξεις") +
  ylab("Πιθανότητες") +
  theme(text = element_text(size = 14))


###50 most positive articles
most_pos_articles <- sentiments_articles %>% arrange(desc(Average_Polarity)) %>% ungroup()%>% slice(1:50) %>% left_join(text_cleaned_new)


###wordcloud with the 50 most frequent words of the positive articles 
wordcloud <- most_pos_articles %>%
  unnest_tokens(words, text) %>% mutate(words_new = chartr("άέόύήώίϋϊ", "αεουηωιυι", words)) %>%
  count(words_new, sort = TRUE) %>% anti_join(stopwords, by = c("words_new" = "Words"))
wordcloud(wordcloud$words_new, freq = wordcloud$n,scale=c(2,0.5), min.freq = 1,max.words=50, random.order=FALSE, random.color=FALSE,rot.per=0,colors=brewer.pal(8,"Dark2"), fixed.asp=TRUE)

###topic modeling to the 50 most positive articles
top_model <- most_pos_articles %>%
  unnest_tokens(words, text) %>%
  mutate(Words = chartr("άέόύήώίϋϊ", "αεουηωιυι", words)) %>% anti_join(stopwords, by = "Words")
dtm <- top_model  %>% count(Words,title_enumeration)
dtm <- cast_dtm(dtm, title_enumeration, Words, n)
topic_modeling <- LDA(dtm, k = 4, control = list(seed = 1234))
topics <- tidy(topic_modeling, matrix = "beta")
terms <- topics %>% group_by(topic) %>% top_n(10, beta) %>% ungroup() %>% arrange(topic, -beta)
terms %>% mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip() +
  xlab("Λέξεις") +
  ylab("Bήτα") +
  theme(text = element_text(size = 14))


###ENEMIES OF THE PEOPLE
#### Detection of the enemies of the people in articles, and random selection of 20 of the articles 
people_enemies <- text_cleaned_new %>%
  mutate(mentions_people_enemies = str_detect(text, "ελιτ|κατεστημ|καναλαρχ|διεφθαρμ|διαφθορ|βρυξελλες"))
set.seed(1234)
people_enemies_sample <- people_enemies %>% ungroup() %>% filter(mentions_people_enemies=="TRUE") %>% slice_sample(n = 20)
#export(people_enemies_sample, "Websites Populist Articles 4.xlsx")
populist_articles_method_4 <- read_excel("Websites Populist Articles 4.xlsx")
###9 out of 20 are populist articles
populist_articles_method_4 %>% count(populism)

###enemies of the people by site
people_enemies_new <- people_enemies %>% group_by(sites) %>% count(mentions_people_enemies) %>% mutate(perc=n/sum(n)) %>% filter(mentions_people_enemies=="TRUE") %>%
  filter(sites != "real") %>% ungroup() %>%mutate(sites=reorder(sites,perc))
ggplot(people_enemies_new, aes(sites, perc)) +
  geom_col(fill="blue")+
  coord_flip()+
  xlab("Ιστοσελίδες") +
  ylab("Ποσοστό (%) λέξεων που υποδηλώνουν εχθρούς του λαού")+
  theme(text = element_text(size = 13))


##combination of sentiment analysis and enemies of the people 
sentiments_enemies <- people_enemies %>% ungroup() %>% left_join(sentiments_articles, by="title_enumeration") %>%
  filter(mentions_people_enemies=="TRUE" & Average_Polarity<0 & str_detect(text,"κυβερνηση|συριζα|ελλαδα|πολιτικη|αντιπολιτευση|μητσοτακης|κομμα|βουλη|τσιπρα|ποσοστο|αντιπολιτευσης")) %>% filter(sites.x!="real")
set.seed(1234)

##random sample of 20 of the the articles which have negative sentiment, refer to the enemies and to the political topic (from now on call them AEΠ articles)
sentiments_enemies_sample <- sentiments_enemies %>% slice_sample(n=20)
#export(sentiments_enemies_sample, "Sentiments Enemies Articles.xlsx")
#export(sentiments_enemies, "Negative Sentiments- MentionEnemies articles.xlsx")
sentiments_enemies_articles <- read_excel("Sentiments Enemies Articles.xlsx")
##10 articles were found populist in a random sample of 20 articles from the combination of methods
sentiments_enemies_articles %>% count(populism)

###AEΠ articles by site
sentiments_enemies %>%mutate(ID=1) %>% right_join(text_cleaned_new, by = "title_enumeration")%>%group_by(sites) %>% count(ID) %>% mutate(perc=n/sum(n)*100) %>%
  filter(ID==1) %>% ungroup() %>%
  mutate(sites=reorder(sites,perc)) %>% ggplot(aes(sites,perc)) +
  coord_flip() +
  geom_col(fill="blue") +
  xlab("Ιστοσελίδες") +
  ylab("Ποσοτό (%) άρθρων (Αρνητικό συναίσθημα, εχθροί λαού και πολιτικός άξονας)") +
  theme(text = element_text(size = 14))

###wordcloud of AEΠ articles
wordcloud <- sentiments_enemies %>%
  unnest_tokens(words, text) %>% mutate(words_new = chartr("άέόύήώίϋϊ", "αεουηωιυι", words)) %>%
  count(words_new, sort = TRUE) %>% anti_join(stopwords, by = c("words_new" = "Words"))
wordcloud(wordcloud$words_new, freq = wordcloud$n,scale=c(3,0), min.freq = 1,max.words=100, random.order=FALSE, random.color=FALSE,rot.per=0.15,colors=brewer.pal(8,"Dark2"), fixed.asp=TRUE)

####topic modeling of the AEΠ articles
top_model <- sentiments_enemies %>%filter(sites.x!="real") %>% filter(mentions_people_enemies=="TRUE")%>%
  unnest_tokens(words, text) %>%
  mutate(Words = chartr("άέόύήώίϋϊ", "αεουηωιυι", words)) %>% anti_join(stopwords, by = "Words")
dtm <- top_model  %>% count(Words,title_enumeration)
dtm <- cast_dtm(dtm, title_enumeration, Words, n)
topic_modeling <- LDA(dtm, k = 5, control = list(seed = 1234))
topics <- tidy(topic_modeling, matrix = "beta")
terms <- topics %>% group_by(topic) %>% top_n(10, beta) %>% ungroup() %>% arrange(topic, -beta)
terms %>% mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip() +
  xlab("Λέξεις") +
  ylab("Πιθανότητες") +
  theme(text = element_text(size = 14))

###topic modeling on the people_enemies articles
top_model <- people_enemies %>%ungroup()%>%filter(sites!="real") %>% filter(mentions_people_enemies=="TRUE")%>%
  unnest_tokens(words, text) %>%
  mutate(Words = chartr("άέόύήώίϋϊ", "αεουηωιυι", words)) %>% anti_join(stopwords, by = "Words")
dtm <- top_model  %>% count(Words,title_enumeration)
dtm <- cast_dtm(dtm, title_enumeration, Words, n)
topic_modeling <- LDA(dtm, k = 2, control = list(seed = 1234))
topics <- tidy(topic_modeling, matrix = "beta")
terms <- topics %>% group_by(topic) %>% top_n(10, beta) %>% ungroup() %>% arrange(topic, -beta)
terms %>% mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip() +
  xlab("Λέξεις") +
  ylab("Πιθανότητες") +
  theme(text = element_text(size = 14))



##### MACHINE LEARNING FOR CLASSIFICATION OF ARTICLES ####
#### importing of manually labelled dataset
#pop_articles <- read_excel("Populist Articles Coded.xlsx")

#### text pre-processing for machine learning
#text_cleaned_new <- text_cleaned %>% group_by(titles, urls, sites, title_enumeration) %>% summarise(text = str_c(Words, collapse = " "))
#pop_articles$populism <- factor(pop_articles$populism, labels = c(0,1))

#text_preprocessing <- function(x) {
  #x <- x %>% inner_join(text_cleaned_new)
  #x <- x %>% unnest_tokens(words, text)
  #pop_articles_new <- x %>% count(title_enumeration, words, sites) %>%
  #inner_join(x)
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
                    #            number = 10)
  #model <- train(populism ~  tf_idf+sites, data = pop_articles_Processed,
    #             trControl = train_control,
   #              method = "glm",family = binomial(link = "logit"))
  
  #results <- model[["results"]]["Accuracy"]
  ###preprocessing of test data
  #test_data <- website_data %>% select(urls, sites, title_enumeration) %>% anti_join(pop_articles)
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
  #sample <- word_predictions_new %>% ungroup %>% slice_sample(n = 50)
  ###human input
  #data.entry(sample)
  #sample <- as.data.frame(sample)
  #sample$populism <- factor(sample$populism, levels = c(0,1))
  #pop_articles <- bind_rows(sample, pop_articles)
#}
#word_predictions_new$populism <- factor(word_predictions_new$populism, levels = c(0,1))
#pop_articles <- bind_rows(word_predictions_new, pop_articles)
#export(pop_articles, "Populist Articles Coded New.xlsx")


##Random sample of 20 articles classified as populist by the algorithm
coded_articles <- read_excel("Populist Articles Coded New.xlsx")
set.seed(1234)
sample <- coded_articles %>% left_join(text_cleaned_new) %>%  filter(populism == "1") %>% group_by(sites)  %>% slice_sample(n = 4)
#export(sample, "Websites Populist Articles 3 .xlsx")
populist_articles_method_3 <- read_excel("Websites Populist Articles 3 .xlsx")
###17 out of 20 are populist articles
populist_articles_method_3 %>% count(populism)


#### POPULIST ARTICLES ANALYSIS ###
### Populist Articles Polarity Distribution
coded_articles <- read_excel("Populist Articles Coded New.xlsx")
populist_articles <- coded_articles %>% filter(populism == 1)
populist_articles <- populist_articles %>% inner_join(text_cleaned_new)
populist_articles <- populist_articles %>% inner_join(sentiments_articles)
p <- ggplot(populist_articles, aes(Average_Polarity)) +
  geom_histogram(binwidth = 0.1, color = "black")
p + xlab("Πόλωση") + ylab("Συχνότητα")  +theme_minimal() + theme(text = element_text(size = 15)) + scale_x_continuous(limits = c(-3.25,3.25))






### Topic Modeling on populist articles
populist_articles <- coded_articles %>% filter(populism == 1)
populist_articles <- populist_articles %>% inner_join(text_cleaned)%>%
  mutate(Words = chartr("άέόύήώίϋϊ", "αεουηωιυι", Words)) %>% anti_join(stopwords)
dtm <- populist_articles  %>% count(Words,title_enumeration)
dtm <- cast_dtm(dtm, title_enumeration, Words, n)
topic_modeling <- LDA(dtm, k =4, control = list(seed = 1234))
topics <- tidy(topic_modeling, matrix = "beta")
terms <- topics %>% group_by(topic) %>% top_n(10, beta) %>% ungroup() %>% arrange(topic, -beta)
terms %>% mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip() +
  xlab("Λέξεις") +
  ylab("Πιθανότητες") +
  theme(text = element_text(size = 14))


### populist articles by site
pop_articles_frequency <- coded_articles %>% inner_join(text_cleaned_new) %>%
  count(populism, sites) %>% mutate(perc = n / sum(n)) %>% filter(populism=="1") %>%
  ggplot(aes(sites, perc)) + 
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  xlab("Ιστοσελίδες") +
  ylab("Σχετική Συχνότητα") +
  ggtitle("Σχετική Συχνότητα λαϊκιστικών άρθρων ανά ιστοσελίδα")

###frequency of populist articles by site
pop_articles_frequency <- coded_articles %>% left_join(text_cleaned_new) %>%
  count(populism, sites) %>% mutate(perc = n / sum(n)) %>% filter(populism=="1")

###wordcloud of populist articles
populist_articles <- coded_articles %>% filter(populism == 1)
wordcloud <- populist_articles %>% inner_join(text_cleaned_new) %>%
  unnest_tokens(words, text) %>% mutate(words_new = chartr("άέόύήώίϋϊ", "αεουηωιυι", words)) %>%
  count(words_new, sort = TRUE) %>% anti_join(stopwords, by = c("words_new" = "Words"))
wordcloud(wordcloud$words_new, wordcloud$n, scale=c(3,0) , max.words = 100, min.freq = 1, random.order=FALSE, rot.per=0.35, colors = brewer.pal(8,"Dark2"))

end_time <- Sys.time()
end_time - start_time
