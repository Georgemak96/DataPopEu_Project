###The following script is used to scrape from kathimerini, avgi, efsyn, ta nea, vima, real, protothema
###This script was used daily for one month (February 2022). In general there was not changes or updates at the sites explored. However, this can always be the case
###when dealing with web scraping and that's why there needs to be constant checks from the user.
###In this script during the data collection period, there was not changes to the sites' html code, but usual errors such as 404 errors etc, happened. These were handled using 
###the base try function, and the flawed html paths were removed manually.
###Each chunk of code for each site is separated by several spaces from the previous one and the next one
###For the most effective and fast detection of the right html paths, google css selector was used.
#### In each site 1) there is a scraping of all links from the navigation menu, 2) each of these links is opened  and all the contained links are scraped too
### 3) each of these links, in turn, are opened and all the content is scraped from them (title and article)
### The above three sequential steps are indicated in the code for kathimerini that follows, and the same thing goes along for the other sites as well.


library(tidyverse)
library(rvest)
library(rio)
library(readxl)
Sys.setlocale("LC_CTYPE", "greek")

kathimerini<- read_html("https://www.kathimerini.gr/")

####scraping of all links from the navigation menu
kathimerini_urls<- kathimerini %>% html_nodes("#menu-main-menu a") %>% html_attr("href") 
kathimerini_urls_unique<- unique(kathimerini_urls) 
pinakas<- data.frame(kathimerini_urls_unique) %>% slice(-1) %>% filter(str_detect(kathimerini_urls_unique,"https://www.kathimerini.gr/."))

### opening of the links from the navigation menu, scraping of all the links contained in them and storing of them at a character vector named urls.
urls<-c()
for (i in pinakas$kathimerini_urls_unique) {
  table1 <- read_html(i)
  links<- table1%>% html_nodes(".article-item a") %>% html_attr("href")
  urls=append(urls,links)
}
kathimerini_urls<- data.frame(urls)%>% filter(str_detect(urls,"https://www.kathimerini.gr/."))%>% filter(!str_detect(urls, "https://www.kathimerini.gr/culture/music/561666289/the-rolling-stones-%cf%83%cf%84%ce%b7-%ce%b4%ce%b7%ce%bc%ce%bf%cf%83%ce%b9%cf%8c%cf%84%ce%b7%cf%84%ce%b1-%ce%b1%ce%ba%cf%85%ce%ba%ce%bb%ce%bf%cf%86%cf%8c%cf%81%ce%b7%cf%84%ce%bf-%cf%85%ce%bb%ce%b9/"))%>% filter(!str_detect(urls, "https://www.kathimerini.gr/culture/agenda/561665044/%ce%b7-%cf%80%ce%bf%ce%bb%ce%b9%cf%84%ce%b9%cf%83%cf%84%ce%b9%ce%ba%ce%ae-%ce%b1%cf%84%ce%b6%ce%ad%ce%bd%cf%84%ce%b1-%cf%84%ce%b7%cf%82-%cf%84%ce%b5%cf%84%ce%ac%cf%81%cf%84%ce%b7%cf%82/"))%>% filter(!str_detect(urls, "https://www.kathimerini.gr/culture/agenda/561665044/%ce%b7-%cf%80%ce%bf%ce%bb%ce%b9%cf%84%ce%b9%cf%83%cf%84%ce%b9%ce%ba%ce%ae-%ce%b1%cf%84%ce%b6%ce%ad%ce%bd%cf%84%ce%b1-%cf%84%ce%b7%cf%82-%cf%84%ce%b5%cf%84%ce%ac%cf%81%cf%84%ce%b7%cf%82/"))%>% filter(!str_detect(urls, "https://www.kathimerini.gr/society/561660031/%ce%b5%ce%b4%cf%8e-%ce%b7-%ce%ba%ce%b1%cf%81%ce%b4%ce%b9%ce%ac-%cf%84%ce%bf%cf%85-%ce%b5%ce%bb%ce%bb%ce%b7%ce%bd%ce%b9%cf%83%ce%bc%ce%bf%cf%8d-%cf%87%cf%84%cf%85%cf%80%ce%ac%ce%b5%ce%b9-%cf%80/")) %>% unique()

###opening of the links from the urls character vector and scraping of the content inside them. 
### Also there is an exception handling with the try keyword, for links with errors e.g. 404 error and so on. The sites with errors are manually removed. 
titles<-c()
articles<-c()
for(i in kathimerini_urls$urls){ try({ 
  pinakas_2 <- read_html(i)
  title<- pinakas_2%>% html_node(".entry-title") %>% html_text()
  title<- str_squish(title)
  titles=append(titles,title)
  body <- pinakas_2 %>%
    html_node(".entry-content") %>%
    html_text()
  text<-str_squish(body)
  articles = append(articles, text)}, silent = T)
  
}
kathimerini_content<- data.frame(titles,articles,kathimerini_urls)

###filtering of articles which contain the words people and popul in the following pattern : " ΛΑΟ| ΛΑΌ|ΛΑΪΚ|ΛΑΈ|ΛΑΏΝ"
kathimerini_content2<- kathimerini_content  %>%  mutate(upper=str_to_upper(articles))%>% mutate(populism=str_detect(upper, " ΛΑΟ| ΛΑΌ|ΛΑΪΚ|ΛΑΈ|ΛΑΏΝ"))%>% filter(populism=="TRUE") %>% select(-populism,-upper)%>% export("Kathimerini.xlsx")







avgi<- read_html("https://www.avgi.gr/")
avgi_urls<- avgi %>% html_nodes(".expanded-menu__section--gallery a , .expanded-menu__title span , .expanded-menu__section--categories a , .extended-menu__author a") %>% html_attr("href") 
avgi_urls<-  paste0("https://www.avgi.gr",avgi_urls) %>% as.data.frame() %>% filter(!str_detect(., "https://www.avgi.grNA"))
pinakas<-as.data.frame(avgi_urls) %>% unique()

urls<-c()
for (i in pinakas$.) {
  table1 <- read_html(i)
  links<- table1%>% html_nodes(".category-promo article a, .horizontal-teasers a") %>% html_attr("href")
  urls=append(urls,links)
  
}

avgi_paste<-paste0("https://www.avgi.gr",urls) %>% as.data.frame() %>% unique()


titles<- c()
articles<-c()
for(i in avgi_paste$.){try({ 
  pinakas_2 <- read_html(i)
  title<- pinakas_2%>% html_node(".double-title")%>% html_text()
  title<- str_squish(title)
  titles=append(titles,title)
  body <- pinakas_2 %>% html_node(".article__main-content") %>% html_text()
  text<- str_squish(body)
  articles = append(articles, text)},silent = T)
}
avgi_final<- data.frame(titles,articles,avgi_paste) %>% rename(urls=".")
avgi_final2<- avgi_final  %>%  mutate(upper=str_to_upper(articles))%>% mutate(populism=str_detect(upper, " ΛΑΟ| ΛΑΌ|ΛΑΪΚ|ΛΑΈ|ΛΑΏΝ"))%>% filter(populism=="TRUE")%>% select(-populism,-upper)%>% export("Avgi.xlsx")







efsyn<- read_html("https://www.efsyn.gr/")
efsyn_urls<- efsyn %>% html_nodes(".container--nav, li a") %>% html_attr("href") 
efsyn_urls<- paste0("https://www.efsyn.gr",efsyn_urls) %>% as.data.frame()
efsyn_urls <- efsyn_urls  %>% filter(str_detect(.,"https://www.efsyn.gr/.")) 
pinakas<-as.data.frame(efsyn_urls)

urls<-c()
for (i in pinakas$.) {
  table1 <- read_html(i)
  links<- table1%>% html_nodes(".main__content .full-link") %>% html_attr("href")
  urls=append(urls,links)
  
}

efsyn_paste<-paste0("https://www.efsyn.gr",urls)
efsyn_content<-as.data.frame(efsyn_paste) %>% unique()

titles<- c()
articles<-c()
for(i in efsyn_content$efsyn_paste){try({
  pinakas_2 <- read_html(i)
  title<- pinakas_2%>% html_node("h1")%>% html_text()
  titles=append(titles,title)
  body <- pinakas_2 %>% html_node(".article__main") %>% html_text()
  text<- str_squish(body)
  articles = append(articles, text)}, silent = T)
}
efsyn_final<- data.frame(titles,articles,efsyn_content)
efsyn_final2<- efsyn_final  %>%  mutate(upper=str_to_upper(articles))%>% mutate(populism=str_detect(upper, " ΛΑΟ| ΛΑΌ|ΛΑΪΚ|ΛΑΈ|ΛΑΏΝ"))%>% filter(populism=="TRUE")%>% select(-populism,-upper) %>% export("Efsyn.xlsx")






nea<- read_html("https://www.tanea.gr/")
nea_urls<- nea%>% html_nodes(".teal-c , .purple-c , .navyblue-c , .brown-c , .red-c , .orange-c , .blue-c , #more-nav a") %>% html_attr("href")
nea_urls<- paste0("https://www.tanea.gr",nea_urls) %>% as.data.frame()

urls<-c()
for (i in nea_urls$.) {
  table1 <- read_html(i)
  links<- table1%>% html_nodes(".nodecor") %>% html_attr("href")
  urls=append(urls,links)
  
}

tanea_urls<- data.frame(urls)%>% filter(str_detect(urls,"https://www.tanea.gr/.")) %>% unique()

titles<-c()
articles<-c()
for(i in tanea_urls$urls){try({ 
  pinakas_2 <- read_html(i)
  title<- pinakas_2%>% html_node(".black-c") %>% html_text()
  title<- str_squish(title)
  titles=append(titles,title)
  body <- pinakas_2 %>%
    html_node(".article-wrapper") %>%
    html_text()
  text<-str_squish(body)
  articles = append(articles, text)},silent = T)
  
}
tanea_final<- data.frame(titles,articles,tanea_urls)
tanea_final2<- tanea_final  %>%  mutate(upper=str_to_upper(articles))%>% mutate(populism=str_detect(upper, " ΛΑΟ| ΛΑΌ|ΛΑΪΚ|ΛΑΈ|ΛΑΏΝ"))%>% filter(populism=="TRUE")%>% select(-populism,-upper) %>% export("Ta Nea.xlsx")









tovima<- read_html("https://www.tovima.gr/")
urls_tovima <- tovima %>% html_nodes(".simpletext a") %>% html_attr("href")
urls_tovima<- paste0("https://www.tovima.gr",urls_tovima)
pinakas <- data.frame(urls_tovima) %>% slice(-1)

urls<-c()
for (i in pinakas$urls_tovima) {
  table1 <- read_html(i)
  links<- table1%>% html_nodes(".article-main") %>% html_attr("href")
  urls=append(urls,links)
  
}

tovima_content<- data.frame(urls) %>% filter(str_detect(urls, "https://www.tovima.gr/."))%>% unique()

titles<- c()
articles<-c()
for(i in tovima_content$urls){try({ 
  pinakas_2 <- read_html(i)
  title<- pinakas_2%>% html_node(".zonabold")%>% html_text()
  titles=append(titles,title)
  body <- pinakas_2 %>% html_node(".mainpost") %>% html_text()
  text<- str_squish(body)
  articles = append(articles, text)}, silent = T)
}
tovima_final<- data.frame(titles,articles,tovima_content)
tovima_final2<- tovima_final  %>%  mutate(upper=str_to_upper(articles))%>% mutate(populism=str_detect(upper, " ΛΑΟ| ΛΑΌ|ΛΑΪΚ|ΛΑΈ|ΛΑΏΝ"))%>% filter(populism=="TRUE")%>% select(-populism,-upper) %>% export("To Vima.xlsx")







real <- read_html("https://www.real.gr/")
urls_real<- real %>% html_nodes(".btn-home~ li a") %>% html_attr("href")
urls_real<- paste0("https://www.real.gr",urls_real )
pinakas <- data.frame(urls_real)


urls<-c()
for (i in pinakas$urls_real) {
  table1 <- read_html(i)
  links<- table1%>% html_nodes("h2 a") %>% html_attr("href")
  urls=append(urls,links)
  
}
new_urls <- paste0("https://www.real.gr", urls)
real_content<- data.frame(new_urls) %>% unique()

titles<-c()
articles<-c()
for(i in real_content$new_urls){try({ 
  pinakas_2 <- read_html(i)
  title<- pinakas_2%>% html_node(".post-title") %>% html_text()
  titles=append(titles,title)
  body <- pinakas_2 %>%
    html_nodes(".leftcol") %>%
    html_text()
  text<-str_squish(body)
  articles = append(articles, text)}, silent = T)
  
}

real_final <- data.frame(titles,articles,real_content) 
real_final2<- real_final  %>%  mutate(upper=str_to_upper(articles))%>% mutate(populism=str_detect(upper, " ΛΑΟ| ΛΑΌ|ΛΑΪΚ|ΛΑΈ|ΛΑΏΝ"))%>% filter(populism=="TRUE")%>% select(-populism,-upper) %>% export("Real.xlsx")







protothema<- read_html("https://www.protothema.gr/")
protothema_urls<- protothema %>% html_nodes(".topNav a") %>% html_attr("href")
pinakas <- data.frame(protothema_urls) 


urls<-c()
for (i in pinakas$protothema_urls) {
  table1 <- read_html(i)
  links<- table1%>% html_nodes(".heading a") %>% html_attr("href")
  urls=append(urls,links)
  
}
protothema_urls<- data.frame(urls)%>% filter(str_detect(urls,"https://www.protothema.gr/.")) %>% unique()

titles<-c()
articles<-c()
for(i in protothema_urls$urls){try({ 
  pinakas_2 <- read_html(i)
  title<- pinakas_2%>% html_node("h1") %>% html_text()
  title<- str_squish(title)
  titles=append(titles,title)
  body <- pinakas_2 %>%
    html_node(".details") %>%
    html_text()
  text<-str_squish(body)
  articles = append(articles, text)}, silent = T)
  
}

protothema_final <- data.frame(titles,articles,protothema_urls) 
protothema_final2<- protothema_final  %>%  mutate(upper=str_to_upper(articles))%>% mutate(populism=str_detect(upper, " ΛΑΟ| ΛΑΌ|ΛΑΪΚ|ΛΑΈ|ΛΑΏΝ"))%>% filter(populism=="TRUE")%>% select(-populism,-upper) %>% export("Protothema.xlsx")
