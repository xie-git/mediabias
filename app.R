library(janeaustenr)
library(dplyr)
library(stringr)
library(data.table)
library(tidyverse)
library(tidytext)
library(Matrix)
library(glmnet)
library(magrittr)
library(shiny)
library(rsconnect)

# system time
Sys.setenv(TZ="America/Indiana/Indianapolis")

# load three files of news articles with source
data.article1 <- read_csv('articles1.csv')
data.article2 <- read_csv('articles2.csv')

# limitations of shiny memory
# data.article3 <- read_csv('articles3.csv')
# data.article <- rbind(data.article1, data.article2, data.article3)
data.article <- rbind(data.article1, data.article2)

# add boolean value to whether each title belongs to a topic
data.article %<>% mutate(Congress = ifelse(grepl('Congress | Congressional',data.article$title), TRUE, FALSE)) %>% 
  mutate(Economy = ifelse(grepl('Economy | Economics', data.article$title), TRUE, FALSE)) %>%
  mutate(Guns = ifelse(grepl(' Shooting | Shot | Gun | Second Amendement | 2nd Amendment | NRA',
                             data.article$title), TRUE, FALSE)) %>% 
  mutate(College = ifelse(grepl('Higher education|college|universit|undergrad|graduat',
                                data.article$title), TRUE, FALSE)) %>% 
  mutate(Republican = ifelse(grepl('Republican|Repub |Repubs |GOP', data.article$title), TRUE , FALSE))  %>%
  mutate(Democrat = ifelse(grepl('Democrat|Dem |Dems ', data.article$title), TRUE, FALSE)) %>%
  mutate(Conservative = ifelse(grepl('Conservative', data.article$title), TRUE, FALSE)) %>% 
  mutate(Liberal = ifelse(grepl('Liberal|Libs|Lib ', data.article$title), TRUE, FALSE)) %>% 
  mutate(Clinton = ifelse(grepl('Clinton ', data.article$title), TRUE, FALSE)) %>% 
  mutate(Obama = ifelse(grepl('Obama', data.article$title), TRUE, FALSE)) %>% 
  mutate(TRUMP = ifelse(grepl('Trump', data.article$title), TRUE, FALSE))


# filter out news titles that do not belong to any of our chosen topics
data.article = data.article[which(apply(data.article[11:21],1,any)),]

# ordered id for each title
data.article$id <- c(1:nrow(data.article))  

text.df <- data_frame(id = data.article$id, text = as.character(data.article$title))

# data frame of Video and single word comment
tokens <- text.df %>% unnest_tokens(word, text)

# make the document-term matrix  (aka the bag-of-words-matrix)
dt.mat = cast_sparse(tokens, id, word)
col.sum = colSums(dt.mat)


# 1 column data frame of all single words commented
Dictionary = data.frame(word = colnames(dt.mat)) %>% as.tbl

# separating words and topics and creating sparse matrix of sentiments
s.dic = sentiments$word %>% table %>% sort %>% names %>% rev
s.desc = sentiments$sentiment %>% table %>% sort %>% names %>% rev
s.desc = c(NA,s.desc)
temp = sparseMatrix(i = match(sentiments$word, s.dic),
                    j = match(sentiments$sentiment, s.desc))
s.desc[1] = "NA"
sent.matrix = as.matrix(temp)
colnames(sent.matrix) = s.desc

# add sentiment boolean for each word into matrix
sent.df = data.frame(word = s.dic, sent.matrix) %>% as.tbl %>% 
  left_join(get_sentiments("afinn")) %>% left_join(get_sentiments("bing"))
sent.df[is.na(sent.df)] = 0
sent.df$sentiment[sent.df$sentiment=="positive"] = 1
sent.df$sentiment[sent.df$sentiment=="negative"] = -1
sent.df$sentiment = as.numeric(sent.df$sentiment)

# all words in dictionary that have a sentiment value
feelings = left_join(Dictionary, sent.df)

#matrix with goodwords as columns, comment id as row
good.words = which(rowSums(is.na(feelings)) == 0)
x = dt.mat[,good.words] 
feelings = feelings[good.words,]

# words in comment i with sentiment j
sx = x %*% as.matrix(feelings[,-1])
sx = as.matrix(sx) 

# numeric sentiment analysis
data.sentiment <- as.data.frame(sx)
data.sentiment$id <- c(1:nrow(data.sentiment))
data.sentiment$abs_score <- abs(data.sentiment$score)
data.sentiment %<>% select(id, sentiment, score, abs_score, everything())

# join to article id
data <- left_join(data.sentiment, data.article)
data %<>% arrange(date)

# plots of score and sentiment
# we see that average scores are close to 0, slightly skewed positive
ggplot(data, aes(x=score)) + geom_bar() 
ggplot(data, aes(x=sentiment)) + geom_bar() 

# filtering data for articles by five major news sources
data5 <- data %>% filter(publication == "Fox News"|publication == "Breitbart"|publication == "NPR"|
                           publication == "Washington Post"|publication == 'CNN')

###### GRAPHS ######

#master data
#dat = data5[,c(1, 2, 3, 21, 22, 33:39)]
#saveRDS(dat, file = "masterData.rds")
#dat <- readRDS('grandData.rds')
dat <- read_csv('finalData.csv')
dat <- dat[,-1]
#write.csv(dat, file = "finalData.csv")

# sentiment analysis by news site
cnn = dat %>% filter(publication == "CNN")
fox = dat %>% filter(publication == "Fox News")
breit = dat %>% filter(publication == "Breitbart")
npr = dat %>% filter(publication == "NPR")
wapo = dat %>% filter(publication == "Washington Post")

# Republican|Democrat|Conservative|Liberal|Clinton|Obama|TRUMP

topics = c("Republican", "Democrat", "Conservative", "Liberal", "Clinton", "Obama", "TRUMP")
news.sites = c("CNN", "Fox News", "Breitbart", "NPR", "Washington Post")
tm = c("Clinton Investigation", "Shootings/Gun Control", 'Illegal Immigratoin', 'GOP Primary', 
       'Democratic Primary', 'Middle East / Islam', 'Obamacare', 'Economic Policy')

total.articles = c(1:length(news.sites))
for(i in 1:length(news.sites)){
  total.articles[i] = length(which(data.article$publication == news.sites[i]))
}

total.articles = data.frame(news.sites, total.articles)
allnews = list()
allnews[[1]] = cnn
allnews[[2]] = fox
allnews[[3]] = breit
allnews[[4]] = npr
allnews[[5]] = wapo

avgs = data.frame()

for(j in 1:length(allnews)) {
  for(i in 1:length(topics)) {
    avgs[i,j] = mean(filter(allnews[[j]], allnews[[j]][, i + 5] == TRUE)$sentiment)
  }
}

# average of topics by news site
average = data.frame(topics, avgs)
colnames(average) = c("topic",news.sites)

# sentiment analysis by topic

republican = filter(dat, Republican == TRUE)
democrat = filter(dat, Democrat == TRUE)
conservative = filter(dat, Conservative == TRUE)
liberal = filter(dat, Liberal == TRUE)
clinton = filter(dat, Clinton == TRUE)
obama = filter(dat, Obama == TRUE)
trump = filter(dat, TRUMP == TRUE)

alltopics = list()
alltopics[[1]] = republican
alltopics[[2]] = democrat
alltopics[[3]] = conservative
alltopics[[4]] = liberal
alltopics[[5]] = clinton
alltopics[[6]] = obama
alltopics[[7]] = trump

avgst = data.frame()

for(j in 1:length(topics)){
  for(i in 1:length(news.sites)) {
    avgst[i,j] = mean(filter(alltopics[[j]], alltopics[[j]][, 5] == news.sites[i])$sentiment)
  }
}

# matrix of sentiment of topics by source
averaget = data.frame(news.sites, avgst)
colnames(averaget) = c("source", topics)

#topic coverage
coverage = data.frame()

for(j in 1:length(topics)){
  for(i in 1:length(news.sites)) {
    coverage[i,j] = length(which(allnews[[i]][j + 5] == TRUE)) / as.numeric(total.articles[i,2])
  }
}

colnames(coverage) = topics
coverage.pct = data.frame(news.sites, coverage)

tcoverage = data.frame()
for(j in 1:length(topics)){
  for(i in 1:length(news.sites)) {
    tcoverage[j,i] = length(which(allnews[[i]][j + 5] == TRUE)) / as.numeric(total.articles[i,2])
  }
}

colnames(tcoverage) = news.sites
tcoverage.pct = data.frame(topics, tcoverage)

average[7,2:6] <- average[7,2:6] - 1

#sources
cnndata = data.frame(topics, average$CNN, tcoverage.pct$CNN)
breitdata = data.frame(topics, average$Breitbart, tcoverage.pct$Breitbart)
nprdata = data.frame(topics, average$NPR, tcoverage.pct$NPR)
wapodata = data.frame(topics, average$`Washington Post`, tcoverage.pct$Washington.Post)
foxdata = data.frame(topics, average$`Fox News`, tcoverage.pct$Fox.News)


#topics
repdata = data.frame(news.sites, averaget$Republican, coverage.pct$Republican)
demdata = data.frame(news.sites, averaget$Democrat, coverage.pct$Democrat)
clintondata = data.frame(news.sites, averaget$Clinton,coverage.pct$Clinton)
libdata = data.frame(news.sites, averaget$Liberal, coverage.pct$Liberal)
obamadata = data.frame(news.sites, averaget$Obama, coverage.pct$Obama)
trumpdata = data.frame(news.sites, averaget$TRUMP - 1, coverage.pct$TRUMP)
consdata = data.frame(news.sites, averaget$Conservative, coverage.pct$Conservative)

# change column names
colnames(repdata) = c("X1", "X2", "X3")
colnames(demdata) = c("X1", "X2", "X3")
colnames(clintondata) = c("X1", "X2", "X3")
colnames(libdata) = c("X1", "X2", "X3")
colnames(obamadata) = c("X1", "X2", "X3")
colnames(trumpdata) = c("X1", "X2", "X3")
colnames(consdata) = c("X1", "X2", "X3")

colnames(cnndata) = c("X1", "X2", "X3")
colnames(breitdata) = c("X1", "X2", "X3")
colnames(nprdata) = c("X1", "X2", "X3")
colnames(wapodata) = c("X1", "X2", "X3")
colnames(foxdata) = c("X1", "X2", "X3")

###### topic modeling topics #######

topics = c("Republican", "Democrat", "Conservative", "Liberal", "Clinton", "Obama", "TRUMP")
news.sites = c("CNN", "Fox News", "Breitbart", "NPR", "Washington Post")
tm = c("Clinton Investigation", "Shootings/Gun Control", 'Illegal Immigration', 'GOP Primary', 
       'Democratic Primary', 'Obamacare', 'Economic Policy')

total.articles = c(1:length(news.sites))
for(i in 1:length(news.sites)){
  total.articles[i] = length(which(data.article$publication == news.sites[i]))
}

total.articles = data.frame(news.sites, total.articles)
allnews = list()
allnews[[1]] = cnn
allnews[[2]] = fox
allnews[[3]] = breit
allnews[[4]] = npr
allnews[[5]] = wapo

avgs = data.frame()

for(j in 1:length(allnews)) {
  for(i in 1:length(tm)) {
    avgs[i,j] = mean(filter(allnews[[j]], allnews[[j]][, i + 5] == TRUE)$sentiment)
  }
}

average.tm = data.frame(tm, avgs)
colnames(average.tm) = c("topic",news.sites)

# sentiment analysis by topic
clinton.investigation = filter(dat, V1 == "Clinton Investigation")
guns = filter(dat, V1 == "Shootings/Gun Control")
immigration = filter(dat, V1 == "Illegal Immigration")
gop.primary = filter(dat, V1 == "GOP Primary")
dem.primary = filter(dat, V1 == "Democratic Primary")
obamacare = filter(dat, V1 == "Obamacare")
econ = filter(dat, V1 == "Economic Policy")

alltm = list()
alltm[[1]] = clinton.investigation
alltm[[2]] = guns
alltm[[3]] = immigration
alltm[[4]] = gop.primary
alltm[[5]] = dem.primary
alltm[[6]] = obamacare
alltm[[7]] = econ

avgst.tm = data.frame()

for(j in 1:length(tm)){
  for(i in 1:length(news.sites)) {
    avgst.tm[i,j] = mean(filter(alltm[[j]], alltm[[j]][, 5] == news.sites[i])$sentiment)
  }
}
averaget.tm = data.frame(news.sites, avgst.tm)
colnames(averaget.tm) = c("source", tm)


#topic coverage

total.articles #total number of articles from each source
coverage.tm = data.frame()

for(j in 1:length(tm)){
  for(i in 1:length(news.sites)) {
    coverage.tm[i,j] = length(which(alltm[[i]][j + 5] == TRUE)) / as.numeric(total.articles[i,2])
  }
}

colnames(coverage.tm) = tm
coverage.pct.tm = data.frame(news.sites, coverage.tm)

tcoverage.tm = data.frame()
for(j in 1:length(tm)){
  for(i in 1:length(news.sites)) {
    tcoverage.tm[j,i] = length(which(alltm[[i]][j + 5] == TRUE)) / as.numeric(total.articles[i,2])
  }
}

colnames(tcoverage.tm) = news.sites
tcoverage.pct.tm = data.frame(tm, tcoverage.tm)

#sources
cnndata.tm = data.frame(tm, average.tm$CNN, tcoverage.pct.tm$CNN)
breitdata.tm = data.frame(tm, average.tm$Breitbart, tcoverage.pct.tm$Breitbart)
nprdata.tm = data.frame(tm, average.tm$NPR, tcoverage.pct.tm$NPR)
wapodata.tm = data.frame(tm, average.tm$`Washington Post`, tcoverage.pct.tm$Washington.Post)
foxdata.tm = data.frame(tm, average.tm$`Fox News`, tcoverage.pct.tm$Fox.News)


#topics

clinton.inv.data = data.frame(news.sites, averaget.tm$`Clinton Investigation`, coverage.pct.tm$Clinton.Investigation)
guns.data = data.frame(news.sites, averaget.tm$`Shootings/Gun Control`, coverage.pct.tm$Shootings.Gun.Control)
immigration.data = data.frame(news.sites, averaget.tm$`Illegal Immigration`,coverage.pct.tm$Illegal.Immigration)
gop.primary.data = data.frame(news.sites, averaget.tm$`GOP Primary`, coverage.pct.tm$GOP.Primary)
dem.primary.data = data.frame(news.sites, averaget.tm$`Democratic Primary`, coverage.pct.tm$Democratic.Primary)
obamacare.data = data.frame(news.sites, averaget.tm$Obamacare, coverage.pct.tm$Obamacare)
econ.data = data.frame(news.sites, averaget.tm$`Economic Policy`, coverage.pct.tm$Economic.Policy)

# change column names
colnames(clinton.inv.data) = c("X1", "X2", "X3")
colnames(guns.data) = c("X1", "X2", "X3")
colnames(immigration.data) = c("X1", "X2", "X3")
colnames(gop.primary.data) = c("X1", "X2", "X3")
colnames(dem.primary.data) = c("X1", "X2", "X3")
colnames(obamacare.data) = c("X1", "X2", "X3")
colnames(econ.data) = c("X1", "X2", "X3")


# Define UI for application that draws a histogram
ui <- navbarPage("Bias in the media",
                 
                 
                 tabPanel("Project Overview",
                          titlePanel("About the Project"),
                          HTML('By: Michael Xie <br> May 2018 <br> Source Code:'),
                          sidebarLayout(position = 'right',
                                        sidebarPanel(
                                          plotOutput('topicPlot0'),
                                          "Distribution graph of the sentiment scores of each article title. The average score was 0.339, which denotes a positive sentiment."
                                          
                                        ),
                                        
                                        mainPanel(
                                          
                                          
                                          
                                          h3("Introduction"),
                                          
                                          "Bias in the media has always been a topic of hot-debate throughout American history but has especially been a point of focus in recent years. Depending on whom you ask, you'll get very different responses about the political leanings of news organizations.  Because of the subjective nature of this topic, we wanted to find a way to analyze bias in the media in an objective manner.  We decided to do this using sentiment analysis on news article titles and examining which topics news sources were covering, which we'll discuss more thoroughly below. Although our method has its limitations and is far from perfect, it still offers us useful insight into the political biases of five major news outlets.", 
                                          
                                          h3("The data"),
                                          
                                          "The original data set contained information on 143,000 news articles including information such as the publication source, publication date, article title, and the article content. The articles ranged from 2012 to 2017, but the majority fell between 2016 and 2017.  Because we're attempting to identify political bias, we only wanted to look at articles on polarizing topics. We filtered down the data set to articles containing the words Trump, Clinton, Obama, Conservative, Liberal, Republican, Democrat, or similar derivations of these words (e.g.  Dems).  The data was then further filtered down from 15 news organizations to articles from only NPR, Fox News, CNN, Breitbart, and the Washington Post. These news organizations were selected because they offered the most abundant data, are fairly popular, and also seemed to cover the political spectrum according to our preconceived notions. In the end, there were 20,453 news articles used for the analysis. The original data was obtained from Kaggle and can be found", a(href = "https://www.kaggle.com/snapcrack/all-the-news/data", "here"), 
                                          
                                          h3("Methods"),
                                          
                                          "There are two ways which we analyzed media bias.",
                                          
                                          h4("Relative Topic Coverage"),
                                          
                                          "Which stories a news organization chooses to cover or not cover can be a telling sign of bias. The top graph on each tab displays the relative coverage of each topic. These percentages were simply calculated by dividing the number of articles in a topic by the total number of articles we analyzed.", 
                                          
                                          h4("Sentiment Analysis"),
                                          
                                          "Sometimes described as opinion mining, sentiment analysis is defined as the process of computationally identifying and categorizing opinions expressed in a piece of text, especially in order to determine whether the writer's attitude towards a particular topic, product, etc., is positive, negative, or neutral. More about sentiment analysis can be found", a(href="https://en.wikipedia.org/wiki/Sentiment_analysis", "here"), ".  We used the 'bing' lexicon from the 'tidytext' package in R, which assigns binary values to a word depending on its sentiment (i.e. 1 is positive, 0 is neutral, -1 is negative). After removing unnecessary words, we ran sentiment analysis on all of the article titles. In the end, each title was given a score by adding the respective values of each word in that title. These scores provide an estimate to whether an article title had a positive or negative tone.  The results are displayed in the second graph on each tab with green representing positive sentiment and red representing negative sentiment.", 
                                          
                                          h3("Topic Modeling"),
                                          
                                          "In order to explore the data further, we used",a(href = "https://en.wikipedia.org/wiki/Latent_Dirichlet_allocation", "Latent Dirichlet Allocation"), "to sort the 20,453 articles into new topics by looking at the articles' full content.  We used the variational expectation-maximization (VEM) algorithm as the method for fitting the models (more about VEM",a(href="https://cran.r-project.org/web/packages/topicmodels/vignettes/topicmodels.pdf", "here"),".  After playing around with different values of k (the # of predetermined topics LDA will sort the data into), we chose the model with 28 different topics. Not every topic was useful, but there were a handful of topics that effectively grouped articles about specific political issues.  For example, by looking at the titles, it was clear that articles assigned to topic 7 focused on the GOP Primary and articles in topic 25 were about the Hillary Clinton investigation. The topic titles were created in this fashion (i.e. summarizing the titles in our own words). The results for these topics are displayed in the last two tabs.", 
                                          
                                          h3("Limitations of the Analysis"),
                                          
                                          "There are key limitations to our analysis that are important to note. For starters, sentiment analysis isn't perfect and should only serve as a rough guide for the true sentiment of a piece of text. It can struggle to interpret the context. For example, it can't detect sarcasm and would likely give the opposite score that it should in this scenario.",
                                          
                                          br(), p("Secondly, the sentiment of an article title is not necessarily indicative of bias. Put in other terms, negative sentiment doesn't necessarily equal negative coverage. An easy example would an article titled Trump Speaks Out about Bombings and Terror Attacks. This article would score very negative because of its keywords but isn't painting Trump in a negative light. The sentiment scores should not be taken as definitive answers to the political leanings of certain news organizations, but rather serve as an insight that needs further exploration."), 
                                          
                                          p("Lastly, the data set had issues as well. Despite starting with 143,000 articles, there were certain topics and directions we couldn't explore because we lacked the data.  We would have liked to explore other news sources as well, but the data became surprisingly sparse after filtering it down. The publication dates were a cause for concern too. The data ranged from 2012-2017, but the articles and publication sources were not evenly spread throughout this range. For example, Breitbart had a disproportionately high number of articles from 2012, whereas other publications didn't have any.  Not only is this troublesome because the time variable may have impacted our results, but it also prevented us from exploring the effects of time on media coverage. Analyzing the change in sentiment and coverage over time could offered even more interesting results. The project could have been improved with higher quality data."), 
                                          
                                          p("It's important to keep these limitations in mind before drawing hard conclusions from our results.") 
                                          
                                          
                                          
                                          
                                        ) 
                          )
                 ),
                 
                 tabPanel("By Topic",
                          sidebarPanel(
                            selectInput(inputId ="dataset1", label='Choose a topic.',
                                        choices =c('Republican','Democrat','Conservative','Liberal','Clinton','Obama','TRUMP'))
                          ),
                          mainPanel(
                            plotOutput('topicPlot2'),
                            plotOutput('topicPlot1')
                            
                            
                          )
                 )
                 ,
                 tabPanel("By Source",
                          sidebarPanel(
                            selectInput(inputId ="dataset2", label='Choose a topic.',
                                        choices = news.sites)
                          )
                          ,
                          mainPanel(
                            
                            plotOutput('topicPlot4'),
                            plotOutput('topicPlot3')
                            
                          )
                 )
                 ,
                 tabPanel("By Topic Modeling",
                          sidebarPanel(
                            selectInput(inputId ="dataset3", label='Choose a topic.',
                                        choices = c("Clinton Investigation", "Shootings/Gun Control",
                                                    "Illegal Immigration","GOP Primary", "Democratic Primary",
                                                    "Obamacare", "Economic Policy" ))
                          )
                          ,
                          mainPanel(
                            plotOutput('topicPlot6'),
                            plotOutput('topicPlot5')
                            
                            
                          )
                 )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  datasetInput1 <- reactive({
    switch(input$dataset1,
           'Republican'= repdata,
           'Democrat'=demdata,
           'Conservative'=consdata,
           'Liberal'=libdata,
           'Clinton'=clintondata,
           'Obama'=obamadata,
           'TRUMP'=trumpdata)
  })
  datasetInput2 <- reactive({
    switch(input$dataset2,
           'CNN'= cnndata,
           'Breitbart'=breitdata,
           'Fox News'=foxdata,
           'NPR'=nprdata,
           'Washington Post'=wapodata)
  })
  
  datasetInput3 <- reactive({
    switch(input$dataset3,
           "Clinton Investigation" = clinton.inv.data,
           "Shootings/Gun Control" = guns.data,
           "Illegal Immigration" = immigration.data,
           "GOP Primary" = gop.primary.data,
           "Democratic Primary"  = dem.primary.data,
           "Obamacare"  = obamacare.data,
           "Economic Policy" = econ.data)
  })
  
  
  output$topicPlot0 <- renderPlot({
    ggplot(dat, aes(x=sentiment)) + geom_bar()+ labs(title="Sentiment Score Distribution", x = "Sentiment Score", y = "Number of Articles")
  })
  output$topicPlot1 <- renderPlot({
    dataset=datasetInput1()
    dataset$colour <- ifelse(dataset$X2 < 0, "negative","positive")
    ggplot(dataset, aes(x=X1,y = X2)) +geom_bar(stat = "identity",width = 0.6, position = position_dodge(width = 10.0), aes(fill=colour))+ scale_x_discrete(labels = abbreviate)+theme(legend.title=element_blank()) +labs(x = "", y="",subtitle ='sentiment')+theme(plot.title = element_text(hjust = 0.5)) +
      scale_fill_manual(values=c(positive="darkgreen",negative="darkred")) + guides(fill=FALSE)
    
  })
  output$topicPlot2 <- renderPlot({
    dataset=datasetInput1()
    ggplot(dataset, aes(x=X1,y = X3*100)) +geom_bar(stat = "identity",width = 0.6, position = position_dodge(width = 10.0))+ scale_x_discrete(labels = abbreviate)+theme(legend.title=element_blank()) +labs(x = "", y="",subtitle ='% of total # of each publication\'s news')+theme(plot.title = element_text(hjust = 0.5))
    
  })
  
  output$topicPlot3 <- renderPlot({
    dataset=datasetInput2()
    dataset$colour <- ifelse(dataset$X2 < 0, "negative","positive")
    ggplot(dataset, aes(x=X1,y = X2)) +geom_bar(stat = "identity",width = 0.6, position = position_dodge(width = 10.0), aes(fill=colour))+ scale_x_discrete(labels = abbreviate)+theme(legend.title=element_blank()) +labs(x = "", y="",subtitle ='sentiment')+theme(plot.title = element_text(hjust = 0.5)) +
      scale_fill_manual(values=c(positive="darkgreen",negative="darkred")) + guides(fill=FALSE)
    
  })
  output$topicPlot4 <- renderPlot({
    dataset=datasetInput2()
    ggplot(dataset, aes(x=X1,y = X3*100)) +geom_bar(stat = "identity",width = 0.6, position = position_dodge(width = 10.0))+ scale_x_discrete(labels = abbreviate)+theme(legend.title=element_blank()) +labs(x = "", y="",subtitle ='% of total # of each publication\'s news')+theme(plot.title = element_text(hjust = 0.5))
    
  })
  
  
  output$topicPlot5 <- renderPlot({
    dataset=datasetInput3()
    dataset$colour <- ifelse(dataset$X2 < 0, "negative","positive")
    ggplot(dataset, aes(x=X1,y = X2)) +geom_bar(stat = "identity",width = 0.6, position = position_dodge(width = 10.0), aes(fill=colour))+ scale_x_discrete(labels = abbreviate)+theme(legend.title=element_blank()) +labs(x = "", y="",subtitle ='sentiment')+theme(plot.title = element_text(hjust = 0.5)) +
      scale_fill_manual(values=c(positive="darkgreen",negative="darkred")) + guides(fill=FALSE)
    
  })
  output$topicPlot6 <- renderPlot({
    dataset=datasetInput3()
    ggplot(dataset, aes(x=X1,y = X3*100)) +geom_bar(stat = "identity",width = 0.6, position = position_dodge(width = 10.0))+ scale_x_discrete(labels = abbreviate)+theme(legend.title=element_blank()) +labs(x = "", y="",subtitle ='% of total # of each publication\'s news')+theme(plot.title = element_text(hjust = 0.5))
    
  })
}
# Run the application 
shinyApp(ui = ui, server = server)

