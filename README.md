# Sentiment Analysis of Bias in Major Media Outlets

Authors: Michael Xie, Dan Kojis

Created: May 2018

## Introduction

Bias in the media has always been a topic of hot-debate throughout American history but has especially been a point of focus in recent years. Depending on whom you ask, you'll get very different responses about the political leanings of news organizations. Because of the subjective nature of this topic, we wanted to find a way to analyze bias in the media in an objective manner. We decided to do this using sentiment analysis on news article titles and examining which topics news sources were covering, which we'll discuss more thoroughly below. Although our method has its limitations and is far from perfect, it still offers us useful insight into the political biases of five major news outlets.

## Data

The original data set contained information on 143,000 news articles including information such as the publication source, publication date, article title, and the article content. The articles ranged from 2012 to 2017, but the majority fell between 2016 and 2017. Because we're attempting to identify political bias, we only wanted to look at articles on polarizing topics. We filtered down the data set to articles containing the words Trump, Clinton, Obama, Conservative, Liberal, Republican, Democrat, or similar derivations of these words (e.g. Dems). The data was then further filtered down from 15 news organizations to articles from only NPR, Fox News, CNN, Breitbart, and the Washington Post. These news organizations were selected because they offered the most abundant data, are fairly popular, and also seemed to cover the political spectrum according to our preconceived notions. In the end, there were 20,453 news articles used for the analysis. The original data was obtained from Kaggle

## Methods

### Relative Topic Coverage

Which stories a news organization chooses to cover or not cover can be a telling sign of bias. The top graph on each tab displays the relative coverage of each topic. These percentages were simply calculated by dividing the number of articles in a topic by the total number of articles we analyzed.

### Sentiment Analysis

Sometimes described as opinion mining, sentiment analysis is defined as the process of computationally identifying and categorizing opinions expressed in a piece of text, especially in order to determine whether the writer's attitude towards a particular topic, product, etc., is positive, negative, or neutral. More about sentiment analysis can be found here . We used the 'bing' lexicon from the 'tidytext' package in R, which assigns binary values to a word depending on its sentiment (i.e. 1 is positive, 0 is neutral, -1 is negative). After removing unnecessary words, we ran sentiment analysis on all of the article titles. In the end, each title was given a score by adding the respective values of each word in that title. These scores provide an estimate to whether an article title had a positive or negative tone. The results are displayed in the second graph on each tab with green representing positive sentiment and red representing negative sentiment.

## Topic Modeling

In order to explore the data further, we used Latent Dirichlet Allocation to sort the 20,453 articles into new topics by looking at the articles' full content. We used the variational expectation-maximization (VEM) algorithm as the method for fitting the models (more about VEM here . After playing around with different values of k (the # of predetermined topics LDA will sort the data into), we chose the model with 28 different topics. Not every topic was useful, but there were a handful of topics that effectively grouped articles about specific political issues. For example, by looking at the titles, it was clear that articles assigned to topic 7 focused on the GOP Primary and articles in topic 25 were about the Hillary Clinton investigation. The topic titles were created in this fashion (i.e. summarizing the titles in our own words). The results for these topics are displayed in the last two tabs.

## Limitations

There are key limitations to our analysis that are important to note. For starters, sentiment analysis isn't perfect and should only serve as a rough guide for the true sentiment of a piece of text. It can struggle to interpret the context. For example, it can't detect sarcasm and would likely give the opposite score that it should in this scenario. 
Secondly, the sentiment of an article title is not necessarily indicative of bias. Put in other terms, negative sentiment doesn't necessarily equal negative coverage. An easy example would an article titled Trump Speaks Out about Bombings and Terror Attacks. This article would score very negative because of its keywords but isn't painting Trump in a negative light. The sentiment scores should not be taken as definitive answers to the political leanings of certain news organizations, but rather serve as an insight that needs further exploration.

Lastly, the data set had issues as well. Despite starting with 143,000 articles, there were certain topics and directions we couldn't explore because we lacked the data. We would have liked to explore other news sources as well, but the data became surprisingly sparse after filtering it down. The publication dates were a cause for concern too. The data ranged from 2012-2017, but the articles and publication sources were not evenly spread throughout this range. For example, Breitbart had a disproportionately high number of articles from 2012, whereas other publications didn't have any. Not only is this troublesome because the time variable may have impacted our results, but it also prevented us from exploring the effects of time on media coverage. Analyzing the change in sentiment and coverage over time could offered even more interesting results. The project could have been improved with higher quality data.

It's important to keep these limitations in mind before drawing hard conclusions from our results.
