# UTD Twitter Analysis

api_key <- 'b4EhLodNEODyGLgecE70zGeh9'
api_secret <- 'nsGR6erNhD2AuTiBC46FyeTEgA0SJQMbBLYvKsklH9OsWlSoEn'
access_token <- '3154815030-f39RZK9SZnNvDX3nD91E32eFrk0ZPQDxfsPElsi'
access_token_secret <- 'nd0irw4Xu97QQcDY8LbnvK5faIMFY27VhQvKcEDCNfeRc'

library(sqldf)
library(twitteR)
library(corpus)
library(dplyr)
lapply(c('twitteR', 'dplyr', 'ggplot2', 'lubridate', 'network', 'sna', 'qdap', 'tm'),
       library, character.only = TRUE)
detach("package:qdap", unload=TRUE)
setup_twitter_oauth(api_key,api_secret,access_token,
                    access_token_secret)


# get twitter in excel
tweets <- searchTwitter('@UT_Dallas', n=300, resultType = "recent", lang = 'en')
head(tweets)
#str(tweets)

tweetsdf <- twListToDF(tweets)
#str(tweetsdf)

class(tweetsdf$created)
tweetsdf$newcreate <- tweetsdf$created
class(tweetsdf$newcreate)
mday(tweetsdf$newcreate)

#******* EXTRACT TIME FROM THE POSIXct
# t <- format(tweetsdf$newcreate, "%H:%M:%S")
# head(t)




# tweetsdf$created <- as.character.Date(tweetsdf$created)
# str(tweetsdf$created)
# mday(tweetsdf$created)
# head(mday(tweetsdf$created))

write.csv(tweetsdf, file = 'C:/UTD/Sem1/BA with R/Project/UTD Twitter Analysis/UT_Dallas Tweets.csv',
          row.names=F)

#trend locations
trend <- availableTrendLocations()
head(trend)
trend
filterTrend <- filter(trend,trend$name %in% 'Bangalore')
filterTrend


#world wide trend
world <- getTrends(1)
head(world)

#boston
boston <- getTrends(2367105)
head(boston)

#delhi
delhi <- getTrends(20070458)
head(delhi)


#Bangalore
banglore <- getTrends(2295420)
head(banglore)


#user timeline
recentUTD <- getUser('Josh')
recentUTD
userTimeline(recentUTD,n=2)



#############################################
# utd <- read.csv(file.choose(), header = T)
# head(utd)
# 
# ###***** STRUCTURE OF DATA
# str(utd)
# head(tweetsdf)
# head(utd)

library(tm)
utd_corpus <- iconv(tweetsdf$text, to ='utf-8')
utd_corpus <- Corpus(VectorSource(utd_corpus))
inspect(utd_corpus[1:5])


#clean
utd_corpus <-  tm_map(utd_corpus, tolower)


utd_corpus <- tm_map(utd_corpus, removePunctuation)



utd_corpus <- tm_map(utd_corpus, removeNumbers)



cleanset <- tm_map(utd_corpus, removeWords, stopwords('english'))
inspect(utd_corpus[1:5])

removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))

cleanset <- tm_map(cleanset, removeWords, c('UTD', 'UT_Dallas', 'dallas','utdallas'))
cleanset <- tm_map(cleanset, removeWords, c('dallas'))
cleanset <- tm_map(cleanset, stripWhitespace)


# ##########
# filterUTD <- filter(utd,utd$text %in% 'dallas')
# filterUTD

#Term document matrix 
tdm <- TermDocumentMatrix(cleanset)
tdm


tdm <- as.matrix(tdm)
tdm[1:10, 1:20]


#Bar plot
w <- rowSums(tdm)
w <- subset(w, w>=15)

barplot(w,
        las=2,
        col = rainbow(50))

#word Cloud
library(wordcloud)
w <- sort(rowSums(tdm), decreasing = T)
set.seed(222)
wordcloud(words = names(w), 
          freq = w,
          max.words = 2000,
          random.order = F,
          min.freq = 5,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5,0.3),
          rot.per = 0.7)


library(wordcloud2)
w <- data.frame(names(w), w)
colnames(w) <- c('word', 'freq')

wordcloud2(w,
           size = 0.8,
           shape='cirlce', #triangle, star
           rotateRatio = 0.5,
           minSize = 1)

letterCloud(w,
            word = "A",
            size = 0.8)


letterCloud(w,
            word="UTD",
            size=0.7)


#sentiment
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)



tweets <- iconv(tweetsdf$text, to='utf-8')


# obtain sentiment scores
s <- get_nrc_sentiment(tweets)
head(tweets)
head(s)

tweets[4]

get_nrc_sentiment('talkin mess you gonna let him do that to you?')
get_nrc_sentiment('I am happy for you!')

get_nrc_sentiment('ugly')
get_nrc_sentiment('exhausted')

barplot(colSums(s),
        las=2,
        col = rainbow(10),
        ylab = 'count',
        main = 'Sentiment Scores of UTD Tweets')




################
lapply(c('twitteR', 'dplyr', 'ggplot2', 'lubridate', 'network', 'sna', 'qdap', 'tm'),
       library, character.only = TRUE)



# Put in local time
tweetsdf$created = with_tz(as.Date(tweetsdf$created), 'America/Los_Angeles')
head(tweetsdf$created)


timeDist = ggplot(tweetsdf, aes(created)) + 
  geom_density(aes(fill = isRetweet), alpha = .5) +
  scale_fill_discrete(guide = 'none') +
  xlab('All tweets')

# Zoom in on conference day

# str(tweetsdf$created)
# head(tweetsdf$created)
# mday(tweetsdf$created)


dayOf = filter(tweetsdf, mday(newcreate) == 18)
timeDistDayOf = ggplot(dayOf, aes(newcreate)) + 
  geom_density(aes(fill = isRetweet), adjust = .25, alpha = .5) +
  theme(legend.justification = c(1, 1), legend.position = c(1, 1)) +
  xlab('Day-of tweets')
cowplot::plot_grid(timeDist, timeDistDayOf)

par(mar = c(3,3,3,2))

head(tweetsdf$statusSource)

tweetsdf$newstatus <- tweetsdf$statusSource
head(tweetsdf$newstatus)

# tweetsdf$newstatus <- substr(tweetsdf$newstatus,
#                                 regexpr('>',tweetsdf$newstatus)+1,
#                                 regexpr('<',tweetsdf$newstatus)-1)


# tweetsdf$newstatus <- substr(tweetsdf$newstatus, 
#                              regexpr('>', tweetsdf$newstatus)+1, 
#                              regexpr('',tweetsdf$newstatus)-1)


tweetsdf$newstatus <- gsub('http\\S+\\s*',"", tweetsdf$newstatus)


tweetsdf$newstatus <- substr(tweetsdf$newstatus,
                             regexpr('>',tweetsdf$newstatus)+1,
                             length(tweetsdf$newstatus)-1)



head(tweetsdf$newstatus)


tweetsdf$newstatus1 <- substr(tweetsdf$newstatus,
                              regexpr('T', tweetsdf$newstatus),
                              regexpr('<', tweetsdf$newstatus)-1)

head(tweetsdf$newstatus1)

tweetsdf$newstatus
dotchart(sort(table(tweetsdf$newstatus1)))
mtext('Number of tweets posted by platform')



detach("package:twitteR", unload=TRUE)

tweetsdf <- read.csv(file.choose(), T)
#split into retweets and original tweets
sp = split(tweetsdf, tweetsdf$isRetweet)
orig = sp[['FALSE']]

#Extract the retweets and pull the original author's screenname
#rt is new dataset which has only retweets
rt = mutate(sp[['TRUE']], sender = substr(text, 5, regexpr(':',text)-1))


attach(tweetsdf)

#remove URL from a column
orig$URLfree <- gsub('http[[:alnum:]]*', '', orig$text)
#remove punctuation from a text
orig$puncURLfree <- gsub('[[:punct:] ]+',' ',orig$URLfree)
orig$puncfree <- gsub('[[:punct:] ]+',' ',orig$text)


`[[.qdap_hash` <- `[[.data.frame`
pol = 
  lapply(orig$URLfree, function(txt) {
    # strip sentence enders so each tweet is analyzed as a sentence,
    # and +'s which muck up regex
    gsub('(\\.|!|\\?)\\s+|(\\++)', ' ', txt) %>%
      # strip URLs
      gsub(' http[^[:blank:]]+', '', .) %>%
      # calculate polarity
      `[[.qdap_hash` <- `[[.data.frame`
      qdap::polarity(orig$URLfree)
      
  })

orig$emotionalValence = sapply(pol, function(x) x$all$polarity)




####******IMPORTANT********** the dirty fix for the polarity() error from above.
# USE THIS CODE: `[[.qdap_hash` <- `[[.data.frame`



ggplot(orig, aes(x = emotionalValence, y = retweetCount)) +
  geom_point(position = 'jitter') +
  geom_smooth()


polWordTable = 
  sapply()




















