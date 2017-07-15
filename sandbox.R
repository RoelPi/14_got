rm(list=ls())
options(scipen=999)
library(jsonlite)
library(data.table)
library(ggplot2)
library(RColorBrewer)
library(fitdistrplus)

key <- as.character(read.table("apikey.txt")[1,1])

series <- read.csv2("series_list.csv",stringsAsFactors=F)$id

getRating <- function(id = series[1]) {
    url <- paste0("http://www.omdbapi.com/?apikey=",key,"&i=",id)
    result <- tryCatch({
        d <- fromJSON(url)
        dt <- c(id,d$Title,substring(d$Year,1,4),gsub("\\.","\\,",d$imdbRating),gsub(",","",d$imdbVotes))
        dt <- data.table(t(dt))
        colnames(dt) <- c('id','title','year','rating','votes')
        dt
    },error = function(e) {
        print(paste0("Could not get ",url))
        dt <- c(id,NA,NA,NA,NA)
        dt <- data.table(t(dt))
        colnames(dt) <- c('id','title','year','rating','votes')
        dt
    })
}
getRatings <- function(s = series,new=F) {
    dt <- data.table(id=character(),title=character(),year=numeric(),rating=numeric(),votes=numeric())
    if (new == T) {
        for (i in 1:length(series)) {
            print(paste0("Getting series ",i," of ",length(series),"."))
            d <- getRating(series[i])
            dt <- rbind(dt,d)
        }
        write.csv2(dt,'series_score.csv',row.names=F)
        dt
    } else {
        dt <- read.csv2('series_score.csv',stringsAsFactors=F)
    }
    dt
}
getEpisodesID <- function(id = series[1]) {
    getSeason <- function(season = 1) {
        url <- paste0("http://www.omdbapi.com/?apikey=",key,"&i=",id,"&type=series&season=",season)
        result <- tryCatch({
            d <- fromJSON(url)
            d
        },error = function(e) {
            print(paste0("Could not get ",url))
            d <- list()
            d$Response = "False"
            d
        })
    }
    dt <- data.table(id=character(),season=numeric(),episode=numeric(),episode_id=character())
    err <- F
    i <- 0
    while (err == F) {
        i <- i + 1
        result <- tryCatch({
            d <- getSeason(i)
            if (d$Response == "False") {
                err <- T
            } else {
                dtSeason <- cbind.data.frame(id,d$Season,d$Episodes$Episode,d$Episodes$imdbID)
            }
        }, error = function(e) {
            print(paste0("Error: ",e))
            dtSeason <- data.table(t(c(id,NA,NA,NA)))
        }, finally = {
            if (err == F) {
                colnames(dtSeason) <- c('id','season','episode','episode_id')
                dt <- rbind(dt,dtSeason)
            }
        })
    }
    dt
}
getAllEpisodeRatings <- function(s = series,newList=F,newRatings=F) {
    dt <- data.table(id=character(),season=numeric(),episode=numeric(),episode_id=character())
    
    # Generate list of all episodes
    if (newList == T) {
        for (i in 1:length(series)) {
            
            print(paste0("Getting series ",i," of ",length(series),"."))
            d <- getEpisodesID(series[i])
            colnames(d) <- c('id','season','episode','episode_id')
            dt <- rbind(dt,d)
        }
        write.csv2(dt,'episode_list.csv',row.names=F)
        dt
    } else {
        dt <- read.csv2('episode_list.csv',stringsAsFactors=F)
    }
    
    ds <- data.table(episode_id=character(),episode_title=character(),year=numeric(),episode_rating=numeric(),episode_votes=numeric())
    # Get ratings for all episodes
    if (newRatings == T) {
        for (i in 1:nrow(dt)) {
            d <- getRating(dt[i,4])
            colnames(d) <- c('episode_id','episode_title','year','episode_rating','episode_votes')
            print(paste0("Getting episode ",i," of ",nrow(dt),"."))
            ds <- rbind(ds,d)
            if (i %% 100 == 0) {
                write.csv2(ds,'episodes_score_partial.csv',row.names=F)
                print("Saving temporarily.")
            }
        }
        write.csv2(ds,'episodes_score.csv',row.names=F)
    } else {
        ds <- read.csv2('episodes_score.csv',stringsAsFactors=F,na.strings="N/A",dec=",")
    }
    ds
}

# Get series by network
n <- data.table(read.csv2("series_list.csv",stringsAsFactors=F))

# Get series data
s <- getRatings()
s <- data.table(s)
s$votes <- as.numeric(s$votes)
s$rating <- as.numeric(gsub(",",".",s$rating))
s <- s[order(-rating)]

# Get episode data
e <- getAllEpisodeRatings()
e$episode_votes <- as.numeric(e$episode_votes)
e$episode_rating <- as.numeric(gsub(",",".",e$episode_rating))
l <- data.table(read.csv2("episode_list.csv",stringsAsFactors=F))
e <- merge(l,e,by=c("episode_id"))

# Merge all data
a <- merge(n,s,by=c("id"))
a <- merge(a,e,by=c("id"))
a <- a[order(title,season,episode)]
quantile(s$votes,c(0.10,0.25,0.50,0.75,0.95),na.rm=T)

t <- s[votes >= quantile(s$votes,c(0.95),na.rm=T)]
t <- t[order(-votes)]

###################
# Game of Thrones #
###################
got <- a[title=="Game of Thrones" & !is.na(episode_votes)]
g1 <- ggplot(got,aes(episode,episode_votes,fill=as.factor(season))) + 
    geom_bar(stat="identity",colour="black") + 
    scale_x_continuous(breaks=seq(1,10,1)) +
    scale_y_continuous(breaks=seq(0,150000,10000)) +
    facet_grid(.~season) +
    labs(y="votes",x="episode") +
    scale_fill_brewer(palette="YlOrRd",guide=F) +
    scale_color_discrete(guide=F)

g2 <- ggplot(got,aes(episode,episode_rating,fill=as.factor(season))) + 
    geom_bar(stat="identity",colour="black") + 
    scale_x_continuous(breaks=seq(1,10,1)) +
    scale_y_continuous(breaks=seq(0,10,1)) +
    facet_grid(.~season) +
    labs(y="votes",x="episode") +
    scale_fill_brewer(palette="YlOrRd",guide=F) +
    scale_color_discrete(guide=F)

s25 <- head(s[votes>1000],25)
g3a <- ggplot(s25,aes(reorder(title,rating),rating,fill=as.character(rating),label=rating)) + 
    geom_bar(stat="identity",colour="black") + 
    geom_text(nudge_y=1) +
    coord_flip() +
    scale_fill_brewer(palette="YlOrRd",guide=F) +
    scale_color_discrete(guide=F) +
    labs(x="title",y="rating")

s25b <- head(s[order(-votes)],25)
g3b <- ggplot(s25b,aes(reorder(title,votes),votes,label=votes)) + 
    geom_bar(stat="identity",colour="black",fill="#d7301f") + 
    geom_text(nudge_y=50) +
    coord_flip() +
    scale_fill_brewer(palette="YlOrRd",guide=F) +
    scale_color_discrete(guide=F) +
    labs(x="title",y="rating")

s3 <- data.table(title=
    c("Game of Thrones","5% Most votes","All series"),
    rating=c(s[title=="Game of Thrones"]$rating,round(mean(t$rating),1),round(mean(s$rating,na.rm=T),1)
))
g4 <- ggplot(s3,aes(title,rating,fill=title,label=rating)) +
    geom_bar(stat="identity",colour="black") + 
    geom_text(nudge_y=1) +
    coord_flip() +
    scale_fill_brewer(palette="YlOrRd",guide=F) +
    scale_color_discrete(guide=F) +
    labs(x="",y="rating")

sCh <- a[,.(aCount=length(unique(title))),by=.(Channel,year.x)]
sCh <- sCh[order(year.x,Channel)]
sCh <- sCh[year.x != 2017]
g5 <- ggplot(sCh,aes(year.x,aCount,fill=Channel)) + 
    geom_bar(position="stack",stat="identity",colour="black") +
    scale_fill_brewer(palette="Paired") +
    labs(x="year",y="# series")

g6 <- ggplot(sCh,aes(year.x,aCount,label=aCount)) + 
    geom_line(size=1,col="#d7301f") +
    geom_text(size=3) +
    labs(x="year",y="# series") +
    facet_grid(Channel~.)
    
sCh2 <- a[,.(aCount=length(unique(title))),by=.(Channel,year.y)]
sCh2 <- sCh2[order(year.y,Channel)]
sCh2 <- sCh2[year.y != 2017 & year.y != 2018 & year.y != 1999 & year.y != 'NA']
g7 <- ggplot(sCh2,aes(year.y,aCount,fill=Channel)) + 
    geom_bar(position="stack",stat="identity",colour="black") +
    scale_fill_brewer(palette="Paired") +
    labs(x="year",y="# series")

g8 <- ggplot(sCh2,aes(year.y,aCount,label=aCount)) + 
    geom_line(size=1,col="#d7301f",group=1) +
    geom_text(size=3) +
    labs(x="year",y="# series") +
    facet_grid(Channel~.)

sCh3 <- a[,.(aRating=max(rating)),by=.(Channel,title,year.x)
          ]
g9 <- ggplot(sCh3,aes(year.x,aRating)) + 
    geom_jitter() +
    geom_smooth(method='lm',se=F) +
    labs(x="year",y="rating")

g10 <- ggplot(s[votes<25000],aes(votes,rating)) + 
    geom_point() +
    geom_smooth(method="lm")

g11 <- ggplot(s,aes(votes,fill=votes)) + 
    geom_histogram(binwidth=25000,na.rm=T,fill="#d7301f",col="black") +
    scale_x_continuous(breaks=seq(0,1200000,25000)) +
    theme(axis.text.x = element_text(size=8, angle=90))

sPlotVotes <- ggplot(sCh3,aes(year.x,votes,fill=Channel)) + geom_jitter(col="black") + geom_smooth(method=lm)
sLMVotes <- with(s,lm(votes~year))
sLMRating <- with(s,lm(rating~year))

perSeason <- function(d = getAllRatings(series,F)) {
    d <- d[,.(avg=mean(score),max=max(score)),by=.(series,season)]
    d[as.numeric(season) <= 5]
}
plotSeries <- function(d = getRatings()) {
    ggplot(d, aes(x=paste0(season,'-',episode),y=score,col=season,group=1)) + 
        geom_line(stat='identity',size=2) +
        theme(axis.text.x  = element_text(angle=90, size=8)) +
        scale_y_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10),limits=c(0,10)) +
        xlab('Episode') +
        ylab('IMDB Rating')
}
plotAllSeries <- function(d = perSeason()) {
    ggplot(d, aes(x=season,y=max,group=1)) + 
        geom_jitter(size=2) +
        theme(axis.text.x  = element_text(angle=90, size=8)) +
        scale_y_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10),limits=c(0,10)) +
        xlab('Season') +
        ylab('IMDB Rating')
}