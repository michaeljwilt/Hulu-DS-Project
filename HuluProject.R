library("dplyr")
library("rcompanion")
library("car")
library("ggplot2")
library("IDPmisc")

View(credits)
View(titles)
#Merge the 2 datasets together & Omit N/A values
hulu <- merge(credits,titles,by="id")
hulu1 <- NaRV.omit(hulu)
huluTV <- hulu1

#Run Correlation between variables
cor.test(hulu$imdb_score, hulu$tmdb_popularity, method="pearson")
cor.test(hulu$tmdb_popularity, hulu$seasons, method="pearson")
cor.test(hulu$imdb_score, hulu$seasons, method="pearson")


#####Explore Data#####

##tmdb_popularity
huluTV %>% group_by(tmdb_popularity) %>% summarise(Mean=mean(tmdb_popularity))
mean(huluTV$tmdb_popularity)
#### mean = 48.1846
median(huluTV$tmdb_popularity)
#### median = 20.072
range(huluTV$tmdb_popularity, na.rm=TRUE)
#### range = 0.600 - 2989.846

##imdb_score
huluTV %>% group_by(imdb_score) %>% summarise(Mean=mean(imdb_score))
mean(huluTV$imdb_score)
#### mean = 7.311293
median(huluTV$imdb_score)
#### median = 7.5
range(huluTV$imdb_score, na.rm=TRUE)
#### range = 1.0 - 9.3

##imdb_votes
mean(huluTV$imdb_votes)
#### mean = 23999.2
median(huluTV$imdb_votes)
#### median = 5088
range(huluTV$imdb_votes, na.rm=TRUE)
#### range = 6 - 661972

##TV Shows/Seasons
mean(huluTV$seasons)
#### mean = 3.929138
median(huluTV$seasons)
#### median = 2
range(huluTV$seasons, na.rm=TRUE)
#### range = 1 - 63

View(hulu1)
huluTV  %>% group_by(type) %>% summarise(count = n())

#####ANOVA Analysis#####
#DV = season
#IV = imdb_score
#IV = tmdb_popularity
#Make imdb_score into a categorical variable for the ANOVA analysis
huluTV$imdb_scoreCAT <- cut(huluTV$imdb_score, breaks=c(0, 3, 6, 10), labels=c(1, 2, 3))
View(huluTV)

#Make tmdb_popularity into a categorical variable for the ANOVA analysis
huluTV$tmdb_popularityCATT <- cut(huluTV$tmdb_popularity, breaks=c(0, 1000, 2000, 3000), labels=c(1, 2, 3))
View(huluTV)

keeps2 <- c("title","imdb_score", "tmdb_popularity", "seasons", "imdb_scoreCAT")
huluTV1 <- huluTV[keeps2]
huluTV2 <- distinct(huluTV1)
View(huluTV2)
nrow(huluTV)
nrow(huluTV2)
range(huluTV2$tmdb_popularity, na.rm=TRUE)
huluTV2$tmdb_popularityCAT <- cut(huluTV2$tmdb_popularity, breaks=c(0, 100, 200, 500, 1000, 3000), labels=c(1, 2, 3, 4, 5))
View(huluTV2)

##Assumptions
##Normality
plotNormalHistogram(huluTV2$seasons)
huluTV2$seasonsLOG <- log(huluTV2$seasons)
plotNormalHistogram(huluTV2$seasonsLOG)
#### We have violated Normality
##Homogeneity of Variance
bartlett.test(seasons ~ imdb_scoreCAT, data=huluTV2)
bartlett.test(seasons ~ tmdb_popularityCAT, data=huluTV2)
#### We have violated the Homogeneity of Variance
##Run the ANOVA
### IMDC Score
ANOVA <- lm(seasonsLOG ~ imdb_scoreCAT, data=huluTV2)
Anova(ANOVA, Type="II", white.adjust=TRUE)
#### Our p-value is >0.05 so we can assume there is no significant affect that imdb_score has on the seasons for a show

###TMDB Popularity
ANOVA2 <- lm(seasonsLOG ~ tmdb_popularityCAT, data=huluTV2)
Anova(ANOVA2, Type="II", white.adjust=TRUE)
#### Our p-value is <0.05 so we can assume there is a significant affect that tmdb_popularity has on the seasons of a show.

##Post Hoc
pairwise.t.test(huluTV2$seasons, huluTV2$imdb_scoreCAT, p.adjust="none")
pairwise.t.test(huluTV2$seasons, huluTV2$tmdb_popularityCAT, p.adjust="none")
### IMDB Scores have an impact until they reach the higher scores, then seems to have little impact
### Tmdb popularity has significant impact from 200 - 500 and then after 1000.

#Conclusions
huluTVmean <- huluTV2 %>% group_by(imdb_scoreCAT) %>% summarize(Mean = mean(seasons))
huluTVmean
huluTVmean2 <- huluTV2 %>% group_by(tmdb_popularityCAT) %>% summarize(Mean = mean(seasons))
huluTVmean2

## Final Conclusion
## imdb_scores: 1 = (0-3), 2 = (4-6), 3 = (7-10)
# Shows with a lower imdb_score tend to have more seasons on average

## tmdb_popularity: 1 = (0-100), 2 = (101-200), 3 = (201-500), 4 = (501-1000), 5 = (1001-3000)
# Shows increase their seasons on average as their popularity score goes up closer to 1000. After 1000 they tend to drop back down a bit.


#Separate data for export into Tableau & delete duplicate entries
keeps <- c("title", "imdb_score", "seasons")
huluScores <- huluTV[keeps]
huluScores1 <- distinct(huluScores)
nrow(huluScores)
nrow(huluScores1)
View(huluScores1)
huluScores2 <- arrange(huluScores1, desc(imdb_score))
#Top 10 by imdb score
huluScores3 <- huluScores2[1:10,1:3]
View(huluScores3)


keeps1 <- c("title", "tmdb_popularity", "seasons")
huluPop <- huluTV[keeps1]
huluPop1 <- distinct(huluPop)
nrow(huluPop)
nrow(huluPop1)
View(huluPop1)
huluPop2 <- arrange(huluPop1, desc(tmdb_popularity))
#Top 10 by tmdb popularity
huluPop3 <- huluPop2[1:10,1:3]
View(huluPop3)



####Export Files####
write.csv(huluTV2,"C:\\Users\\michaelwilt\\Desktop\\Bethel\\Project\\Contest\\huluTV.csv", row.names = FALSE)
write.csv(huluScores3,"C:\\Users\\michaelwilt\\Desktop\\Bethel\\Project\\Contest\\huluScores.csv", row.names = FALSE)
write.csv(huluPop3,"C:\\Users\\michaelwilt\\Desktop\\Bethel\\Project\\Contest\\huluPop.csv", row.names = FALSE)
write.csv(huluScoresLow,"C:\\Users\\michaelwilt\\Desktop\\Bethel\\Project\\Contest\\huluScoreLow.csv", row.names = FALSE)
write.csv(huluScoresLow1,"C:\\Users\\michaelwilt\\Desktop\\Bethel\\Project\\Contest\\huluScoreLow10.csv", row.names = FALSE)
write.csv(huluPopLow,"C:\\Users\\michaelwilt\\Desktop\\Bethel\\Project\\Contest\\huluPopLow.csv", row.names = FALSE)
write.csv(huluPopLow1,"C:\\Users\\michaelwilt\\Desktop\\Bethel\\Project\\Contest\\huluPopLow10.csv", row.names = FALSE)




                    
