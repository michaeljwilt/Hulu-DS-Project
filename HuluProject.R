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
cor.test(hulu1$imdb_score, hulu1$tmdb_popularity, method="pearson")


#Explore Data

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

#ANOVA Analysis
#DV = season
#IV = imdb_score
#IV = tmdb_popularity
#Make imdb_score into a categorical variable for the ANOVA analysis
huluTV$imdb_scoreCAT <- cut(huluTV$imdb_score, breaks=c(0, 3, 6, 10), labels=c(1, 2, 3))
View(huluTV)

#Make imdb_score into a categorical variable for the ANOVA analysis
huluTV$tmdb_popularityCAT <- cut(huluTV$tmdb_popularity, breaks=c(0, 1000, 2000, 3000), labels=c(1, 2, 3))
View(huluTV)

huluTV %>% group_by(tmdb_popularityCAT) %>% summarise(count=n())

##Assumptions
##Normality
plotNormalHistogram(huluTV$seasons)
huluTV$seasonsLOG <- log(huluTV$seasons)
plotNormalHistogram(huluTV$seasonsLOG)
#### We have violated Normality
##Homogeneity of Variance
bartlett.test(seasons ~ imdb_scoreCAT, data=huluTV)
bartlett.test(seasons ~ tmdb_popularityCAT, data=huluTV)
#### We have violated the Homogeneity of Variance
##Run the ANOVA
ANOVA <- lm(seasonsLOG ~ imdb_scoreCAT, data=huluTV)
Anova(ANOVA, Type="II", white.adjust=TRUE)

ANOVA2 <- lm(seasonsLOG ~ tmdb_popularityCAT, data=huluTV)
Anova(ANOVA2, Type="II", white.adjust=TRUE)
#### Our p-values are <0.05 so we can assume there is a significant affect that both variables have on the seasons of a show.

##Post Hoc
pairwise.t.test(huluTV$seasons, huluTV$imdb_scoreCAT, p.adjust="none")
pairwise.t.test(huluTV$seasons, huluTV$tmdb_popularityCAT, p.adjust="none")
### IMDB Scores have a significant impact across the board
### While tmdb popularity only significantly impacts up to 2000. Above 200 is not significant enough

#Conclusions
huluTVmean <- huluTV %>% group_by(imdb_scoreCAT) %>% summarize(Mean = mean(seasons))
huluTVmean
huluTVmean2 <- huluTV %>% group_by(tmdb_popularityCAT) %>% summarize(Mean = mean(seasons))
huluTVmean2

##Final Conclusion
#imdb_scores: 1 = (0-3), 2 = (4-6), 3 = (7-10)
##Shows with a lower imdb_score tend to have more seasons on average

#tmdb_popularity: 1 = (0-1000), 2 = (1001-2000), 3 = (2001-3000)
## Shows ranked between 1000-2000 have significantly more seasons than the higher or lower. Triple at the least.

write.csv(huluTV,"C:\\Users\\michaelwilt\\Desktop\\Bethel\\Project\\Contest\\huluTV.csv", row.names = FALSE)

