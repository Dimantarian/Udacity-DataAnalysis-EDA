library(reshape2)
library(dplyr)
library(ggplot2)
setwd('C:\\Users\\geeky\\Dropbox\\Learning\\MOOCs\\Udacity\\Data Analysis NanoDegree\\EDA\\Project\\Udacity-DataAnalysis-EDA')

redWine <- read.csv(file = 'wineQualityReds.csv')
  
table <- data.frame()
for (i in  names(redWine)) {
  assign(paste(i),summary(redWine[,i]))
  }

test<-rbind(alcohol,
            chlorides,
            citric.acid,
            density,
            fixed.acidity,
            free.sulfur.dioxide,
            pH,
            residual.sugar,
            sulphates,
            total.sulfur.dioxide,
            volatile.acidity,
            quality)

melt(redWine)
ggplot(data = melt(redWine[, 2:13]), mapping = aes(x = value)) + 
  geom_histogram(bins = 30) + facet_wrap(~variable, scales = 'free_x')

library(GGally)

?ggpairs


redWine$StrengthCategory <- factor(ifelse(redWine$alcohol < 10 ,'Weak',
                                   ifelse(redWine$alcohol < 12.5, 'Moderate',
                                          ifelse(redWine$alcohol < 14.5, 'Strong', 'Aussie')
                                   )
)
)

## Remove outliers that could be misleading

ggplot(aes(x = residual.sugar , y = density), data = subset(redWine,redWine$residual.sugar<14)) +
  geom_point(alpha = 0.5, color = I('indianred4')) +
  stat_smooth(method = loess)

## Try it with the cube root

cuberoot_trans = function() trans_new('cuberoot', transform = function(x) x^(1/3),
                                      inverse = function(x) x^3)

ggplot(aes(x = residual.sugar^(1/3), y = density), data = subset(redWine,redWine$residual.sugar<14)) +
  scale_x_continuous(trans = cuberoot_trans()) +
  geom_point(alpha = 0.5, color = I('indianred4')) +
  stat_smooth(method = lm)



?stat_smooth



library(ggcorrplot)
# install.packages('corrplot')
library(corrplot)
corrplot()
?ggcorr

ggcorr(redWine[, 2:13], geom = 'text')


#Let's look at alcohol, citric acids and sultphates

g1 <- 
  
  ggplot(redWine,aes(as.factor(quality),citric.acid)) + 
  geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred") + 
  geom_boxplot(width=0.1) + 
  scale_fill_brewer(palette="Reds") + 
  theme_minimal()




g2 <- ggplot(redWine,aes(as.factor(quality),sulphates)) +
  geom_violin(fill = 'red')

g3 <- ggplot(redWine,aes(as.factor(quality),alcohol)) + 
  geom_violin(fill = 'yellow')


library(gridExtra)


grid.arrange(g1, g2, g3, ncol=3)

ggplot(aes(x=citric.acid, y = sulphates), data = redWine) + 
  geom_point(aes(alpha = 0.2, color = quality)) +
  geom_smooth(method = lm)

#install.packages("ggthemes") # Install 
library(ggthemes) # Load

ggplot(aes(x=citric.acid, y = sulphates), data = subset(redWine,redWine$citric.acid<1)) + 
  geom_point(aes(color = as.factor(quality))) +
  geom_smooth(method = lm, color = 'black', fill = 'pink')  +
  coord_cartesian(ylim = c(0.25,1.25)) +
  scale_colour_grey() +
  theme_tufte()
# + 
#   scale_color_economist()


sd(redWine$sulphates)
sd(redWine$citric.acid)
redWine$quality<-factor(redWine$quality,levels = c(3,4,5,6,7,8))


ggplot(aes(x=sulphates, y = chlorides), data = redWine) + 
  geom_point(aes(color = as.factor(quality))) +
  geom_smooth(color = 'black', fill = 'red', method = loess) +
  coord_cartesian(xlim = c(min(redWine$sulphates),1.5)) +
  scale_colour_grey() +
  theme_economist()


redWine.wine_by_quality <- redWine %>%
  group_by(quality) %>%
  summarise(al.stdDev = sd(alcohol),
            al.mean = mean(alcohol),
            al.median = min(alcohol),
            al.n = n()) %>%
  arrange(quality)

qplot(x=quality, y = al.stdDev, data = redWine.wine_by_quality, color = 'blue') + 
  geom_smooth(color = 'blue', fill = 'steelblue', method = lm) +
  theme_tufte()
  
ggplot(aes(x=as.factor(quality), y = al.stdDev), data = redWine.wine_by_quality) + 
  geom_point() + 
  geom_smooth() +
  theme_tufte()


#clustering

# Install
install.packages("FactoMineR")

# Load
library("FactoMineR")

install.packages("factoextra")
library("factoextra")


set.seed(1234)

km.res <- kmeans(scale(redWine[,2:12]),5, nstart = 25)

fviz_cluster(km.res, 
             data = redWine[,2:12],
             geom = 'point',
             ggtheme = theme_minimal(),
             main = "kMeans Clustering Plot"
)

fviz_nbclust(scale(redWine[,2:12]), kmeans, method = "gap_stat")

km.res <- kmeans(scale(redWine[,2:12]),2, nstart = 25)

fviz_cluster(km.res, 
             data = redWine[,2:12],
             geom = 'point',
             ggtheme = theme_minimal(),
             main = "kMeans Clustering Plot"
)

km.res <- kmeans(scale(redWine[,2:12]),4, nstart = 25)

fviz_cluster(km.res, 
             data = redWine[,2:12],
             geom = 'point',
             ggtheme = theme_minimal(),
             main = "kMeans Clustering Plot"
)


?fviz_nbclust



test<-cbind(redWine,km.res[1])

test$cluster <- as.character(test$cluster)
km.res[1]

ggplot(aes(as.numeric(quality)), data = test) +
  geom_histogram(aes(fill = cluster))

ggplot(aes(x=quality), data = test) +
  stat_count(geom = "bar", aes(fill=cluster),position = 'fill') + 
  theme_minimal()

str(test)


##Final plots

redWine$quality <- as.numeric(redWine$quality)

ggcorr(redWine[, 2:13], geom = 'circle', min_size = 5, max_size = 15) + 
  labs(title = 'Red Wine Correlation Matrix', caption = 'Visualising linear 
       relationships between variables') + 
  theme_tufte()