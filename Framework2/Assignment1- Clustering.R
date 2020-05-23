RNGversion(vstr = 3.6)

library(mice)
survey = read.csv("/Users/shashank/Desktop/Semester 2/Frameworks 2/Week 3/FastfoodSurvey.csv")

#section 1

#Q1
str(survey)
#ans: 21

#Q2
data_cluster = survey[,1:11]
head(data_cluster[,1:5])
#ans: 11

#Q3
table(is.na(data_cluster$cleanliness))
#ans: 23

#Q4
data_cluster2 = data_cluster
new_data_cluster = na.omit(data_cluster)
summary(new_data_cluster)
sum(is.na(new_data_cluster))
dim(new_data_cluster)

nrow(data_cluster2)
#ans: 556

#Q5
set.seed(1706)
data_cluster = complete(mice(data_cluster))
data_cluster$cleanliness[10]
head(data_cluster[,1:4])
#ans:6

#Q6
data_cluster = scale(data_cluster)
head(data_cluster[,1:4])
data_cluster[10,'cleanliness']
#ans: 0.347997612

#################################################
#section 2
  
#Q1
d = dist(x = data_cluster,method = 'euclidean')
length(d)
#ans: 193131

#Q2

clusters = hclust(d = d,method='ward.D2')
plot(clusters)
cor(cophenetic(clusters),d)
#and: 0.7903926

#Q3

plot(cut(as.dendrogram(clusters),h=5)$upper)
plot(clusters)
rect.hclust(tree=clusters,k = 4,border='tomato')
#install.packages("dendextend")
library(dendextend)
plot(color_branches(as.dendrogram(clusters),k = 4,groupLabels = F))
#install.packages("factoextra")
library(factoextra)
fviz_dend(x = clusters,k=6)
#install.packages("gridExtra")
library(gridExtra)
grid.arrange(fviz_dend(x = clusters,k=4),
             fviz_dend(x = clusters,k=5),
             fviz_dend(x = clusters,k=6)
)

h_segments = cutree(tree = clusters,k=6)
table(h_segments)

#ans: 2

#Q4
h_segments2 = cutree(tree = clusters,k=2)
table(h_segments2)
#ans: 41

#Q5
h_segments3 = cutree(tree = clusters,k=3)
table(h_segments3)
#ans: 41

#Q6
set.seed(1706)
km = kmeans(x = data_cluster,centers = 2,iter.max=100)
table(km$cluster)
#ans: 43

#Q7
set.seed(1706)
km2 = kmeans(x = data_cluster,centers = 3,iter.max=100)
table(km2$cluster)
#ans: 41

#Q8
set.seed(1706)
paste(km2$totss,'=',km2$betweenss,'+',km2$tot.withinss,sep = ' ')
km2$tot.withinss
#ans: 3801.308

#Q9
km2$betweenss/km2$totss
#ans: 0.4435209

#Q10

within_ss = sapply(1:10,FUN = function(x) kmeans(x = data_cluster,centers = x,iter.max = 100)$tot.withinss)
ggplot(data=data.frame(cluster = 1:10,within_ss),aes(x=cluster,y=within_ss))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))

#ans: 2,3


#Q11
library(cluster)
pam(data_cluster,k = 2)$silinfo$avg.width

#ans: 0.5869224

#Q12
pam(data_cluster,k = 3)$silinfo$avg.width

#ans:0.1722077

#Q13
pam(data_cluster,k = 5)$silinfo$avg.width
pam(data_cluster,k = 6)$silinfo$avg.width
pam(data_cluster,k = 7)$silinfo$avg.width


silhoette_width = sapply(2:10,FUN = function(x) pam(x = data_cluster,k = x)$silinfo$avg.width)
ggplot(data=data.frame(cluster = 2:10,silhoette_width),aes(x=cluster,y=silhoette_width))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(2,10,1))

#ans: 2

#Q14
#install.packages("mclust")
library(mclust)
clusters_mclust = Mclust(data_cluster)
summary(clusters_mclust)
#ans: 3

#Q15
clusters_mclust_2 = Mclust(data_cluster,G=2)
summary(clusters_mclust_2)
#ans: 171

#Q16

table(h_segments2)
table(km$cluster)

table(h_segments2,km$cluster)
sum(h_segments2 == km$cluster)

#ans: 4

#Q17

table(km$cluster)
summary(clusters_mclust_2)
171-43
#ans: 128

#################################################
#section 3
#Q1-4

set.seed(1706)
km = kmeans(x = data_cluster,centers = 3,iter.max=100)
k_segments = km$cluster
data2 = cbind(survey,k_segments)

library(dplyr)
data2 %>%
  select(speed_of_service:taste_burgers,k_segments)%>%
  group_by(k_segments)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  data.frame()

library(dplyr); library(ggplot2); library(tidyr)
data2 %>%
  select(speed_of_service:taste_burgers,k_segments)%>%
  group_by(k_segments)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  gather(key = var,value = value,speed_of_service:taste_burgers)%>%
  ggplot(aes(x=var,y=value,fill=factor(k_segments)))+
  geom_col(position='dodge')+
  coord_flip()


#Q5-7
data2[,11]

prop.table(table(data2$k_segments,data2[,3]),1)

library(ggplot2)
tab = prop.table(table(data2$k_segments,data2[,12]),1)
tab2 = data.frame(round(tab,2))
library(RColorBrewer)
ggplot(data=tab2,aes(x=Var2,y=Var1,fill=Freq))+
  geom_tile()+
  geom_text(aes(label=Freq),size=6)+
  xlab(label = '')+
  ylab(label = '')+
  scale_fill_gradientn(colors=brewer.pal(n=9,name = 'Greens'))


lapply(12:21,function(x) round(prop.table(table(data2$k_segments,data2[,x]),1),2)*100)

lapply(12:21,function(x) {
  dat = round(prop.table(table(data2$k_segments,data2[,x]),1),2)*100
  dat = data.frame(dat)
  ggplot(data=dat,aes(x=Var2,y=Var1,fill=Freq))+
    geom_tile()+
    geom_text(aes(label=Freq),size=6)+
    xlab(label = '')+
    ylab(label = '')+
    scale_fill_gradientn(colors=brewer.pal(n=9,name = 'Greens'))
})



