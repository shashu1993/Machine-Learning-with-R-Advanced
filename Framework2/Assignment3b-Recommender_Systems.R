getwd()
setwd('/Users/shashank/Desktop/Semester 2/Frameworks 2/Week 9')
data = read.csv('product_ratings_data.csv')

#section 2

#q1
head(data)
#a1: long

#q2
library(recommenderlab)
ratings_matrix = as(data,Class = 'realRatingMatrix')
as(ratings_matrix,'matrix')

nratings(ratings_matrix)
#a2: 362105

#q3
as(ratings_matrix,'matrix')['u10023', 'prod_14']
#a3: 4

#q4
set.seed(1031)
split = sample(nrow(ratings_matrix),size = 0.9*nrow(ratings_matrix))
train = ratings_matrix[split,]
test = ratings_matrix[-split,]

nrow(train)
#a4: 4500

#q5
nratings(train['u20150',])
#a5: 44

#q6
nratings(train[,'prod_25'])
#a6: 3745


#q7
table(getRatings(train[]))
#ans: 3

#q8
mean(getRatings(train[,'prod_100']))
#a8: 2.824492

#q9

mean(getRatings(normalize(train, method='center', row = TRUE)[,'prod_100']))
#a9: 0.08438464


#q10
similarity(normalize(train)[1:5,],method = 'cosine')
#a10: u8926 and u395  



#section 3

#q1
recommenderRegistry$get_entries(data=train)$UBCF_realRatingMatrix

recom_ubcf = Recommender(train, method='UBCF')
recom_ubcf
pred_ubcf_topN = predict(recom_ubcf,newdata=test)
getList(pred_ubcf_topN)[1:5]
#a1: prod_11, prod_51

#q2"
pred_ubcf = predict(recom_ubcf,newdata=test,type='ratings')
as(pred_ubcf,'matrix')['u10139','prod_1']
#a2: 3.054352

#q3
recommenderRegistry$get_entries(data=train)$IBCF_realRatingMatrix # see parameters for IBCF
recom_ibcf = Recommender(train, method='IBCF')
recom_ibcf

pred_ibcf_topN = predict(recom_ibcf,newdata=test,method='topNList',n=5)
getList(pred_ibcf_topN)[1:5]
#a3: prod_3 and prod_51


#q4
pred_ibcf = predict(recom_ibcf,newdata=test,type='ratings')
as(pred_ibcf,'matrix')['u10139','prod_1']
#a4: 3.311504


#q5
set.seed(1031)
es = evaluationScheme(ratings_matrix,method='split',train = 0.8, given=30)

recom = Recommender(getData(es,'train'),method='IBCF')
pred_ibcf = predict(recom,newdata = getData(es,'known'),type='ratings')
accuracy_ibcf = calcPredictionAccuracy(x = pred_ibcf,data = getData(es,'unknown'))
accuracy_ibcf
#a5: 1.307363

#q6
recom = Recommender(getData(es,'train'),method='UBCF')
pred_ubcf = predict(recom,newdata = getData(es,'known'),type='ratings')
accuracy_ubcf = calcPredictionAccuracy(x = pred_ubcf,data = getData(es,'unknown'))
accuracy_ubcf
#a6: 1.1821212


#q7
recommenderRegistry$get_entries()$UBCF_realRatingMatrix

recom_ubcf = Recommender(data = getData(es,'train'),
                         method='UBCF',
                         parameter = list(nn=100))
pred_ubcf = predict(recom_ubcf,newdata=getData(es,'known'), type='ratings')
calcPredictionAccuracy(pred_ubcf,data = getData(es,'unknown'))
#a7: 1.1676130

#q8
recom = Recommender(getData(es,'train'),method='POPULAR')
pred_ibcf = predict(recom,newdata = getData(es,'known'),type='ratings')
accuracy_ibcf = calcPredictionAccuracy(x = pred_ibcf,data = getData(es,'unknown'))
accuracy_ibcf
#a8: 1.1692988
