
# code to find table of ratings by the user to all restaurants ,columns are rest_id and rows are user_id
big_data<-read.csv("G:/technical/python/rachana maam project/new data/again user to user/Food_business_filtered_123_6604.csv")
#nrow-19922
#length(unique(big_data$user_id)) - 6604
#length(unique(big_data$business_id)) - 123

user<-levels(big_data$user_id)
rest<-levels(big_data$business_id)
rest<- unlist(rest)

mydata=big_data[,c("user_id","business_id","stars.x")]

# kmode is the file from where i will select the column which describe the clouster_no of each user_id
kmode<-read.csv("G:/technical/python/rachana maam project/new data/again user to user/cluster_result.csv")
 
myMatrix <- data.frame(matrix(user,ncol=1,nrow=length(user)))
 
myMatrix <- cbind(myMatrix,kmode$cluster_no)
colnames(myMatrix)<- c("user_id","cluster_no")


library(varhandle)
for(i in 1:length(user)){
print(i)
temp = subset(mydata, (mydata$user_id==user[i]))

 
	 for(j in 1:nrow(temp))
	 {
	 y <- temp[j,2] 
	 
	 if(!is.element(y,colnames(myMatrix))) 
	
         {
         columnMatrix <- data.frame(matrix(ncol=1,nrow=length(user)))
         colnames(columnMatrix) <- y 
         columnMatrix[i,] <- temp[j,3]  
         myMatrix <- cbind(myMatrix,columnMatrix) 
     }
     else #if(is.element(y,colnames(myMatrix))) 
	 {
          myMatrix[i,levels(factor(y))]<- temp[j,3]
		  }
	 }
 }
 
write.csv(myMatrix,"G:/technical/python/rachana maam project/new data/again user to user/ratings_matrix.csv", row.names=FALSE)

#ncol(myMatrix)-125
#nrow(myMatrix)-6604






# code to find the similarity matrix for each cluster based on the attributes,consider there are k clusters so k similarity matrices will be form for each cluster

att<-read.csv("G:/technical/python/rachana maam project/new data/again user to user/my_attributes3.csv")

rate_data<-read.csv("G:/technical/python/rachana maam project/new data/again user to user/ratings_test_matrix.csv")
all_prediction_score <- data.frame(matrix(ncol=(ncol(rate_data)-1),nrow=0))# score for all clusters 
colnames(all_prediction_score) <- c("user_id",colnames(rate_data)[3:ncol(rate_data)])
library(SnowballC)

k<- length(unique(rate_data$cluster_no))# give total no of clusters
	 for(j in 0:(k-1))
	 {
	 print(j)
	 tempoo <- subset(rate_data, (rate_data$cluster_no == j))# select all the rows of users_ids of cluster no j 
	 n<- nrow(tempoo) # total no of user ids in kth cluster
	 
	 f<- tempoo[,c(3:ncol(rate_data))]# total no of restraunt ids are 10845
	 copy_f<- f
	 m<- ncol(f) # total no of restaurants ids so f is of order nxm
	 avg_rating_user  <- rowMeans(f[sapply(f, is.numeric)],na.rm=TRUE)#find avg of ratings of user only when he is giving ratings of order nx1
	 avg_rating_user <- as.data.frame(avg_rating_user)
	 #now put avg ratings of user where ratings are null
	 for(i in 1:nrow(f))
	 {
	  #f[i,is.na(f[i,])] <- avg_rating_user[i,]
	  f[i,is.na(f[i,])] <- 0
	 }
	 
	 simi <- subset(att, (att$cluster_no == j))
	 sim<- simi[,c(3:ncol(att))]
	 mat<- as.matrix(sim)
	 mat<- t(mat) # transpose of mat becoz pearson fxn find similarity btw  columns so i changed user ids to columns 
	 
	 library(lsa)
	 sim_matrix <- cosine(mat)
	 print("complete")
      # similariity matrix,square matrix of order of nrow in temp means nxn
	 sim_matrix<- as.data.frame(sim_matrix)
	 
	 # now subtract avg rating of user from each row
	 #avg_subtracted_ratings <- f - avg_rating_user[,1] # order nxm
	 avg_subtracted_ratings <- f  # order nxm
	 #multiply sim_matrix and avg_subtracted_ratings as matrix multiplication occurs
	 kth_cluster_prediction_score <- data.frame(matrix(ncol=(m+1),nrow=n))# 1 column for user_ids and rest for restaurants & score for kth cluster
	 
	 colnames(kth_cluster_prediction_score) <- c("user_id",colnames(rate_data)[3:ncol(rate_data)])
	 #colnames(kth_cluster_prediction_score) <- c("user_id",rest)#it can be another way
	 kth_cluster_prediction_score$user_id <- tempoo$user_id
	 
#z<- myMatrix[1:1600,]
#summary(factor(z$cluster_no))
 # 0   1   2   3  4  5   6   7   8   9 
#256 211  14  91   1 356 181   7  50 433 
	 a<- c(256, 211 , 14 , 91  , 1 ,356 ,181 ,  7  ,50 ,433  )
	 for(p in 1:a[j+1]){ #for(p in 1:nrow(sim_matrix)){ 
	 print(p)
      for(q in 1:ncol(avg_subtracted_ratings)){ 
      
	  a<- sim_matrix[p,]# row dataframe
	  a<- t(a)#transpose then becomes matrix
	  a<- as.data.frame(a)#col dataframe
	  
	  b<- copy_f[,q]# its a column
	  b<- t(b)#transpose then becomes matrix
	  b<- as.data.frame(b)#row dataframe shows rating of restaurant q by all users,it is having null vlaues
	  
	  numerator <-  sum(a * avg_subtracted_ratings[,q])# a is column of length n and avg_subtracted_ratings[,q] is column of length n now multiply and take sum
	  
	  if(length(sim_matrix[p,!is.na(b)]) !=0){
	  denominator <- sum(sim_matrix[p,!is.na(b)])  #  now divide by the sum of similarity  p user with the all users that has rated the restaurant q
	  }
	  if(length(sim_matrix[p,!is.na(b)]) ==0)
	  {
	  denominator = 0
	  }
	  
	  
	  if(denominator!=0)
	  {
	  
	  kth_cluster_prediction_score[p,(q+1)]<- (numerator/denominator) # add average rating value of user p
	  
	  }
	  if(denominator==0)# means if no user rated restaurant q then 0/0 comes  in score as num and den will be both zero so i m changing it to null
	  {
	  kth_cluster_prediction_score[p,(q+1)]<- NA
	  }
	  
	  }
    	  
	}
	
	 all_prediction_score <- rbind(all_prediction_score , kth_cluster_prediction_score)# score for all clusters
	 
	 
	  }
	  
write.csv(all_prediction_score,"G:/technical/python/rachana maam project/new data/again user to user/prediction_score.csv", row.names=FALSE)

 

#arrange pred data according to rating matrix
pred_data <-read.csv("G:/technical/python/rachana maam project/new data/again user to user/prediction_score.csv")
rate_data<-read.csv("G:/technical/python/rachana maam project/new data/again user to user/ratings_test_matrix.csv")

pred_data_arrange <- data.frame(matrix(ncol=ncol(pred_data),nrow=0))#
colnames(pred_data_arrange) <- colnames(pred_data)

for(j in 1:nrow(rate_data))
{
print(j)
a<- rate_data$user_id[j]
temp <- subset(pred_data, (pred_data$user_id == a))

pred_data_arrange <- rbind(pred_data_arrange , temp)
	 
}
write.csv(pred_data_arrange,"G:/technical/python/rachana maam project/new data/again user to user/pred_data_arrange25%.csv", row.names=FALSE)
   
 
 
 
 
   
   
 

# now code for finding recommendation by just seeing in each row of prediction_score file top 10 prediction scores 
pred_data <-read.csv("G:/technical/python/rachana maam project/new data/again user to user/pred_data_arrange25%.csv")
pred_data<- pred_data[1:1600,]
rate_data<-read.csv("G:/technical/python/rachana maam project/new data/again user to user/ratings_test_matrix.csv")# ratings file
rate_data<- rate_data[1:1600,]

g<- ncol(pred_data)
my_dataa <- pred_data[2:g]

recommend <- data.frame(matrix(ncol=52,nrow=nrow(pred_data)))# having 10 columns to show rest_ids to recoomend

recommend[,1] <- pred_data$user_id
recommend[,2] <- rate_data$cluster_no
 
for(u in 1:nrow(pred_data))
{
# see 10 max values in row
print(u)
user <- my_dataa[u,is.na(rate_data[u,3:ncol(rate_data)])]# finding only prediction score of only that restaurant for user u which usr has not visited
user<- user[,!is.na(user)]# now take only that restraunts for which prediction score is not null
if(ncol(user) >= 50)
{
p <- data.frame(colnames(user[,order(user, decreasing= T)][1:50]))# it create a column of top 10 columnnames of restaurant ids for user that is not visited by user
p <- t(p)
p<- as.data.frame(p)# it creates a row dataframe for a user of colnames of top 10 restaurant id to recommend
recommend[u,3:52]<-  unlist(p)
}
if(ncol(user) < 50)
{
p <- data.frame(colnames(user[,order(user, decreasing= T)][1:ncol(user)]))
p <- t(p)
p<- as.data.frame(p)# it creates a row dataframe for a user of colnames of top 10 restaurant id to recommend
recommend[u,3:(ncol(user)+2)]<-  unlist(p)
}


} 


write.csv(recommend,"G:/technical/python/rachana maam project/new data/again user to user/recommendation_to_user_50_25%.csv", row.names=FALSE)
   

   
    
   
   
   
   
   
   