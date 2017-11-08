my_data1<-read.csv("Vash_FC.csv")     
my_data1<-my_data1[1:6000, ]      #selecting users we have made clusters for
user_fact<-my_data1$user_id
library(varhandle)
user_fact<-unfactor(user_fact)           #selecting the user_id vector
bid<-my_data1$business_id
bid_level<-levels(bid)                 #selecting the vector containing unique restaurants
#bid_level<-unfactor(bid_level)
avg<-vector(mode="numeric", length = length(bid_level))    #initialising the average vector
for(i in 1:length(bid_level))     #in this loop, we store average ratings of each restaurant in a vector
{
  print(i)
  selection<-my_data1$business_id[]==bid_level[i]
  temp<-my_data1$stars.x[selection]
  if(length(temp)==0)
  {
    avg[i]<-0
  }
  else
  {
    #temp<-unfactor(temp)
    avg[i]<-mean(temp)
  }
}
print(avg)
names(avg)<-bid_level
print("..................................")
My_matrix<-matrix( , nrow=0, ncol = length(user_fact))
for(i in 1:length(bid_level))   #this loop makes the matrix between all the restaurants and users. (the elements are the rating by a user given to a restaurant)
{
  print(i)
  
  temp<-rep(avg[i], length(user_fact))
  names(temp)<-user_fact
  selection<-my_data1$business_id[]==bid_level[i]
  temp_user<-my_data1$user_id[selection]
  temp_star<-my_data1$stars.y[selection]
  
  temp[temp_user]<-temp_star
  My_matrix<-rbind(My_matrix, temp)
}
colnames(My_matrix)<-user_fact
#row.names(My_matrix)<-bid_level
My_matrix<-t(My_matrix)    #transpose of matrix to convert it into proper format for feeding into cosine function.
library(SnowballC)
library(lsa)
res_similarity<-cosine(My_matrix)
write.csv(res_similarity, "item-item_rec_similarity_matrix.csv")