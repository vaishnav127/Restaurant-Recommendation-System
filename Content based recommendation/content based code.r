user_data<-read.csv("G:/technical/python/rachana maam project/new data/content based/user_attributes.csv")
user_data<-subset(user_data,user_data$cluster_no!=9)#97
business_data<-read.csv("G:/technical/python/rachana maam project/new data/content based/final_unique_business.csv")#1369
#89th column will be cluster_no
library(lsa)

rec<- data.frame(matrix(ncol=(nrow(business_data)+2),nrow=0))#having 10 columns to show rest_ids to recoomend
colnames(rec)[1:2]<- c("user_id","cluster_no")
k<-46
for(t in 1:k)

{
print(t)
a<- subset(user_data,user_data$cluster_no==t)
b<- subset(business_data,business_data$cluster_no==t)

if((nrow(a)!=0 )& (nrow(b)!=0)){
sim<- data.frame(matrix(ncol=(nrow(b)+1),nrow=nrow(a)))# having 10 columns to show rest_ids to recoomend
library(varhandle)
colnames(sim)<- c("user_id",unfactor(b$business_id))#ERROR
sim$user_id <- a$user_id
for (i in 1:nrow(a))
{
print(i)
print("aaaaaaaaaaa")

for(j in 1:nrow(b))
{
#print(j)
vec1 = unlist(a[i,2:87])
vec2 = unlist(b[j,2:87])
s <- cosine(vec1,vec2)[,1]
sim[i,j+1] <- s

}

}

print("done")
pred_data<- sim
recommend <- data.frame(matrix(ncol=(nrow(business_data)+2),nrow=nrow(a)))# having 10 columns to show rest_ids to recoomend
colnames(recommend)[1:2]<- c("user_id","cluster_no")

recommend$user_id <- a$user_id
recommend$cluster_no <- a$cluster_no

pred_data<- pred_data[,2:ncol(pred_data)]
for(u in 1:nrow(pred_data))
{
#print(u)
user<- pred_data[u,!is.na(pred_data[u,])]

g<- floor(nrow(b)/2) #half of cluster size

if(ncol(user) >= g)
{
p <- data.frame(colnames(user[,order(user, decreasing= T)][1:g]))# it create a column of top 10 columnnames of restaurant ids for user that is not visited by user
p <- t(p)
p<- as.data.frame(p)# it creates a row dataframe for a user of colnames of top 10 restaurant id to recommend
recommend[u,(3:(g+2))]<-  unlist(p)
}
if(ncol(user) < g)
{
p <- data.frame(colnames(user[,order(user, decreasing= T)][1:ncol(user)]))
p <- t(p)
p<- as.data.frame(p)# it creates a row dataframe for a user of colnames of top 10 restaurant id to recommend
recommend[u,3:(ncol(user)+2)]<-  unlist(p)
}

}


rec<- rbind(rec,recommend)
}
}
write.csv(rec,"G:/technical/python/rachana maam project/new data/content based/recommendation_to_user.csv", row.names=FALSE)
 

 
#arrange  rec data in order of user_id as original
 
rec_data <-read.csv("G:/technical/python/rachana maam project/new data/content based/recommendation_to_user.csv")
user_data<-read.csv("G:/technical/python/rachana maam project/new data/content based/user_attributes.csv")
user_data<-subset(user_data,user_data$cluster_no!=9)#97

rec_data_arrange <- data.frame(matrix(ncol=ncol(rec_data),nrow=0))#
colnames(rec_data_arrange) <- colnames(rec_data)

for(j in 1:nrow(user_data))
{
print(j)
a<- user_data$user_id[j]
temp <- subset(rec_data, (rec_data$user_id == unfactor(a)))

rec_data_arrange <- rbind(rec_data_arrange , temp)
	 
}
write.csv(rec_data_arrange,"G:/technical/python/rachana maam project/new data/content based/recommendation to user_final.csv", row.names=FALSE)
  
  
  
  
  
  
  
  
  
  
  
