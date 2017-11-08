
########### FILE INPUT ############
my_data1<-read.csv("Vash_FC.csv")
clusters<-read.csv("Cluster_result.csv")
res_similarity<-read.csv("item-item_rec_similarity_matrix.csv")
f = read.csv("allocated_cluster.csv")
fr = read.csv("similar_business.csv")

my_data1<-my_data1[1:6000, ]
res_similarity<-res_similarity[, 2:ncol(res_similarity)]
bid_level<-levels(my_data1$business_id)
rec_user<-matrix(, nrow=0, ncol=51)
f = f[, 2:ncol(f)]
fr = fr[, 2:ncol(fr)]
library(varhandle)
#### traversing through all the new users ######
for(all in 197:1000)
{
  user_id = unfactor(f[all, 1])
  cluster_no = f[all, 2]
  select<- (clusters$cluster_no==cluster_no)
  target_user<- unfactor(clusters$user_id[select])
  target_business<-vector(mode="character", length = 0)
  for(some in 1:length(target_user))
  {
    target_business<-cbind(target_business, t(unfactor(my_data1$business_id[my_data1$user_id[]==target_user[some]])))
  }
     #selecting the business ids from the cluster the new user belongs to
  visited_restaurant<-unfactor(fr[all, 2])    #selecting restaurants visited by the user
  rec_res<-levels(factor(target_business))
  rec_res_rat<-vector(mode="numeric", length = length(rec_res))      #initializing another vector which will contain the recommended ratings
  names(rec_res_rat)<-rec_res
  for(i in 1:length(rec_res))
  {
    print(i)
    iin<-which(bid_level==rec_res[i])        
    iin<-iin[1]
    print(iin)
    jn<-which(bid_level==visited_restaurant)
    jn<-jn[1]
    print(jn)
    sim<-res_similarity[iin, jn]              #the similarity between the visited restaurant and the recommended one
    print(sim)
    rat<-my_data1$stars.x[unfactor(my_data1$business_id[])==visited_restaurant]      #rating of the visited restaurant as given by the user
    print(rat)
    rating<-mean(sim*rat)                   #probable rating for the recommended restaurant
    print(rating)
    rec_res_rat[i]<-rating
  }
  rec_res_rat<-sort(rec_res_rat, decreasing = TRUE)                #sorting the recommended restaurants on the basis of probable ratings
  print(all)
  rec_user<-rbind(rec_user, c(user_id, names(rec_res_rat)[1:50]))  
}
write.csv(rec_user,"final_recommendation.csv",row.names=FALSE)
