
############## FILE INPUT ##################

my_data1<-read.csv("final_recommendation.csv")
rec_user<-my_data1
check<-read.csv("Vash_FC.csv")
check<-unfactor(check$business_id)
#check<-unfactor(check[, 2])
count=0
for(i in 1:nrow(rec_user))
{
  print(i)
  for(j in 2:ncol(rec_user))
  {
    print(j)
    if(is.na(rec_user[i,j]))
    {
      next()
    }
    if(rec_user[i, j]==check[i])
    {
      count=count+1
      break
    }
  }
}
acc=count/nrow(rec_user)