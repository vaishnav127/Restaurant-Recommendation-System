



recomm_user<-read.csv("G:/technical/python/rachana maam project/new data/again user to user/recommendation_to_user_50_25%.csv")


data<-read.csv("G:/technical/python/rachana maam project/new data/again user to user/Food_business_filtered_123_6604.csv")

a <-unique(data$user_id)
num=0
n=0
for(i in 1:nrow(recomm_user))
{
  print(i)
  #n =  n+length(subset(data$business_id,data$user_id==a[i]))
  f<-length(subset(data$business_id,data$user_id==a[i]))
  if(f>=50)
{
   n <- n+50
}
if(f<50)
{
   n <- n+f
}
  for(j in 3:ncol(recomm_user))
  {
    if(!is.na(recomm_user[i,j]))
    {
      if(is.element(recomm_user[i,j],subset(data$business_id,data$user_id==a[i])))
    {
      print("yeah")
      num=num+1
      
    }
}
  
  }

}

accuracy =  (num/n)* 100

23.5456




