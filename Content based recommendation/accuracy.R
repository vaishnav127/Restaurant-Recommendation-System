recomm_user<-read.csv("G:/technical/python/rachana maam project/new data/content based/recommendation to user_final.csv")


data<-read.csv("G:/technical/python/rachana maam project/new data/content based/filtered_user_business.csv")

#a <-unique(data$user_id)
a<- unique(recomm_user$user_id)
num=0
library(varhandle)
n=0
for(i in 1:nrow(recomm_user))
{
  print(i)

f<-length(subset(data$business_id,data$user_id==unfactor(a[i])))
  if(f>=50)
{
   n <- 50
}
if(f<50)
{
   n <- f
}
  for(j in 3:ncol(recomm_user))
  {
    if(!is.na(recomm_user[i,j]))
    {
      if(is.element(recomm_user[i,j],subset(data$business_id,data$user_id==unfactor(a[i]))))
    {
      print("yeah")
      num=num+1
      
    }
}
  
  }

m = num/n
accuracy = accuracy + m


}



final_accuracy = (accuracy/nrow(recomm_user))*100 

