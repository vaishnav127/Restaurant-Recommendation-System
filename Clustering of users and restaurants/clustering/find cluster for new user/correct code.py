attribute_3=read.csv("D:/rachana maam/find the new cluster for user/arranged_attribute_6000.csv")#attribute3 wali files
library(varhandle)
new_user_preferences=read.csv(D:/rachana maam/find the new cluster for user/preferences.csv)#preferences of new users data

#new file made from preferences of user having same columns as attribute_3
new_user=data.frame(matrix(ncol=(ncol(attribute_3)),nrow=nrow(new_user_preferences)))

colnames(new_user)=colnames(attribute_3)

new_user$user_id <- new_user_preferences$user_id
for(i in 1:nrow(new_user_preferences))
{ #copying the user name as it is
  new_user[i,2]=NA
  
  #copying the following columns as it is
  new_user[i,c("BusinessAcceptsCreditCards"	,"BikeParking"	,"WiFi"	,"RestaurantsDelivery",
  "HappyHour","Smoking"	,"Alcohol","parking","DogsAllowed","HasTV","OutdoorSeating"	,"WheelchairAccessible","RestaurantsTakeOut",
  "RestaurantsReservations"	,"RestaurantsTableService"	,"DriveThru")]=new_user_preferences[i,c("BusinessAcceptsCreditCards","BikeParking","WiFi",
  "RestaurantsDelivery","HappyHour","Smoking","Alcohol","parking","DogsAllowed","HasTV","OutdoorSeating","WheelchairAccessible","RestaurantsTakeOut",
  "RestaurantsReservations"	,"RestaurantsTableService","DriveThru")]
  
  #initaially put 0 in all columns made from cuisines
   new_user[i,c(3:109)]=0
  #spliting the cuisines column and putting 1 where it matches the colname assuming that new user gives it in cuisine form
   x=new_user_preferences$Cuisines[i]
   x=as.vector(x)
   x=strsplit(x,",")
   x=unlist(x)
   x<- gsub("\\[|\\]", "", x)
   x<- gsub("\\'|\\'", "", x)
   x<- gsub(" ","",x)  
   
   for(j in 1:length(x))
   { new_user[i,x[j]]=1  
   }
}

cen=read.csv("D:/rachana maam/find the new cluster for user/Cluster_centroids.csv")#centroid wali file
#new file made from centroids having same columns as attribute_3
new_cen=data.frame(matrix(ncol=(ncol(attribute_3)),nrow=nrow(cen)))
colnames(new_cen)=colnames(attribute_3)

#copying the following columns as it is from centroids
new_cen$BusinessAcceptsCreditCards=unfactor(cen$BusinessAcceptsCreditCards)
new_cen$BikeParking=unfactor(cen$BikeParking)
new_cen$WiFi=unfactor(cen$WiFi)
new_cen$RestaurantsDelivery=unfactor(cen$RestaurantsDelivery)
new_cen$HappyHour=unfactor(cen$HappyHour)
new_cen$Smoking=unfactor(cen$Smoking)
new_cen$Alcohol=unfactor(cen$Alcohol)
new_cen$parking=unfactor(cen$parking)
new_cen$DogsAllowed=unfactor(cen$DogsAllowed)
new_cen$HasTV=unfactor(cen$HasTV)
new_cen$OutdoorSeating=unfactor(cen$OutdoorSeating)
new_cen$WheelchairAccessible=unfactor(cen$WheelchairAccessible)
new_cen$RestaurantsTakeOut=unfactor(cen$RestaurantsTakeOut)
new_cen$RestaurantsReservations=unfactor(cen$RestaurantsReservations)
new_cen$RestaurantsTableService=unfactor(cen$RestaurantsTableService)
new_cen$DriveThru=unfactor(cen$DriveThru)

#change colnames of few columns
colnames(new_cen)[33] <- "SaladBar"
colnames(new_cen)[37] <- "WhiskeyBar"
colnames(new_cen)[86] <- "TapasBar"
colnames(new_cen)[85] <- "SportsBar"

for(i in 1:nrow(cen))
{
  #initaially put 0 in all columns made from cuisines
  new_cen[i,c(3:109)]=0
  
  #spliting the cuisine and put where it matches the colname
   x=cen$Cuisines[i]
   x=as.vector(x)
   x=strsplit(x,",")
   x=unlist(x)
   x<- gsub("\\[|\\]", "", x)
   x<- gsub("\\'|\\'", "", x)
   x<- gsub(" ","",x)  
   
   for(j in 1:length(x))
   { new_cen[i,x[j]]=1  
   }  
}

high_weighted_columns=c(colnames(attribute_3)[3:109],"Smoking","Alcohol")

###################### replace yes no entries of centroids ##################
for(i in c("parking","RestaurantsDelivery","RestaurantsReservations","OutdoorSeating","WiFi","RestaurantsTakeOut","RestaurantsTableService",
           "BikeParking","HasTV","WheelchairAccessible","HappyHour","DriveThru","DogsAllowed","BusinessAcceptsCreditCards"))

{
new_cen[,i][new_cen[,i] == "No"] <- 0
new_cen[,i][new_cen[,i] == "Yes"] <- 1
}
new_cen$Smoking[new_cen$Smoking == "No"] <- 0
new_cen$Smoking[new_cen$Smoking == "Outdoor"] <- 1

new_cen$Alcohol[new_cen$Alcohol == "full_bar"] <- 1
new_cen$Alcohol[new_cen$Alcohol == "beer_and_wine"] <- 1
new_cen$Alcohol[new_cen$Alcohol == "No"] <- 0
########################### predict cluster with min distance ##############################
  cluster_number=NULL
  for(i in 1:nrow(new_user))
  {distance=NULL
   for(j in 1:nrow(new_cen))
   {
     w=0
     sum=0
	 for(k in 3:ncol(new_cen))
	 {sum=sum+( new_user[i,k]-as.numeric(new_cen[j,k]) )^2
	 }
	 w=sqrt(sum)
     distance[j]=w	 
   }
  s=which(distance == min(distance))
  if(length(s)==1)
  {cluster_number[i]=s[1]
  }
  else
  {
  distance_special=NULL
   for(z in 1:length(s))
   {   w_special=0
       
       sum_special=0
       for(j in c(3:109,115,116))
	   {sum_special=sum_special+( new_user[i,j]-as.numeric(new_cen[s[z],j]) )^2
	   }
	   w_special=sqrt(sum_special)
       distance_special[z]=w_special
   }
     s_special=which.min(distance_special)
	 cluster_number[i]=s[z]
  }
  }
 
  
w=new_user[,1]
q=cbind(w,cluster_number)

write.csv(w,"D:/rachana maam/find the new cluster for user/new_users_clusters.csv",row.names=FALSE)
  
  







