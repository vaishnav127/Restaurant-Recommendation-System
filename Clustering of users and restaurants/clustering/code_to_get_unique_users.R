
big_data<-read.csv("G:/technical/python/rachana maam project/new data/sample_FC200.csv")


user<-levels(big_data$user_id)

columnMatrix2 <- data.frame(matrix(ncol=29,nrow=length(user)))

columnMatrix2<-cbind(columnMatrix2,big_data$latitude[1:length(user)], big_data$longitude[1:length(user)],big_data$user_id[1:length(user)],big_data$useful.y[1:length(user)],big_data$compliment_photos[1:length(user)],big_data$compliment_list[1:length(user)],big_data$compliment_funny[1:length(user)],big_data$compliment_plain[1:length(user)],big_data$review_count[1:length(user)],big_data$elite[1:length(user)],big_data$fans[1:length(user)],big_data$type.y[1:length(user)],big_data$compliment_note[1:length(user)],big_data$funny.y[1:length(user)],big_data$compliment_writer[1:length(user)],big_data$compliment_cute[1:length(user)],big_data$average_stars[1:length(user)],big_data$compliment_more[1:length(user)],big_data$friends[1:length(user)],big_data$compliment_hot[1:length(user)],big_data$cool.y[1:length(user)],big_data$compliment_profile[1:length(user)],big_data$compliment_cool[1:length(user)])

colnames(columnMatrix2)<-c("business_id","Cuisines","BusinessAcceptsCreditCards","BusinessAcceptsBitcoin","BikeParking","Ambience","ByAppointmentOnly","WiFi","RestaurantsDelivery","GoodForKids","HappyHour" ,"Smoking" ,"Alcohol" ,"DogsAllowed" ,"HasTV" ,"OutdoorSeating","WheelchairAccessible","RestaurantsTakeOut" ,"RestaurantsReservations","RestaurantsTableService",
"DriveThru","parking","stars.y","date","useful.x","type.x","cool.x","funny.x","stars.x","latitude","longitude","user_id","useful.y","compliment_photos","compliment_list","compliment_funny","compliment_plain","review_count","elite","fans","type.y","compliment_note","funny.y","compliment_writer","compliment_cute","average_stars","compliment_more","friends","compliment_hot","cool.y","compliment_profile","compliment_cool")



for (i in 1:length(user))
{ print(i)

  temp = subset(big_data, (big_data$user_id==user[i]))

  
  columnMatrix2[i, "latitude"]<-mean(temp[,"latitude"])
  columnMatrix2[i, "longitude"]<-mean(temp[,"longitude"])

  a<-c("business_id","Ambience","Alcohol","stars.y","date","useful.x","type.x","cool.x","funny.x","stars.x")



  for (k in 1:10)
  {
if(length(temp[!is.na(temp[,a[k]]),a[k]]) !=0){ # ERROR
    tempoo <- temp[!is.na(temp[,a[k]]),a[k]]
    
    gfk<-paste("[u'", tempoo[1], sep="")
    for(d in 2:length(tempoo))
    {
      gfk<-paste(gfk, tempoo[d], sep="',u'")
    }
    gfk<-paste(gfk, "']", sep="")
  
  columnMatrix2[i, a[k]]<- gfk
}
if(length(temp[!is.na(temp[,a[k]]),a[k]]) ==0){
columnMatrix2[i, a[k]]<- NA
}
}


b<-c("BusinessAcceptsCreditCards","BusinessAcceptsBitcoin","BikeParking","ByAppointmentOnly","WiFi","RestaurantsDelivery","GoodForKids","HappyHour" ,"Smoking"  ,"DogsAllowed" ,"HasTV" ,"OutdoorSeating","WheelchairAccessible"
,"RestaurantsTakeOut" ,"RestaurantsReservations","RestaurantsTableService","DriveThru","parking")

for (m in 1:18)
  {
    if(length(temp[!is.na(temp[,b[m]]),b[m]]) !=0){
    temp1 <- temp[!is.na(temp[,b[m]]),b[m]]
    
    p<- length(temp1[temp1 == "Yes"])
    q<- length(temp1[temp1 == "No"])

    if(p>q)
{
columnMatrix2[i, b[m]]<- "Yes"
}
if(q>p)
{
columnMatrix2[i, b[m]]<- "No"
}
}
if(length(temp[!is.na(temp[,b[m]]),b[m]]) ==0){
columnMatrix2[i, b[m]]<- NA
}

}



  columnMatrix2[i,c("user_id","useful.y","compliment_photos","compliment_list","compliment_funny","compliment_plain","review_count","elite","fans","type.y","compliment_note","funny.y","compliment_writer","compliment_cute","average_stars","compliment_more","friends","compliment_hot","cool.y","compliment_profile","compliment_cool")]<-temp[1, c("user_id","useful.y","compliment_photos","compliment_list","compliment_funny","compliment_plain","review_count","elite","fans","type.y","compliment_note","funny.y","compliment_writer","compliment_cute","average_stars","compliment_more","friends","compliment_hot","cool.y","compliment_profile","compliment_cool")]
  
#Cuisines  

if(length(temp[!is.na(temp[,"Cuisines"]),"Cuisines"]) !=0){
tempo <- temp[!is.na(temp[,"Cuisines"]),"Cuisines"]  
a<- tempo[1]
gf<- gsub("]", "", a)

    for(d in 2:length(tempo))
    {
      b<- tempo[d]
      y<- gsub("\\[|\\]", "", b)
      gf<-paste(gf, y, sep=", ")
    }

 
 gf<-paste(gf,"]", sep="")

columnMatrix2[i, "Cuisines"]<- gf
}

if(length(temp[!is.na(temp[,"Cuisines"]),"Cuisines"]) ==0)
{
columnMatrix2[i, "Cuisines"]<- NA
}


  
}

write.csv(columnMatrix2, "G:/technical/python/rachana maam project/new data/unique_user_file_restraunt_based.csv", row.names = TRUE)

