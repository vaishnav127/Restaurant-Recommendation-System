newAttributes$BusinessAcceptsCreditCards
BusinessAcceptsBitcoin 
BikeParking 
Ambience
ByAppointmentOnly 
WiFi 
RestaurantsDelivery 
GoodForKids 
HappyHour 
Smoking 
Alcohol  
DogsAllowed 
HasTV 
OutdoorSeating 
WheelchairAccessible 
RestaurantsTakeOut
RestaurantsReservations
RestaurantsTableService
DriveThru
parking
pricelevel








my_data<-read.csv("G:/technical/python/rachana maam project/new data/yelp business/Food_business.csv")

library(stringr)
user_review_business<-my_data$attributes
newAttributes<-data.frame(matrix(nrow = length(user_review_business), ncol = 0))
newAttributes$BusinessAcceptsCreditCards <- NA
newAttributes$BusinessAcceptsBitcoin <- NA
newAttributes$BikeParking <- NA
newAttributes$Ambience <- NA
newAttributes$ByAppointmentOnly <- NA
newAttributes$WiFi <- NA
newAttributes$RestaurantsDelivery <- NA
newAttributes$GoodForKids <- NA
newAttributes$HappyHour <- NA
newAttributes$smoking <- NA
newAttributes$Alcohol <- NA
newAttributes$DogsAllowed <- NA
newAttributes$HasTV <- NA
newAttributes$OutdoorSeating <- NA
newAttributes$WheelchairAccessible <- NA
newAttributes$RestaurantsTakeOut <- NA
newAttributes$RestaurantsReservations <- NA
newAttributes$RestaurantsTableService <- NA
newAttributes$DriveThru <- NA
newAttributes$parking <- NA
newAttributes$pricelevel <- NA


p39 <- 'HappyHour: True'
p40 <- 'HappyHour: False'

p34 <- 'Smoking: no'
p26 <- 'Alcohol: full_bar'
p27 <- 'Alcohol: beer_and_wine'
p3 <- 'BusinessAcceptsCreditCards: True'
p1 <- 'BikeParking: True'
p2 <- 'BikeParking: False'
p4 <- 'BusinessAcceptsCreditCards: False'
p7 <- 'BusinessAcceptsBitcoin: False'
p8 <- 'BusinessAcceptsBitcoin: True'
#Ambience
p9 <- "'romantic': True"
p10 <- " 'classy': True"
p11 <- "'trendy': True"
p12 <- "'casual': True"
p13<-"'intimate': True" 
p14<- "'hipster': True"
p15<- "'divey': True" 
p16<- "'touristy': True"
p17<- "'upscale': True"

p18 <- 'ByAppointmentOnly: True'
p19 <- 'ByAppointmentOnly: False'
p20 <- 'WiFi: free'
p22 <- 'RestaurantsDelivery: False'
p23 <- 'RestaurantsDelivery: True'
p5 <- 'GoodForKids: True'
p6 <- 'GoodForKids: False'
p21 <- 'Wifi: no'
p33<- 'Smoking: outdoor'
p28<- "'garage': True"
p29 <-"'street': True"
p30 <- "'validated': True"
p31 <-"'lot': True" 
p32 <-"'valet': True"
p49<-'DogsAllowed: False'
p50<-'DogsAllowed: True'
p25<-'HasTV: True'
p24<-'HasTV: False'
p36<-'OutdoorSeating: False'
p35<-'OutdoorSeating: True'
p47<-'WheelchairAccessible: False'
p48<-'WheelchairAccessible: True'
p38<-'RestaurantsTakeOut: False'
p37<-'RestaurantsTakeOut: True'
p51<-'Alcohol: none'
p41<-'RestaurantsReservations: False'
p42<-'RestaurantsReservations: True'
p43<-'RestaurantsTableService: False'
p44<-'RestaurantsTableService: True'
p46<-'DriveThru: True'
p45<-'DriveThru: False'

# pricelevel

p52<-'RestaurantsPriceRange2: 1'



p53<-'RestaurantsPriceRange2: 2

'
p54<-'RestaurantsPriceRange2: 3'


p55<-'RestaurantsPriceRange2: 4'




for(i in 1:length(user_review_business))
{
  print(i)
  st<-user_review_business[i]
  if(is.na(st))
  {
    next()
  }
  #bike parking
  r <- str_match_all(st, p1)
  ans <- as.character(r)
  if(ans == 'BikeParking: True')
  {
    newAttributes$BikeParking[i] <- 'Yes';
  }
  
  r <- str_match_all(st, p2)
  ans <- as.character(r)
  if(ans == 'BikeParking: False')
  {
    newAttributes$BikeParking[i] <- 'No';
  }
  
  #business_accepts_credit_card
  
  r <- str_match_all(st, p3)
  ans <- as.character(r)
  if(ans == 'BusinessAcceptsCreditCards: True')
  {newAttributes$BusinessAcceptsCreditCards[i] <- 'Yes';
  }
  
  r <- str_match_all(st, p4)
  ans <- as.character(r)
  if(ans == 'BusinessAcceptsCreditCards: False')
  {newAttributes$BusinessAcceptsCreditCards[i] <- 'No';
  }
  
  #Good for kids
  
  r <- str_match_all(st, p5)
  ans <- as.character(r)
  if(ans == p5)
  {newAttributes$GoodForKids[i] <- 'Yes';
  }
  
  r <- str_match_all(st, p6)
  ans <- as.character(r)
  if(ans == p6)
  {newAttributes$GoodForKids[i] <- 'No';
  }
  
  #accepts bitcoin
  
  r <- str_match_all(st, p7)
  ans <- as.character(r)
  if(ans == 'BusinessAcceptsBitcoin: False')
  {newAttributes$BusinessAcceptsBitcoin[i] <- 'No';
  }
  
  r <- str_match_all(st, p8)
  ans <- as.character(r)
  if(ans == 'BusinessAcceptsBitcoin: True')
  {newAttributes$BusinessAcceptsBitcoin[i] <- 'Yes';
  }
  
  #ambience
  
  r <- str_match_all(st, p9)
  ans <- as.character(r)
  if(ans == "'romantic': True")
  {newAttributes$Ambience[i] <- 'Romantic';
  }
  
  r <- str_match_all(st, p10)
  ans <- as.character(r)
  if(ans == "'classy': True")
  {newAttributes$Ambience[i] <- 'classy';
  }
  
  r <- str_match_all(st, p11)
  ans <- as.character(r)
  if(ans == "'trendy': True")
  {newAttributes$Ambience[i] <- 'trendy';
  }
  
  r <- str_match_all(st, p12)
  ans <- as.character(r)
  if(ans == "'casual': True")
  {newAttributes$Ambience[i] <- 'casual';
  }
  
  r <- str_match_all(st, p13)
  ans <- as.character(r)
  if(ans == "'intimate': True")
  {newAttributes$Ambience[i] <- 'intimate';
  }
  
  r <- str_match_all(st, p14)
  ans <- as.character(r)
  if(ans == "'hipster': True")
  {newAttributes$Ambience[i] <- 'hipster';
  }
  
  r <- str_match_all(st, p15)
  ans <- as.character(r)
  if(ans == "'divey': True")
  {newAttributes$Ambience[i] <- 'divey';
  }
  
  r <- str_match_all(st, p16)
  ans <- as.character(r)
  if(ans == "'touristy': True")
  {newAttributes$Ambience[i] <- 'touristy';
  }
  
  r <- str_match_all(st, p17)
  ans <- as.character(r)
  if(ans == "'upscale': True")
  {newAttributes$Ambience[i] <- 'upscale';
  }
  
  #by appointement
  
  r <- str_match_all(st, p18)
  ans <- as.character(r)
  if(ans == p18)
  {newAttributes$ByAppointmentOnly[i] <- 'Yes';
  }
  
  r <- str_match_all(st, p19)
  ans <- as.character(r)
  if(ans == p19)
  {newAttributes$ByAppointmentOnly[i] <- 'No';
  }
  
  
  #wifi
  
  r <- str_match_all(st, p20)
  ans <- as.character(r)
  if(ans == p20)
  {newAttributes$WiFi[i] <- 'Yes';
  }
  
  r <- str_match_all(st, p21)
  ans <- as.character(r)
  if(ans == p21)
  {newAttributes$WiFi[i] <- 'No';
  }
  
  #res_delivery
  
  r <- str_match_all(st, p22)
  ans <- as.character(r)
  if(ans == p22)
  {newAttributes$RestaurantsDelivery[i] <- 'No';
  }
  
  r <- str_match_all(st, p23)
  ans <- as.character(r)
  if(ans == p23)
  {newAttributes$RestaurantsDelivery[i] <- 'Yes';
  }
  
  #HasTV
  r <- str_match_all(st, p24)
  ans <- as.character(r)
  if(ans == p24)
  {newAttributes$HasTV[i] <- 'No';
  }
  
  r <- str_match_all(st, p25)
  ans <- as.character(r)
  if(ans == p25)
  {newAttributes$HasTV[i] <- 'Yes';
  }
  
  #alcohol
  
  r <- str_match_all(st, p26)
  ans <- as.character(r)
  if(ans == p26)
  {newAttributes$Alcohol[i] <- 'full_bar';
  }
  
  r <- str_match_all(st, p27)
  ans <- as.character(r)
  if(ans == p27)
  {newAttributes$Alcohol[i] <- 'beer_and_wine';
  }
  
  r <- str_match_all(st, p51)
  ans <- as.character(r)
  if(ans == p51)
  {newAttributes$Alcohol[i] <- 'No';# error
  }
  
  #parking
  
  r <- str_match_all(st, p28)
  ans <- as.character(r)
  if(ans == p28)
  {newAttributes$parking[i] <- 'Yes';
  }
  else
  {
    newAttributes$parking[i] <- 'No';
  }
  
  r <- str_match_all(st, p29)
  ans <- as.character(r)
  if(ans == p29)
  {newAttributes$parking[i] <- 'Yes';
  }
  else
  {
    newAttributes$parking[i] <- 'No';
  }
  
  r <- str_match_all(st, p30)
  ans <- as.character(r)
  if(ans == p30)
  {newAttributes$parking[i] <- 'Yes';
  }
  else
  {
    newAttributes$parking[i] <- 'No';
  }
  
  r <- str_match_all(st, p31)
  ans <- as.character(r)
  if(ans == p31)
  {newAttributes$parking[i] <- 'Yes';
  }
  else
  {
    newAttributes$parking[i] <- 'No';
  }
  
  r <- str_match_all(st, p32)
  ans <- as.character(r)
  if(ans == p32)
  {newAttributes$parking[i] <- 'Yes';
  }
  else
  {
    newAttributes$parking[i] <- 'No';
  }
  
  #smoking
  
  r <- str_match_all(st, p33)
  ans <- as.character(r)
  if(ans == p33)
  {newAttributes$smoking[i] <- 'Outdoor';
  }
  
  r <- str_match_all(st, p34)
  ans <- as.character(r)
  if(ans == p34)
  {newAttributes$smoking[i] <- 'No';
  }
  
  #outdoors
  
  r <- str_match_all(st, p35)
  ans <- as.character(r)
  if(ans == p35)
  {newAttributes$OutdoorSeating[i] <- 'Yes';
  }
  
  r <- str_match_all(st, p36)
  ans <- as.character(r)
  if(ans == p36)
  {newAttributes$OutdoorSeating[i] <- 'No';
  }
  
  #take_out
  
  r <- str_match_all(st, p37)
  ans <- as.character(r)
  if(ans == p37)
  {newAttributes$RestaurantsTakeOut[i] <- 'Yes';
  }
  
  r <- str_match_all(st, p38)
  ans <- as.character(r)
  if(ans == p38)
  {newAttributes$RestaurantsTakeOut[i] <- 'No';
  }
  
  #happy hours
  
  r <- str_match_all(st, p39)
  ans <- as.character(r)
  if(ans == p39)
  {newAttributes$HappyHour[i] <- 'Yes';
  }
  
  
  r <- str_match_all(st, p40)
  ans <- as.character(r)
  if(ans == p40)
  {newAttributes$HappyHour[i] <- 'No';
  }
  
  #reservation
  
  r <- str_match_all(st, p41)
  ans <- as.character(r)
  if(ans == p41)
  {newAttributes$RestaurantsReservations[i] <- 'No';
  }
  
  r <- str_match_all(st, p42)
  ans <- as.character(r)
  if(ans == p42)
  {newAttributes$RestaurantsReservations[i] <- 'Yes';
  }
  
  #service
  
  r <- str_match_all(st, p43)
  ans <- as.character(r)
  if(ans == p43)
  {newAttributes$RestaurantsTableService[i] <- 'No';
  }
  
  r <- str_match_all(st, p44)
  ans <- as.character(r)
  if(ans == p44)
  {newAttributes$RestaurantsTableService[i] <- 'Yes';
  }
  
  #drive Through
  
  r <- str_match_all(st, p45)
  ans <- as.character(r)
  if(ans == p45)
  {newAttributes$DriveThru[i] <- 'No';
  }
  
  r <- str_match_all(st, p46)
  ans <- as.character(r)
  if(ans == p46)
  {newAttributes$DriveThru[i] <- 'Yes';
  }
  
  #accessible
  
  r <- str_match_all(st, p47)
  ans <- as.character(r)
  if(ans == p47)
  {newAttributes$WheelchairAccessible[i] <- 'No';
  }
  
  r <- str_match_all(st, p48)
  ans <- as.character(r)
  if(ans == p48)
  {newAttributes$WheelchairAccessible[i] <- 'Yes';
  }
  
  #allowed
  
  r <- str_match_all(st, p49)
  ans <- as.character(r)
  if(ans == p49)
  {newAttributes$DogsAllowed[i] <- 'No';
  }
  
  r <- str_match_all(st, p50)
  ans <- as.character(r)
  if(ans == p50)
  {newAttributes$DogsAllowed[i] <- 'Yes';
  }

 #pricelevel


r <- str_match_all(st, p52)
  ans <- as.character(r)
  if(ans == p52)
  {newAttributes$pricelevel[i] <- 1;
  }
  
  r <- str_match_all(st, p53)
  ans <- as.character(r)
  if(ans == p53)
  {newAttributes$pricelevel[i] <- 2;
  }

r <- str_match_all(st, p54)
  ans <- as.character(r)
  if(ans == p54)
  {newAttributes$pricelevel[i] <- 3;
  }
  
  r <- str_match_all(st, p55)
  ans <- as.character(r)
  if(ans == p55)
  {newAttributes$pricelevel[i] <- 4;
  }

}

write.csv(newAttributes,"G:/technical/python/rachana maam project/new data/yelp business/attributes.csv", row.names=FALSE)

y_data<-read.csv("G:/technical/python/rachana maam project/new data/yelp business/combined_with_maincategory.csv")
b<- cbind(y_data,newAttributes)
write.csv(b,"G:/technical/python/rachana maam project/new data/yelp business/Final_business_with_mycategory_attributes.csv", row.names=FALSE)



a<-read.csv("G:/technical/python/rachana maam project/new data/yelp business/yelp_business_avg_hours_food_business.csv")
b<-read.csv("G:/technical/python/rachana maam project/new data/yelp business/Final_business_with_mycategory_attributes.csv")

c<- cbind(b,a[,"average_hours"])
colnames(c)<- c(colnames(b),"average_hours")
write.csv(c,"G:/technical/python/rachana maam project/
new data/yelp business/Final_business_with_mycategory_attributes_avg_hours.csv", row.names=FALSE)

p<-read.csv("G:/technical/python/rachana maam project/new data/yelp business/Final_business_with_mycategory_attributes_avg_hours.csv")
q<-read.csv("G:/technical/python/rachana maam project/new data/yelp business/correct distance.csv")

r<- cbind(p,q)
write.csv(r,"G:/technical/python/rachana maam project/new data/yelp business/Final_business_with_mycategory_attributes_avg_hours_distance.csv", row.names=FALSE)







# lat lon distance

p<-read.csv("G:/technical/python/rachana maam project/new data/yelp business/postal_modified.csv")

q<-read.csv("G:/technical/python/rachana maam project/new data/yelp business/unique postal code.csv")

r<-read.csv("G:/technical/python/rachana maam project/new data/yelp business/lat lon unique_670.csv")
new<-data.frame(matrix(nrow = nrow(p), ncol = 0))


for(i in 1:nrow(p))
{

a<- p$postal code[i]
new$lat_cent[i]<- subset(r$lat,q$postal code==a)
new$lon_cent[i]<- subset(r$lon,q$postal code==a)


}

new$lat<- p$latitude
new$lon<- p$longitude
write.csv(new,"G:/technical/python/rachana maam project/new data/yelp business/lat lon centre all.csv", row.names=FALSE)

distance<-data.frame(matrix(nrow = nrow(new), ncol = 0))
library(geosphere)
for(j in 1:nrow(new))
{

lat1<-new$lat[j]
lon1<-new$lon[j]
lat2<-new$lat_cent[j]
lon2<-new$lon_cent[j]
#distm (c(-115.1592718, 36.1922841), c(-79.429089, 43.661054), fun = distHaversine)
distance$distance[j] = distm (c(lon1,lat1), c(lon2, lat2), fun = distHaversine)[,1] / 1609



}

write.csv(distance,"G:/technical/python/rachana maam project/new data/yelp business/correct distance.csv", row.names=FALSE)





