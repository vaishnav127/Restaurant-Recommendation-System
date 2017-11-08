#attributes_matrix file is file that will contain 8 columns user_id,buisness_id,review_count,stars,distance,pricelevel,average hours,rating this will work as the table like rating table for the slope one
# for finding the square matrix that will be of items that will containing the avg difference of items
rate<-read.csv("C:/Users/dell/Downloads/.csv")
rate<- rate[,2:ncol(rate)]
avg_diff_mat <- data.frame(matrix(ncol=(ncol(rate)-1),nrow=(ncol(rate)-1)))
row.names(avg_diff_mat)<- colnames(rate)[2:ncol(rate)]
colnames(avg_diff_mat)<- colnames(rate)[2:ncol(rate)]
no_users <- data.frame(matrix(ncol=(ncol(rate)-1),nrow= (ncol(rate)-1)))
row.names(no_users)<- colnames(rate)
colnames(no_users)<- colnames(rate)

for ( i in 1:(ncol(rate)-1))
{

for ( j in 1:(ncol(rate)-1))
{
a<- rate[,i]

b<- rate[,j]
if(length(a[(!is.na(a)&!is.na(b))]) !=0)
{
a1<- a[(!is.na(a)&!is.na(b))]# col
b1<- b[(!is.na(a)&!is.na(b))]# col
n <- length(a1)
no_users[i,j]<- n
c<- a1-b1
sum <- sum(c)
final<- sum/n
avg_diff_mat[i,j]<- final
}
}
}
write.csv(avg_diff_mat,"C:/Users/dell/Downloads/avg_diff_items.csv", row.names=FALSE)

write.csv(no_users,"C:/Users/dell/Downloads/no_of_users.csv", row.names=FALSE)






rate<-read.csv("C:/Users/dell/Downloads/.csv")

avg_diff_mat<-read.csv("C:/Users/dell/Downloads/avg_diff_items.csv")
no_users<-read.csv("C:/Users/dell/Downloads/no_of_users.csv")

prediction_score <- data.frame(matrix(ncol=ncol(rate),nrow=nrow(rate)))
colnames(prediction_score)<- colnames(rate)
prediction_score$user_id <- rate[,"user_id"]
#prediction_score$business_id <- rate[,"business_id"]


rate<- rate[,2:ncol(rate)]
for ( i in 1:nrow(rate))
  
{
print(i)

for ( j in 1:ncol(rate))
 {
print(j)
prediction_score[i,j+1]<- rate[i,j]
if(length(rate[i,!is.na(rate[i,])])!=0)
  
  {
p<- rate[i,!is.na(rate[i,])]# row
q <- avg_diff_mat[j,!is.na(rate[i,]]# row
r<- no_users[j,!is.na(rate[i,]]

if(length(p[!is.na(q)])!=0)
  {
p<- p[!is.na(q)&!is.na(r)]
q<- q[!is.na(q)&!is.na(r)]
r<- r[!is.na(q)&!is.na(r)]
s<- p+q  # row,predicted rating 
num <- sum(s*r)# sum(row * row)
den <- sum(r)

if(den!=0)
	  {
	  if(is.na(rate[i,j))
	  {
prediction_score[i,j+1]<- num/den
}


}


}
}

}
  
}

write.csv(prediction_score,"G:/technical/python/rachana maam project/new data/slope one/predicted_attributes_matrix.csv", row.names=FALSE)






