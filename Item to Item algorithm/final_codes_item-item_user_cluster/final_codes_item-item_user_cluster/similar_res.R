my_data1<-read.csv("Vash_FC.csv")   #new user file
my_data1<-my_data1[, 2:25]
library(varhandle)
############## FILE INPUT  #######################3
my_data3<-read.csv("Vash_FC.csv")
my_data3_subset<-my_data3[, 2:25]     #selecting the attributes similarity depends on
my_data1_subset<-my_data1
library(varhandle)
business_id<-levels(my_data3_subset[, 1])
my_matrix<-(matrix(, nrow = length(business_id), ncol = nrow(my_data1)))
colnames(my_matrix)<-my_data1$business_id
row.names(my_matrix)<-business_id
my_data1_subset_cuisines<-my_data1_subset[,24]
my_data3_subset_cuisines<-my_data3_subset[,24]

library(varhandle)
my_data1_subset[,3]<-unfactor(my_data1_subset[,3])
my_data1_subset[,2]<-unfactor(my_data1_subset[,2])
my_data1_subset[,4]<-unfactor(my_data1_subset[,4])
my_data1_subset[,5]<-unfactor(my_data1_subset[,5])
my_data1_subset[,6]<-unfactor(my_data1_subset[,6])
my_data1_subset[,8]<-unfactor(my_data1_subset[,8])
my_data1_subset[,10]<-unfactor(my_data1_subset[,10])
my_data1_subset[,11]<-unfactor(my_data1_subset[,11])
my_data1_subset[,12]<-unfactor(my_data1_subset[,12])
my_data1_subset[,13]<-unfactor(my_data1_subset[,13])
my_data1_subset[,16]<-unfactor(my_data1_subset[,16])
my_data1_subset[,15]<-unfactor(my_data1_subset[,15])
my_data1_subset[,17]<-unfactor(my_data1_subset[,17])
my_data1_subset[,18]<-unfactor(my_data1_subset[,18])
my_data1_subset[,19]<-unfactor(my_data1_subset[,19])
my_data1_subset[,20]<-unfactor(my_data1_subset[,20])
my_data1_subset[,21]<-unfactor(my_data1_subset[,21])
my_data1_subset[,22]<-unfactor(my_data1_subset[,22])
my_data1_subset[,23]<-unfactor(my_data1_subset[,23])
my_data1_subset[,7]<-unfactor(my_data1_subset[,7])
my_data1_subset[,9]<-unfactor(my_data1_subset[,9])
my_data1_subset[,14]<-unfactor(my_data1_subset[,14])
my_data1_subset_cuisines<-unfactor(my_data1_subset_cuisines)
my_data3_subset_cuisines<-unfactor(my_data3_subset_cuisines)
matrix<-matrix(, nrow = 2, ncol = 23)

for(l in 123:length(business_id))   #making a vector for each row
{print(l)
  i = which(my_data3_subset[, 1]==business_id[l])
  print("...................")
  print(i)
  if(length(i)==0)
  {
    next()
  }
  i = i[1]
  print(i)
  for(j in 1:nrow(my_data1))
  {print(j)
    if(is.na(my_data1_subset[j, colnames(my_data3_subset)[2]]))
    {
      my_data1_subset[j, colnames(my_data3_subset)[2]]<-0
    }
    if(is.na(my_data1_subset[j, colnames(my_data3_subset)[3]]))
    {
      my_data1_subset[j, colnames(my_data3_subset)[3]]<-0
    }
    if(is.na(my_data3_subset[i, 2]))
    {
      my_data3_subset[i, 2]<-0
    }
    if(is.na(my_data3_subset[i,3]))
    {
      my_data3_subset[i,3]<-0
    }
    matrix[1,1]<-my_data1_subset[j, colnames(my_data3_subset)[2]]
    matrix[1,2]<-my_data1_subset[j, colnames(my_data3_subset)[3]]
    matrix[2,1]<-my_data3_subset[i, 2]
    matrix[2,2]<-my_data3_subset[i,3]
    for(k in 4:ncol(my_data3_subset))
    {
      print(k)
      
      
      if(k==24)
      {
        if(is.na(my_data1_subset_cuisines[j])|is.na(my_data3_subset_cuisines[i])|my_data1_subset_cuisines[j]=="NUll"|my_data3_subset_cuisines[i]=="NUll"|my_data1_subset_cuisines[j]==""|my_data3_subset_cuisines[i]=="")
        {
          matrix[1,k-1]=1
          matrix[2,k-1]=0
          next()
        }
        
        
        temp1<-unlist(strsplit(my_data1_subset_cuisines[j], "'"))
        
        temp2<-unlist(strsplit(my_data3_subset_cuisines[i], "'"))
        
        matrix[1,k-1]=1
        flag=0
        for(x in 1:length(temp1))
        {
          if(x%%2==0)
          {
            for(y in 1:length(temp2))
            {
              if(y%%2==0)
              {
                if(temp1[x]==temp1[y])
                {
                  matrix[2,k-1]=1
                  flag=1
                  break
                }
              }
            }
            if(flag==1)
            {
              break
            }
          }
        }
        if(flag!=1)
        {
          matrix[2,k-1]=0
        }
        next()
      }
      if(k==7|k==9|k==14)
      {
        if(is.na(my_data1_subset[j, k])|my_data1_subset[j, k]=="NUll")
        {
          if(is.na(my_data3_subset[i,k])|my_data3_subset[i,k]=="NUll")
          {
            matrix[1,k-1]=1
            matrix[2,k-1]=0
          }
          else
          {
            matrix[1,k-1]=1
            matrix[2,k-1]=0
          }
          next()
        }
        matrix[1,k-1]=1
        if(is.na(my_data3_subset[i,k])|my_data3_subset[i,k]=="NUll")
        {
          matrix[2,k-1]=0
          next()
        }
        if(my_data1_subset[j, colnames(my_data3_subset)[k]]==my_data3_subset[i, k])
        {
          matrix[2,k-1]=1
        }
        else
        {
          matrix[2,k-1]=0
        }
        next()
      }
      if(is.na(my_data1_subset[j, colnames(my_data3_subset)[k]])|my_data1_subset[j, colnames(my_data3_subset)[k]]=="NUll")
      {
        matrix[1,k-1]=0
      }
      else if(my_data1_subset[j, colnames(my_data3_subset)[k]]=="Yes")
      {
        matrix[1,k-1]=1
      }
      else
      {
        matrix[1,k-1]=0
      }
      
      if(is.na(my_data3_subset[i, k])|my_data3_subset[i, k]=="NUll")
      {
        matrix[2,k-1]=0
      }
      else if(my_data3_subset[i,k]=="Yes")
      {
        matrix[2,k-1]=1
      }
      else
      {
        matrix[2,k-1]=0
      }
    }
    library(SnowballC)
    library(lsa)
    vec1<-matrix[1,]
    vec2<-matrix[2,]
    print(matrix)
    print(cosine(vec1,vec2)[1,1])
    print(".............................")
    my_matrix[my_data3_subset[i,"business_id"], my_data1_subset[j,"business_id"]]<-cosine(vec1,vec2)[1,1]
    
  }
}
main_matrix<-matrix(, nrow = ncol(my_matrix), ncol = 2)
for(i in 1:ncol(my_matrix))
{
  temp<- my_matrix[, i]
  temp<-sort(temp, decreasing = TRUE)
  
  temp<-names(temp)[2]
  main_matrix[i,1]<- colnames(my_matrix)[i]
  main_matrix[i,2]<-temp[1]
}
write.csv(main_matrix, "similar_business.csv")