# Restaurant-Recommendation-System
Developed an efficient Recommendation Engine for recommending food restaurants to the users based on their preferences, past history, reviews and users data for restaurants from NewYork and San Francisco. Algorithm involved hybrid of Content based, Item based and User based recommendation models.

Algorithms used and steps followed are as follows :

1.Slope one algorithm
2.Clustering algoes - k-modes(for categorical) , k-means, dbscan
3.User to User
4.Item to Item
5.Content Based Recommendation 
6.Hybrid Recommendation Algorithm

Dataset:

Yelp dataset contains three types of data:- User Review Data , User Data ,Data of Restaurants.

Step 1:

Analysis of data by drawing plots and based on them making inferences eg :how many restaurants are there in a particular area having sea food and their price range 
This is done in the "plots with insights and situations" folder.

Step 2:
Data cleaning and transformation.It includes find null percentages in the attributes used and with the help of google api using using latitude , longitude 
of restaurants we got the city , country , neighbourhood of the restaurants.Also,used the slope one algorithm which is basically used to find the rating given by a user to a restaurant.
But here we used to predict the missing attributes of restaurants and handling null values.This is present in the "data preprocessing and cleaning" folder. 

Step 3:
Clustering of users and Clustering of items.For clustering of users first we prepared data which is having attributes of the users as well as the attributes of the restaurants 
that are visited by the users.Then taking the mode,mean of the attributes we created a single value of that attribute to a users who likes som many restaurants like out of 7 restaurants
visited 5 of them had seafood so we used the mode value which is seafood to fill attributes like type of food liked by user.For new users we then checked to which cluster the
new user belonged on the basis of attribute of that user by calculating the distance of the attributes of new user with the centroids of all clusters.Clustering helped us to reduce the work space.
This is done in the "clustering" folder. 

Step 4:
User to User Recommendation- Here we first calculated the  rating matrix and made a similairty matrix of only those users that belong to same cluster.Then,
rating of a user to a restaurant was calculated on the basis of the other users in the same cluster 
who have rated that particular restaurant and using the similarity to between user and rating given by a user to calculate top 10 rating restaurant which 
are finally recommended to user.

Step 5:
Item to Item:- 
Here we found similar restaurant to a particular restaurant and then on the basis of similaity to a restaurant and rating given by the particular user found the predicted ratings
for a restaurant by a user.

Step 6:
Content based Recommendation- Here,we first found clusters of restaurants and assigned a cluster number to a user on the basis of the similaity of attributes of that user to
the centorids of the clusters.After assigning clusters we will find the similarity of that user from those restaurants that are present in the cluster which is allocated to that user 
and based on the similarity we recommended top 50 restaurant to that user.

Step 7:
Hybrid Collaborative algorithm:
Here,the predictions of user to user algorithm and item to item algorithm were combined by using control factor a.

********Detailed Pseudo codes and working of algorithms is present in the "Algorithms" Folder ************

To calculate accuracy we removed ratings given by 25% users to all the restaurants,and calculated recommendatins for those 25% users.Finally 
checked whether recommended restaurants to a user contain those restaurants for which we initially removed the ratings or not.


