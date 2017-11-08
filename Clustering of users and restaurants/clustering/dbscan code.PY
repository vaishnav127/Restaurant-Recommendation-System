print(__doc__)
import numpy as np
import pandas as pd
from sklearn.cluster import DBSCAN
from sklearn import metrics
from sklearn.datasets.samples_generator import make_blobs
from sklearn.preprocessing import StandardScaler

X= pd.read_csv("G:/technical/python/rachana maam project/new data/differennt clustering/YelpUserAttr.csv")
X = X[X.columns[1:]]

db = DBSCAN(eps=0.5, min_samples=5, metric='euclidean', algorithm='auto', leaf_size=30, p=None, n_jobs=1).fit(X)

core_samples_mask = np.zeros_like(db.labels_, dtype=bool)
core_samples_mask[db.core_sample_indices_] = True
labels = db.labels_

# Number of clusters in labels, ignoring noise if present.
n_clusters_ = len(set(labels)) - (1 if -1 in labels else 0)
print(n_clusters_)
Y= pd.DataFrame(labels)
Y.to_csv("G:/technical/python/rachana maam project/new data/differennt clustering/labels.csv", index = False)
print('Estimated number of clusters: %d' % n_clusters_)
print("Homogeneity: %0.3f" % metrics.homogeneity_score(labels_true, labels))
print("Completeness: %0.3f" % metrics.completeness_score(labels_true, labels))
print("V-measure: %0.3f" % metrics.v_measure_score(labels_true, labels))
print("Adjusted Rand Index: %0.3f"
      % metrics.adjusted_rand_score(labels_true, labels))
print("Adjusted Mutual Information: %0.3f"
      % metrics.adjusted_mutual_info_score(labels_true, labels))
print("Silhouette Coefficient: %0.3f"
      % metrics.silhouette_score(X, labels))





library(plyr)
count(X,vars = colnames(X))





"User_id"                    "Salvadoran"                
  "Wine.Bar"                   "Arabian"                   
   "Austrian"                   "Argentine"                 
   "Belgian"                    "Teppanyaki"                
   "Dim.Sum"                    "French"                    
  "Hot.pot"                    "Bar"                       
  "Dominican"                  "Filipino"                  
 [15] "Canadian"                   "Bakery"                    
 [17] "Ice.Cream"                  "Italian"                   
 [19] "Mediterranean"              "Moroccan"                  
 [21] "Desserts"                   "Nepalese"                  
 [23] "Polish"                     "Tex.Mex"                   
 [25] "Caribbean"                  "Pasta.Shop"                
 [27] "Champagne.Bar"              "Indonesian"                
 [29] "Salad.Bar"                  "Hawaiian"                  
 [31] "Iberian"                    "Slovakian"                 
 [33] "Whiskey.Bar"                "Thai"                      
 [35] "European"                   "Cuban"                     
 [37] "Uzbek"                      "Soups"                     
 [39] "Cantonese"                  "Tiki.Bar"                  
 [41] "Burgers"                    "Burmese"                   
 [43] "Steakhouse"                 "African"                   
 [45] "Beer.Bar"                   "Singaporean"               
 [47] "Kebab"                      "Greek"                     
 [49] "Asian.Fusion"               "Ethiopian"                 
 [51] "Shanghaniese"               "Cafe"                      
 [53] "Sushi.Bar"                  "Turkish"                   
 [55] "Middle.Eastern"             "Bangladeshi"               
 [57] "Lounge"                     "Indian"                    
 [59] "Latin.American"             "Brazilian"                 
 [61] "Korean"                     "Hookah.Bar"                
 [63] "Barbeque"                   "Russian"                   
 [65] "Afghan"                     "Malaysian"                 
 [67] "Kosher"                     "American"                  
 [69] "Thai.Vietnamese"            "Irish"                     
 [71] "Chinese"                    "German"                    
 [73] "Taiwanese"                  "Czech"                     
 [75] "Persian"                    "Mongolian"                 
 [77] "Basque"                     "Vietnamese"                
 [79] "Donuts"                     "Scottish"                  
 [81] "Waffles"                    "Sports.Bar"                
 [83] "Tapas.Bar"                  "Fast.Food"                 
 [85] "Australian"                 "Venezuelan"                
 [87] "Pizza"                      "Ukranian"                  
 [89] "Egyptian"                   "Deli"                      
 [91] "Tuscan"                     "Lebanese"                  
 [93] "Diner"                      "Seafood"                   
 [95] "Food.truck"                 "British"                   
 [97] "Portuguese"                 "Columbian"                 
 [99] "Mexican"                    "Peruvian"                  
[101] "Japanese"                   "Juice.Bar"                 
[103] "Cambodian"                  "Hot.Dogs"                  
[105] "Spanish"                    "Nicaraguan"                
[107] "review_count"               "stars"                     
[109] "BusinessAcceptsCreditCards" "BusinessAcceptsBitcoin"    
[111] "BikeParking"                "Ambience"                  
[113] "ByAppointmentOnly"          "WiFi"                      
[115] "RestaurantsDelivery"        "GoodForKids"               
[117] "HappyHour"                  "smoking"                   
[119] "Alcohol"                    "DogsAllowed"               
[121] "HasTV"                      "OutdoorSeating"            
[123] "WheelchairAccessible"       "RestaurantsTakeOut"        
[125] "RestaurantsReservations"    "RestaurantsTableService"   
[127] "DriveThru"                  "parking"                   
[129] "pricelevel"                 "average_hours"             
[131] "count"                      "AvgStars"                  
[133] "AvgReviewCount"             "AvgStarsGiven"             
[135] "Diff"                      