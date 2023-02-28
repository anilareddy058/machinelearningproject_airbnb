Myfile2="/Users/anilareddy/Downloads/project/portland/cleaned datasets/cleaned_listings.csv"
df <- read.csv(Myfile2)
summary(df)
str(df)
df<- select(df, accommodates, bedrooms, beds, price) 

(Dist1<- dist(df, method = "minkowski", p=1)) ##Manhattan
(Dist2<- dist(df, method = "minkowski", p=2)) #Euclidean
(DistE<- dist(df, method = "euclidean")) #same as p = 2

(df <- as.data.frame(apply(df[,1:3 ], 2, ##2 for col
                                          function(x) (x - min(x))/(max(x)-min(x)))))


(df_scale<-scale(df))

(df<- dist(df, method = "minkowski", p=2)) #Euclidean
(HClust_Ward_Euc_N_3D <- hclust(df, method = "average" ))
plot(HClust_Ward_Euc_N_3D, cex=0.9, hang=-1, main = "Minkowski p=2 (Euclidean)")
rect.hclust(HClust_Ward_Euc_N_3D, k=4)

dist_C <- stats::dist(df, method="manhattan")
HClust_Ward_CosSim_N_3D <- hclust(dist_C, method="ward.D2")
plot(HClust_Ward_CosSim_N_3D, cex=.7, hang=-30,main = "Manhattan")
rect.hclust(HClust_Ward_CosSim_N_3D, k=2)









##################################################################
##################################################################
d=dist(df)
hc=hclust(d,method="average")
plot(hc)

df[is.na(df)] <- 0


Hierar_cl <- hclust(df, method = "average")
Hierar_cl

Hierar_cl[is.na(Hierar_cl)] <- 0

# Plotting dendrogram
plot(Hierar_cl)

# Choosing no. of clusters
# Cutting tree by height
abline(h = 110, col = "green")

# Cutting tree by no. of clusters
fit <- cutree(Hierar_cl, k = 3 )
fit

table(fit)
rect.hclust(Hierar_cl, k = 3, border = "green")