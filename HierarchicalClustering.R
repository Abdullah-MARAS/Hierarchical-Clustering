#Hierarchical Clusterin Example from Machine Learning Course
# I have used the dataset called ?USArrests?. This is a public dataset available in R. To see the dataset, use the library called ?datasets?. For more details on the dataset see https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/USArrests.html
install.packages("datasets")
library(datasets)

install.packages("tidyverse") # data manipulation
library(tidyverse)  

install.packages("cluster") # clustering algorithms
library(cluster)    

install.packages("factoextra") # clustering visualization
library(factoextra) 

install.packages("dendextend") # for comparing two dendrograms
library(dendextend) 

# Here, we'll use the built-in R data set USArrests, which contains statistics in arrests per 100,000 residents for 
#assault, murder, and rape in each of the 50 US states in 1973.
#It includes also the percent of the population living in urban areas
df <- USArrests

# To remove any missing value that might be present in the data, type this
df <- na.omit(df)
head(df)

# As we don't want the clustering algorithm to depend to an arbitrary variable unit, 
#we start by scaling/standardizing the data using the R function scale
df <- scale(df)
head(df)

# The idea in Hierarchical Clustering is to build a binary tree of the data that 
#successively merges similar groups of points.Visualizing this tree provides a useful summary of the data

# Aim of our Hierarchical Cluster analysis is on "USArrests" to classify states based on their criminal similarity. 
# Hierarchical clustering can be divided into two main types: agglomerative and divisive.
# Agglomerative clustering: It's also known as AGNES (Agglomerative Nesting). 
#It works in a bottom-up manner. That is, each object is initially considered as a single-element cluster (leaf).
#At each step of the algorithm, the two clusters that are the most similar are combined into a new bigger cluster (nodes). This procedure is iterated until all points are member of just one single big cluster (root) (see figure below). The result is a tree which can be plotted as a dendrogram.https://uc-r.github.io/hc_clustering
"""
Divisive hierarchical clustering: It's also known as DIANA (Divise Analysis) and it works in a top-down manner. 
The algorithm is an inverse order of AGNES. 
It begins with the root, in which all objects are included in a single cluster. 
At each step of iteration, the most heterogeneous cluster is divided into two. 
The process is iterated until all objects are in their own cluster (see figure below). 
https://uc-r.github.io/hc_clustering
"""
# Note that agglomerative clustering is good at identifying small clusters. 
#Divisive hierarchical clustering is good at identifying large clusters.
"""
There are different functions available in R for computing hierarchical clustering. The commonly used functions are:
hclust [in stats package] and agnes [in cluster package] for agglomerative hierarchical clustering (HC)
diana [in cluster package] for divisive HC"""

# Agglomerative Hierarchical Clustering
"""
We can perform agglomerative HC with hclust.
First we compute the dissimilarity values with dist and then feed these values into hclust
and specify the agglomeration method to be used (i.e. complete, average, single, ward.D). 
We can then plot the dendrogram.
"""
# Dissimilarity matrix
d <- dist(df, method = "euclidean")

# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "complete" )

# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)

"""
Alternatively, we can use the agnes function. These functions behave very similarly; 
however, with the agnes function you can also get the agglomerative coefficient, 
which measures the amount of clustering structure found (values closer to 1 suggest strong clustering structure).
"""
# Compute with agnes
hc2 <- agnes(df, method = "complete")

# Agglomerative coefficient
hc2$ac   # [1] 0.8531583

# This allows us to find certain hierarchical clustering methods that can identify stronger clustering structures. 
#Here we see that Ward?s method identifies the strongest clustering structure of the four methods assessed.

# methods to assess
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
  agnes(df, method = x)$ac
}

map_dbl(m, ac)

#Similar to before we can visualize the dendrogram
# plot dendrogram
hc3 <- agnes(df, method = "ward")
pltree(hc3, cex = 0.6, hang = -1, main = "Dendrogram of agnes") 


#Divisive Hierarchical Clustering
#The R function diana provided by the cluster package allows us to perform divisive hierarchical clustering.
#diana works similar to agnes; however, there is no method to provide.

# compute divisive hierarchical clustering
hc4 <- diana(df)

# Divise coefficient; amount of clustering structure found
hc4$dc  # [1] 0.8514345

# plot dendrogram
pltree(hc4, cex = 0.6, hang = -1, main = "Dendrogram of diana")


# Working with Dendrograms
# In the dendrogram displayed above, each leaf corresponds to one observation. 
#As we move up the tree, observations that are similar to each other are combined into branches, 
#which are themselves fused at a higher height.
"""
The height of the fusion, provided on the vertical axis, indicates the (dis)similarity between two observations. 
The higher the height of the fusion, the less similar the observations are.
Note that, conclusions about the proximity of two observations can be drawn only based on the height 
where branches containing those two observations first are fused. 
We cannot use the proximity of two observations along the horizontal axis as a criteria of their similarity.
"""
# The height of the cut to the dendrogram controls the number of clusters obtained. 
#It plays the same role as the k in k-means clustering. In order to identify sub-groups (i.e. clusters), 
#we can cut the dendrogram with cutree

# Ward's method
hc5 <- hclust(d, method = "ward.D2" )

# Cut tree into 4 groups
sub_grp <- cutree(hc5, k = 4)

# Number of members in each cluster
table(sub_grp)


#We can also use the cutree output to add the the cluster each observation belongs to to our original data.
USArrests %>%
  mutate(cluster = sub_grp) %>%
  head

# It's also possible to draw the dendrogram with a border around the 4 clusters.
#The argument border is used to specify the border colors for the rectangles
plot(hc5, cex = 0.6)
rect.hclust(hc5, k = 4, border = 2:5)

# we can also use the fviz_cluster function from the factoextra package to visualize the result in a scatter plot.
fviz_cluster(list(data = df, cluster = sub_grp))

#To use cutree with agnes and diana you can perform the following
# Cut agnes() tree into 4 groups
hc_a <- agnes(df, method = "ward")
cutree(as.hclust(hc_a), k = 4)

# Cut diana() tree into 4 groups
hc_d <- diana(df)
cutree(as.hclust(hc_d), k = 4)

#Lastly, we can also compare two dendrograms. Here we compare hierarchical clustering with complete linkage versus
#Ward's method. The function tanglegram plots two dendrograms, side by side, with their labels connected by lines.

# Compute distance matrix
res.dist <- dist(df, method = "euclidean")

# Compute 2 hierarchical clusterings
hc1 <- hclust(res.dist, method = "complete")
hc2 <- hclust(res.dist, method = "ward.D2")

# Create two dendrograms
dend1 <- as.dendrogram (hc1)
dend2 <- as.dendrogram (hc2)

tanglegram(dend1, dend2)


# The output displays unique nodes, with a combination of labels/items not present in the other tree, 
#highlighted with dashed lines. The quality of the alignment of the two trees can be measured using the function 
#entanglement. Entanglement is a measure between 1 (full entanglement) and 0 (no entanglement).
#A lower entanglement coefficient corresponds to a good alignment. 
#The output of tanglegram can be customized using many other options as follow

dend_list <- dendlist(dend1, dend2)

tanglegram(dend1, dend2,
           highlight_distinct_edges = FALSE, # Turn-off dashed lines
           common_subtrees_color_lines = FALSE, # Turn-off line colors
           common_subtrees_color_branches = TRUE, # Color common branches 
           main = paste("entanglement =", round(entanglement(dend_list), 2))
)
