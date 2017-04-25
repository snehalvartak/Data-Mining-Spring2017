
# Objective:
# 1. Compute the sample covariance matrix for the trees dataset
# 2. Perform Principal Component Analysis (PCA) by computing Singular Value Decomposition (SVD) of the sample covariance matrix.
# 3. Find an approximate linear model between the mean centered variables, using the columns of U associated with the smallest diagonal element.


#Set the data set
data(trees)
X = as.matrix(trees)

#plot of the variables in the dataset, to get a view if how the variables relate
pairs(X)

#mean center the variables prior to computing covariance
Xc=scale(X,center=TRUE,scale=FALSE)
pairs(Xc)

#sample covariance matrix computation
n = nrow(Xc)
scov = t(Xc) %*% Xc/n 

#PCA by computing SVD of sample covariance matrix 
s = svd(scov)
D = diag(s$d) 
#The diagonal matrix is
D
#The U matrix is 
s$u 


# The column of U associated with the smallest diagonal element is the 3rd column of U. 
# Since the #value of diagonal element tends to zero the variance in the 3rd direction also tends to zero. 
# So to approximate we can write the linear model between the three # centered variables as
# -0.98025557 * girth -0.04654394* height +0.19217878 * volume = 0 # i.e 0.19217878*volume = 0.98025557 * girth + 0.04654394* height
