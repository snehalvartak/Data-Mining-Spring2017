# Objective
# Data is generated randomly
# Number of Clusters K = 4 2-d clsuters
# Each cluster has n=50 points
# each cluster is constructed by choosinga random 2x2 matrix T_k
# and a random 2x1 marix b_k. Then the ith example from kth cluster is obtained as
# x_{ki} = T_k z_i+ b_k
# z_i and T_k are N(0,1)
# b_k is N(0,10)

# Using 100 random datasets, run the k-means algorithm to 
# convergence and plot the number of steps to conversion for all.

set.seed(25)
num_iter = rep(0,100)
for(a in 1:100){
  m=1  # starting point for for loop
  n=50 #no. of samples for each cluster
  K=4  #no. of clusters
  d=2  #no. of dimensions
  X = matrix(NA,n*K,d)  #create a empty matrix
  init_cluster = matrix(NA,n*K,1)
  for(k in 1:K){
    Tk = matrix(rnorm(4),2,2)
    bk =matrix(rnorm(2,mean=0,sd=10),2,1)
    for(i in m:n) {
      z = matrix(rnorm(2),2,1)
      X[i,] = Tk %*% z + bk
      init_cluster[i] = k
    }
    m=n+1
    n = n+50
    
  }
  n=50 #reset n to 50 ,since its value exceeds in the for loop above
  
  #plot(X,col=init_cluster, main="Initial Data - Part a")
  
  proto = matrix(rnorm(K*d),K,d) # proto[k,] will be center (prototype) of kth cluster
  dist = matrix(NA,K,n*K) # dist[k,i] is distance from ith point to kth cluster center
  cluster = rep(0,n)
  center_chng = rep(0,K)
  
  for(iter in 1:15){
    for(i in 1:200){
      for(k in 1:K){
        dist[k,i] = (X[i,1]-proto[k,1])^2 + (X[i,2]-proto[k,2])^2
      }
      cluster[i] = which.min(dist[,i])
    }

    
    for(k in 1:K) {
      if (sum(cluster == k) == 0) next;
      if (sum(cluster == k) == 1) next;	# don't reestimate center if only have 0-1 examples
      
      
      if(all(proto[k,] == colMeans(X[cluster==k,]))){   # reestimate kth cluster center
        center_chng[k] =FALSE
      }
      else{
        center_chng[k] =TRUE
      }
      proto[k,] = colMeans(X[cluster==k,])
    }
    if(sum(center_chng == FALSE) ==4){
      num_iter[a] = iter-1
      break
    }
    #readline()
    
  }
}
#Plot the number of steps to convergence for 100 datasets
plot(num_iter,xlab="number of dataset", ylab='Number of Iterations to convergence',xlim=c(0,100),ylim=c(0,15),type = 'l')