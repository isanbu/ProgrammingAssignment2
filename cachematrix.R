makeCacheMatrix <- function(x=matrix()){
#This function creates a matrix, cache it and cache its inverse
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse<- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  
}

cacheSolve <- function(x) {
  
  # This function computes the inverse of a matrix called x, received as argument. 
  # If the inverse has been previously computed it is retrieved from the cache.
  # Return the inverse of x.
  
  #Check the matrix stored in the cache is the same one received as argument
  mat <- x$get()
  #If the matrix has not changed from the one in cache
  if(mat == x){
    #Retrieve the inverse from the cache
    m <- x$getinverse()
    
    if(!is.null(m)) {
      #If the inverse is not null, we return the inverse retrieved from cache
      message("getting cached data")
      return(m)
    }
  }
  #If the inverse is not cached, compute it and store it in the cache for future access
  i <- solve(mat)
  x$setinverse(i)
  i
}
