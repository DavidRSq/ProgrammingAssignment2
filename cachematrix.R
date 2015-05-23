## These are two functions that compute the inverse of a matrix and store the
## input matrix and the inverse in a cache
## Subsequent inputs are checked against the cache, and if the inverse of the
## input matrix has already been calculated it is returned from the cache, and
## computation is bypassed.

## makeCacheMatrix creates a special "matrix" object (actually a list) that
## caches the inverse of the input matrix, which is computed in cacheSolve 
## Cachesolve calls functions from makeCacheMatrix via this list


makeCacheMatrix <- function(x = matrix()) {
  
  inv<- NULL           ## inv set to NULL
  
  set<-function(y) {   ## This function used when the input to makeCacheMatrix     
    x<<- y             ## is a previously unseen matrix and adds it to the    
    inv<<- NULL        ## cache, while resetting inv to NULL, which tells        
  }                    ## cacheSolve to compute a new inverse matrix 
  
  get <- function() x  ## Function used to send input matrix to cacheSolve
                       ## function
  
  setinv <- function(matinv) inv <<-matinv
                       ## Sets inv to value of inverse computed in cacheSolve
  
  getinv <- function() inv
                       ## Sends value of inverse to cacheSolve
                       
  list(set=set, get=get, setinv=setinv, getinv=getinv)
                       ## Special "matrix" (list) passes the input matrix, inv,
                       ## cached matrices and cached inverse matrices to 
                       ## cacheSolve
  
}


## This function computes the inverse of the matrix input into makeCacheMatrix
## If the inverse has already been computed previously, it retrieves the cached
## solution and bypasses the computation


cacheSolve <- function(x, ...) {
    
  inv <- x$getinv()    ## Retrieves value of inv from makeCacheMatrix
                       
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }                    ## If inv is not NULL (i.e. input matrix is present in 
                       ## cache), retrieves inverse from cache and ends
                       ## function
  data <- x$get()      
                       ## Retrieves new input matrix from makeCacheMatrix
                       
  inv <- solve(data, ...)
                       ## Computes inverse
  x$setinv(inv)
                       ## Sends inv value back to makeCacheMatrix
                       
  inv
                       ## Returns newly computed inverse
}
