## These functions together take a matrix input and return the inverse of the matrix.
## makeCacheMatrix caches (stores) the inverse so that it can be recalled for further
## saving valuable computational time.  cacheSolve takes the matrix arguement and returns
## inverse, retrieving it from makeCacheMatrix if in memory, or calculating if not. 

## makeCacheMatrix is the function which caches the inverse matrix. It stores the 
## inverted matrix from cacheSolve the first time, so that it can be retrieved when recalled. 
## It returns a list of function thats allow retrieval from cache

makeCacheMatrix <- function(x = matrix()) {
  cache<-NULL
  set<-function(y){ #The set function 
    x<<-y #assign y to x (the matrix input by user)
    cache<<-NULL #assign NULL to cache (to clear the cache from previous values)
  }
  get<- function()x #get x matrix from parent environment
  getinverse<- function()cache #retrieve inverse from cache
  setinverse<- function(inverse)cache <<-inverse # set new inverse into cache
  list(set = set, get = get, getinverse = getinverse, setinverse = setinverse)
  #returns a list which sets the names to the functions and allows these to be 
  #called upon in the cachesolve function
}


## This function takes the matrix argument and retrieves the inverse from cache where it
## exists. If not in cache, it calculates the inverse and stores it into cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse() #retrieve cache value from makecachematrix
  if (!is.null(inv)){ #if there is a value in inv
    message("getting cache data")
    return (inv) ##return the cache inverse matrix
  }
  data<-x$get() ##else, inverse the matrix and cache
  inv<-solve(data, ...)
  x$setinverse(inv)
  inv ## return the inverse matrix
}

