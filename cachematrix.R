## This defines a simple Matrix inverse calculation function - cacheSolve()
## cacheSolve() calculates the inverse of a given matrix, 
## saves it in "cache", and returns the result
## If the same(identical Matrix) is given as input again, 
## it will return the data/result from "cache"
## saving time and processing resources, 
## from recalculating the same result.

## How to use this function ?
## call the function cacheSolve with a matrix(inversable) as input
## example,
## a <- matrix(c(0,2,2,0),2,2)
## cacheSolve(a)
## a is cached in variable xCache
## inverse of matrix a, is cached in mCache

## Sets up the "cache" valriables, and methods to access the "cache"
makeCacheMatrix <- function(y = matrix()) {
  
  mCache <<- NULL
  
  set <- function(y) {
    xCache <<- y
  }
  
  get <- function() xCache
  
  setInverse <- function(inverse) mCache <<- inverse
  
  getInverse <- function() mCache
  
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
  
}


## Performs inverse calculation and cache update

cacheSolve <- function(z, ...) {
  if(!exists("xCache")||!identical(z,xCache)){
    
    ## setup Cache with new values
    v <<- makeCacheMatrix(z)
    v$set(z)
    v$setInverse(solve(z, ...))
    return(mCache)
  }
  
  ## return data from Cache
  message("Getting it from cache ...")
  return(mCache)
  
}
