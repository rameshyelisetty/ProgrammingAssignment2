## This defines a simple mean calculation function - cacheMean()
## cacheMean() calculates the mean of a given vector, 
## saves it in "cache", and returns the result
## If the same(identical vector)data is given as input again, 
## it will return the data/result from "cache"
## saving time and processing resources, 
## from recalculating the same result.

## How to use this function ?
## call the function cacheMean with a valid vector as input
## example,
## a <- c(1, 23, 17, 124, 2, 12)
## cacheMean(a)
## a is cached in variable xCache
## mean of matrix a, is cached in mCache

## Sets up the "cache" valriables, and methods to access the "cache"
makeVector <- function(y = numeric()) {
        
        mCache <<- NULL
        
        set <- function(y) {
                xCache <<- y
        }
        
        get <- function() xCache
        
        setmean <- function(mean) mCache <<- mean
        
        getmean <- function() mCache
        
        list(set=set, get=get, setmean=setmean, getmean=getmean)
        
}

calcMean <- function(z, ...){
        
        if(!exists("xCache")||(length(z)!=length(xCache))){
                
                # setup Cache with new values
                v <<- makeVector(z)
                v$set(z)
                v$setmean(mean(z))
                return(mCache)
        }
        
        if(sum(z!=xCache)>0){
                
                # setup Cache with new values  
                v <<- makeVector(z)
                v$set(z)
                v$setmean(mean(z, ...))
                return(mCache)
        }
        
        # return data from Cache
        message("Getting it from cache ...")
        return(mCache)
}