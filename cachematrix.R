## There are,in this repo, a couple functions that are used to create a special object that stores a matrix and caches its inverse.

## makeCacheMatrix function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
{
        inverse<-NULL
        set<-function(y)
        {
                x<<-y
                inverse<<-NULL
        }
        get<-function() x
        setm<-function(solve) inverse<<- solve
        getm<-function() inverse
        list(set=set, get=get,
         setm=setm,
         getm=getm)
}


## cacheSolve computes the inverse of the matrix created by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
{
        inverse<-x$getm()
        if(!is.null(inverse))
        {
        message("getting cached data")
        return(inverse)
        }
        m<-x$get()
        inverse<-solve(m, ...)
        x$setm(inverse)
        ## Return a matrix that is the inverse of 'x'
        inverse
}
