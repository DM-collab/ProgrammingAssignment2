#this functions creates de matrix, which can be stored in cache
makeCacheMatrix <- function(x = matrix()) {

       i = NULL
       set = function(y) {
                 x <<- y
                 i <<- NULL
       }
                  
        
        get = function() x
        setinverse = function(inverse) i <<- inverse 
        getinverse = function() i
        
        list(set = set,  get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
  
}


## Search for cache solved inverse matrix, 
##  in case it havent been calculated, it calculates it again
#### Checks for Inverse Matrix if its already in cache ####

#if its NOT null -> its alread´stored in cache
## If it IS Null -> calculates the inverse 
# sets the value of the inverse in the cache via the setinv function.
cacheSolve <- function(x, ...) {

  i = x$getinverse()
          if (!is.null(i)){
            message("getting inverse Matrix from cached data")
            return(i)
  }

  data <- x$get()
  i <- solve(data, ...)
  
  x$setinverse(i)
  i
  
}


