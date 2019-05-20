##        (Note to my peer reviewers: I've test run this code and it seems to work properly. 
##                    Please test run if you think there's any error.)

#The following "makeCacheMatrix" function creates 
#a special "matrix" object that can cache its inverse:

makeCacheMatrix <- function(x = matrix()) {
        #setting up the null inverse property
        i <- NULL
        #Setting the matrix
        set <- function(z) {
                x <<- z
                i <<- NULL
        }
        
        #To get the matrix
        get <- function() {
                ## Returning the value of the matrix
                return(x)   }
        
        
        #Setting the inverse of the matrix
        setinverse <- function(inverse){ 
                i <<- inverse}
        
        #Setting the inverse of the matrix
        getinverse <- function(){
                ## Returning the inverse property
                return(i)
        }
        
        ## Returning  the list of the used methods         
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

##The following "cachesolve" function calculates the inverse of the matrix
##returned by the above function.  If the inverse has already been calculated,
##(and the matrix has not changed), then the function retrieves the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Returning a matrix that is the inverse of 'x'
        i <- x$getinverse()
        ##Returning the inverse if it's already calculated
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        #Getting the matrix object and calculating the inverse
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

 