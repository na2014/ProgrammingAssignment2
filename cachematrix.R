# makeCacheMatrix - creates a matrix object
# cacheSolve - used to invert the matrix, caching results for future use
#
# Assumes matrix is always invertible.
#
# Sample usage:
#       mobj <- makeCacheMatrix(matrix(1:4,2,2))
#       
#       #  Next command will compute inverse and show message 
#       #  indicating inverse is being computed.
#       print(cacheSolve(mobj))  
#       
#       #  Next message will display inverse, relying on 
#       #  previously cached result.  
#       
#       print(cacheSolve(mobj))  


# This function creates an object with getter and setter methods
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


# This function returns the inverse of a matrix, using or creating 
# cached result as needed
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        #  Get the inverse stored in the object
        m <- x$getinverse()
        
        #  If it null, compute and set it
        if (is.null(m)) {
                message("Computing inverse")
                m <- solve(x$get())
                x$setinverse(m)
        }
        
        #   At this point in the code, m is the inverse.
        #   Return it
        m
}


