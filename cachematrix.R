# Following the same format as the assignment example
# Creating a makeCacheMatrix object will consist of
# four functions encapsulated in a list

# 1. set the matrix
# 2. get the matrix
# 3. set the inverse of the matrix
# 4. get the inverse of the matrix

# Initially set to NULL
# Changes when the user sets the value

# Created by Dave Langcaster 17/8/19

makeCacheMatrix <- function(x = matrix()) {
        
        # Initialise the cache to NULL
        
        m = NULL
        
        # Define a function that sets the value of the matrix and clears the old inverse from the cache
        
        set = function(y) {
                x <<- y    # Set the value
                m <<- NULL # Clear the cache
        }
        
        # Define a function that gets the value of the matrix, but not the inverse
        
        get = function() x
        
        # Define a function to set the inverse - only used by getinverse() when
        # there is no cached inverse
        
        setInverse = function(inverse) m <<- inverse
        
        # Define a function to get the inverse
        
        getInverse = function() m
        
        # Return a list with the above four functions
        
        list(set=set, get=get,
             setInverse=setInverse,
             getInverse=getInverse)
}

# This function computes the inverse of the special "matrix" created by 
# makeCacheMatrix. If the inverse has already been calculated (and the 
# matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        # Return a matrix that is the inverse of 'x', checking to see if it has been computed
        
        m = x$getInverse()
        
        # If the inverse has been computed, return it
        
        if(!is.null(m)) {
                message("getting cached data.")
                return(m)
        }
        
        # If the inverse hasn't been computed, get the matrix, solve, and cache the result
        
        data = x$get()
        m = solve(data, ...)
        x$setInverse(m)
        return(m)
}
