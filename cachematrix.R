## These two functions should retrieve the inverse of a square matrix, the first 
## one should cache the result, in order to save time for looping instances in
## in which this calculation has to be done over an over

## The first function creates a cached vector with the inverse matrix
#which will set/get the value of a cached vector and set/get the inverse of a
##matrix

makeCacheMatrix <- function(x = matrix()) { #Creation of the vector as matrix
    inv <- NULL #Start vector with NULL
    set <- function(y) { 
        x <<- y
        inv <<- NULL
    }
    get <- function() x #Gets the value of the inverse matrix
    setinverse <- function(solve) inv <<- solve #Setting the inverse of the function
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse) #Tag the elements of the list with names
}


## This function will look for the innverse of matrix. First will look if the 
## inverse is already stored in a cached vector. If not, it will calculate the
## inverse and then it will set it for future use with the setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) { #Look if the inverse already exist, if not it will calculate it
        message("Looking for cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...) #Function to inverse a matrix
    x$setinverse(inv) #Set the inverse to store it in the cache
    inv #Prints the inverse of the matrix
}

