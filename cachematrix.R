## Put comments here that give an overall description of what your
## functions do


## Write a short comment describing this function
#
#  This function creats a list of four functions. The set function is used to
#  assign a matrix object. The get function simply retrieves the matrix.
#  The setMatrix function is used to assign a computed inverse, and the 
#  getMatrix function is used to retrieve it.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL               # Create an empty object to hold the matrix
        set <- function(y) {    # A function to assign a matrix object
                x <<- y
                i <<- NULL
        }
        get <- function() x     # Matrix object is retrieved
        setMatrix <- function(inverse) i <<- inverse
        getMatrix <- function() i
        list(set = set, get = get,
             setMatrix = setMatrix,
             getMatrix = getMatrix)
 
 
}


## Write a short comment describing this function
#
#  This function makes calls to the functions created by makeCacheMatrix.
#  It checks to see whether or not a new inverse computation is required.
#  If the matrix has a previously associated inverse calculation, the 
#  calculation will not be repeated.
cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getMatrix()
        if(!is.null(i)) {   # Check to see if matrix object is non-empty
                message("getting cached data")
                return(i)   # Return the cached result
        }
        data <- x$get()     # Compute a new inverse
        i <- solve(data)
        x$setMatrix(i)
        i                   # Return the result

}
