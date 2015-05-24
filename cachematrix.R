## This assingment creates two functions to cache the inverse of a matrix, these funtions need to be used togather in a looping program to be of any benefit.
## If the inverse has already been caclulated (an the matrix has not changed), then cacheSolve should retrieve the inverse from the cache



## makeCacheMatrix: This function creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
             im <- NULL
             set <- function(y) {
                    x <<- y
                    im <<- NULL
             }
             get <- function() x
             setinvm <- function(invmatrix) im  <<- invmatrix
             getinvm <- function() im
             list(set = set, get = get, setinvm = setinvm, getinvm = getinvm)
}



## cachSolve This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## it assume the matrix is always invertible.

cacheSolve <- function(x, ...) {
         im <- x$getinvm()
         if(!is.null(im)) {
                    message (" getting cached matrix")
                    return(im)    
         }
         data <- x$get()
         im <- solve(data, ...)
         x$setinvm(im)
         im
         
        ## Return a matrix that is the inverse of 'x'
}
