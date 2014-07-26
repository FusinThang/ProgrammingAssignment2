## This function is to create a class that we can save the inverse matrix to it if we has run the matrix once
## Input should be a square matrix
## Output is a list saving it and its inverse
makeCacheMatrix <- function(x = matrix()) {
    cached <- NULL
    # Set to another matrix
    set <- function(y) {
        x <<- y
        cached <<- NULL
    }
    
    # Get current matrix
    get <- function() x
    
    # Save inverse_matrix to cached
    set_inverse <- function(inverse_matrix) cached <<- inverse_matrix
    
    # Get inverse_matrix in cached
    get_inverse <- function() cached
    
    # Return a list
    list(get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}

## This function is to: get the inverse of a matrix, will get the cached inverse matrix if the matrix has been computed before
## Input should be a square matrix
## Output is the inverse matrix
cacheSolve <- function(x, ...) {
    current_matrix <- x$get()
    # Check and return error if x is not a matrix
    if ( !is.matrix(current_matrix) ){
        return("Should input a matrix here.")
    }
    # Check and return error if x is not a square matrix
    if ( length(current_matrix[,1]) != length(current_matrix[1,]) ){
        return("Inputted matrix should be a square matrix.")
    }
    # Check and return error if x is a singular matrix
    if ( det(current_matrix) == 0 ){
        return("Inputted matrix is a singular matrix, it don't have inverse matrix. Please try another.")
    }
    
    cached <- x$get_inverse()
    # Load cache if this matrix has been computed before
    if(!is.null(cached)) {
        message("getting cached data")
        return(cached)
    }
    
    # Get the inverse matrix if this matrix is computed at 1s time
    inverse_matrix <- solve(current_matrix)
    # Save the inverse_matrix to cached
    x$set_inverse(inverse_matrix)
    # Return the inverse_matrix
    inverse_matrix
}