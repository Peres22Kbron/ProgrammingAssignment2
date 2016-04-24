# The functions built for this assignements work together to either calculate
# or retrieve a cached inverse matrix.

# The function makeCacheMatrix() creates a special "matrix" object that can 
# cache its inverse. The function takes a matrix as argument and creates a list
# with four element as following:
# 1 - The set_mat() function changes the matrix stored in the main function
# 2 - The get_mat() function gets the matrix stored in the main function
# 3 - The set_inv_mat() function stores the inverse matrix in the main function
# 4 - The get_inv_mat() function gets the inverse matrix stored in the main function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set_mat <- function (y){
        x <<- y
        inv <<- NULL
    }
    get_mat <- function() x 
    set_inv_mat <- function(inverse) inv <<- inverse
    get_inv_mat <- function () inv
    list(set_mat = set_mat, get_mat = get_mat,
         set_inv_mat = set_inv_mat, get_inv_mat = get_inv_mat)
}

# The function cacheSolve() either calculates the inverse matrix passed as 
# argument of the makeCacheMatrix() function or retrieves it from the cache 
# of the same function. First of all, the cacheSolve() function checks if the 
# inverse matrix has already been calculated and cached in the makeCacheMatrix() 
# function, and, if so, it returns the inverse matrix. Otherwise, the function
# gets the matrix passed to the makeCacheMatrix() function, calculates its
# inverse, and cached it in the makeCacheMatrix() function.

cacheSolve <- function(x, ...) {
    inv_mat <- x$get_inv_mat()
    if(!is.null(inv_mat)){
        message("getting cached data")
        return(inv_mat)
    }
    data <- x$get_mat()
    inverse <- solve(data,...)
    x$set_inv_mat(inverse)
    return(inverse)
}

# You may test the functions running the following code twice.
# The message - getting cached data - should appear after running the second time

mat <- matrix(rnorm(25),5,5)
x<-makeCacheMatrix(mat)
cacheSolve(x);cacheSolve(x)
