makeCacheMatrix <- function(x = matrix()) {
	#we will create a special matrix object which can cache its inverse
    m <- NULL
    #settting value of the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    #getting the matrix
    get <- function() x
    #setting the inverse of the matrix
    set_inverse <- function(solve()) m <<- inverse
    #getting the inversed matrix
    get_inverse <- function() m
    #returning list with all functions
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}

cacheSolve <- function(x, ...) {
    #returns the inversed matrix of x
    m <- x$get_inverse()
    #checking if m is not null - if so, we have already computed it
    if(!is.null(m)) {
        #here we are telling the user that we are retrieving the cached data and printing the value of m
        message("getting cached data")
        return(m)
    }
    #if m is null, then we need to compute the inversed matrix using the following code and print out the value
    data <- x$get()
    m <- solve(data, ...)
    x$set_inverse(m)
    m
}