## Program contains two functions to cache inverse of a matrix

## First function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { # initializes matrix
        i <- NULL # sets the inverse of matrix, i to NULL
        set <- function(y){ # if matrix is changed then value of inverse i is re intialized
                x <<- y
                i <<- NULL
        }
        get <- function() x # returns the value of saved matrix
        setinv <- function(inv) i <<- inv # sets the value of inverse of the matrix x
        getinv <- function() i # returns the value of inverse of matrix x i.e. i
        list(set = set, get = get, setinv = setinv, getinv = getinv) # adds all the function to a list object
}

## Second function computes the inverse of the matrix stored in special "matrix" object

cacheSolve <- function(x, ...) { # takes the special "matrix" object created by the above function as input
        i <- x$getinv()
        if(!is.null(i)){ # checks stored value of inverse in cache
                message("getting cached data") # prints message if value is retrived from cache
                return(i) # returns the inverse from cache
        }
        data <- x$get() # if inverse is not store in cache, matrix is retived using get() function and stored in data
        i <- solve(data, ...) # inverse is calculated
        x$setinv(i) # calculated inverse is sored in cache
        i # inverse in returned
}
