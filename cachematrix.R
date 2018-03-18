## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a function objects which contains 4 inner-functions.
## Those inner functions are responsible for:
## initialising matrix object, retrieving it, setting the inverse of it and retrieving it.

makeCacheMatrix <- function(x = matrix())
{
    inv <- NULL
    set_matrix <- function(y) 
        {
            x <<- y
            inv <<- NULL
        }
    get_matrix <- function() x
    set_inverse <- function(inverse) inv <<- inverse
    get_inverse <- function() inv
    list(set_matrix = set_matrix,
         get_matrix = get_matrix,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## Write a short comment describing this function
## This function takes a function object (which contains inner-functions and matrix itself)
## as an argument and retrieves inverse matrix. If the inverse was not calculated yet it calculates it.

cacheSolve <- function(x, ...)
{
    inv <- x$get_inverse()
    if(!is.null(inv))
        {
            message("getting cached inverse")
            return(inv)
        }
    data <- x$get_matrix()
    inv <- solve(data, ...)
    x$set_inverse(inv)
    inv
}

