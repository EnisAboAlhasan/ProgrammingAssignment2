## Put comments here that give an overall description of what your
## the functions take a matrix invert it using solve and storing it in inverted for future use.

## makeCacheMatrix is assigned a function that takes a matrix and assigns it to variable x
# the function has a variable that stores the inverted matrix in is initiate with null value
# invert stores a function that take variable y assigns it to x using <<- from global environment
#the get variable stores the function that return the matrix without taking any arguments
#the setinverted stores a function that take inverse as an argument as assigns it to inverted
#We create a list that will help us access all the variables that store the functions

makeCacheMatrix <- function(x = matrix()) {
        inverted <- NULL
        invert <- function(y){
                x <<- y
                inverted <<- NULL
        }
        
        
        get <- function() {x}
        setinverted <- function(inverse) {inverted <<- inverse}
        getinverted <- function() {inverted}
        
        list(invert = invert, get = get, setinverted = setinverted, getinverted = getinverted)

}


## cacheSolve stores a function that take args x and any other args
# inverted variable stores the matrix from the makeCacheMatrix
#the "if" checks if the inverted variable stores a value (in our case an inverted matrix or null)
# if inverted stores a value the value is returned with a message that it was cached, else the code continues
# data stores the get function that take the matrix 
# inverted uses the functin solve to invert the matrix and any other args
#then the inverted value is stored in inverted replacing the null
# the inverted matrix is returned

cacheSolve <- function(x, ...) {
        inverted <- x$getinverted()
        if(!is.null(inverted)){
                message("getting cached data")
                return(inverted)
        }
        
        data <-x$get()
        inverted <- solve(data, ...)
        x$setinverted(inverted)
        inverted
        
        ## Return a matrix that is the inverse of 'x'
}
