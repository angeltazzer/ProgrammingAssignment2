################### Coursera Assignment 2 by Angel Tazzer ##########################
######### FUNCTIONS #######################

## The Function MakecacheMatrix works as follows:
## Create a list of four functions that will:
## 1. Set (store) the matrix as defined by the user (set)
## 2. get (retrieve) the matrix as defined by the user i.e. (get)
## 3. Set (calculate) the inverse matrix based on the matrix defined by the user (setinverse)
## 4. get (retrieve) the inverse matrix based on the matrix defined by the user (getinverse)



makeCacheMatrix <- function(x = matrix()) {
m <- NULL
set <- function(y) {
    x <<- y
    m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<-solve
    getinverse <- function() m
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## The Function cacheSolve works as follows:
## Verifies that the inverse of the matrix has not being calculated previously by cheching if m is not NULL 
## If m is not null the value of m (inverse matrix) is displayed and a message "getting cached data" is displayed
## If m is null the matrix is retrieved using the get function and the inverse matrix is calculated by solve
## The inverse matrix is retrieved and displayed



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m

}

#######  COMMANDS TO RUN PROGRAM


# Create list of 4 functions (set, get, setinverse, getinverse)
A = makeCacheMatrix()

# Define Matrix 1
Matrix1 <- matrix(c(50, 4, 3, 1, 5, 7,9,9,8), # the data elements 
       nrow=3,              # number of rows 
       ncol=3,              # number of columns 
       byrow = TRUE)        # fill matrix by rows 

# Set Matrix 1
A$set(Matrix1)

# Calculating Inverse Matrix from Matrix 1 for the first Time
cacheSolve(A)

# Calling the function to compute the Inverse Matrix from Matrix 1 for the second time. A message
# "getting cached data" is being dislayed since the matrix 1 has not changed and the Inverse Matrix for
# Matrix 1 is called from the cache. No calculation is performed at this time
cacheSolve(A)

# Define Matrix 2
Matrix2 <- matrix(c(5, 2, 4, 1), # the data elements 
                  nrow=2,              # number of rows 
                  ncol=2,              # number of columns 
                  byrow = TRUE)        # fill matrix by rows 
# Set Matrix 2
A$set(Matrix2)

# Calculating Inverse Matrix from Matrix 2. No message is displayed since Matrix 2 is differenf from Matrix 1
cacheSolve(A)
