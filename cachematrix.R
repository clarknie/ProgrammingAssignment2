# Two functions are defined here for caching the inverse of a matrix that may have been calculate before
#     makeCacheMatrix can create a special object that has both a matrix and its inverse in the environment
#     cacheSolve can get the inverse of the matrix special 
# Though it's not required. A test of performance is also included in the end
#################################################################################

# makeCacheMatrix function is used to create a special object that 
# stores a matrix and caches its inverse
#################################################################################

makeCacheMatrix <- function(x = matrix()) {
    ## create a special object 
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    # get function takes no argument, return x
    get <- function() x
    # solve is a matrix that is used to set m(inverse) of this environment
    setsolve <- function(solve) m <<- solve
    # getsolve function takes no argument, return m
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


# cacheSolve is used to create the inverse of the special object created by makeCacheMatrix.
# It first checks if the inverse has already been computed. If so, it gets the inverse
# without the computation. If not, it computes the inverse and set the value of the inverse 
# through setsolve function.
#################################################################################

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
        # the inverse had been cached, directly retrieve the inverse
        #message("getting cached data")
        # since message itself can be a time consuming function
        # it can be turned off by commenting it to further reduce computation time
        return(m)
    }
    # if solve is not cached, then go through the computation by calling solve()
    data <- x$get()
    m <- solve(data, ...)
    # save this inverse just calculated into the environment
    x$setsolve(m)
    m
}


# Test submission's performance by retrieving inverse of some randomly generated matrix 
#################################################################################

# In this test set, I generate 5000 20 by 20 matrix with all 400 elements randomly drawn from 
# a uniform distribution between 0 and 10000. 
myObj <- list(makeCacheMatrix(matrix(runif(400,0,10000), 20, 20)))
for(j in 1:4999) myObj <- append(myObj,list(makeCacheMatrix(matrix(runif(400,0,10000), 20, 20))))

# first pass of getting the inverse
# it takes 0.018 seconds on my system (Mac OSX with 16G RAM)
system.time(for(item in myObj) { cacheSolve(item) }, gcFirst = TRUE ) 

# second pass 
# it takes 0.002 seconds on my system 
system.time(for(item in myObj) { cacheSolve(item) }, gcFirst = TRUE ) 

# First pass is slower than the second pass because each inverse is calculated from scratch in the first pass.
# Second pass only retrive the inverse directly.

