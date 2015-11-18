## Put comments here that give an overall description of what your
## functions do

## This function creates a special vector, being nothing else than list containing 4 functions to 
# a) set the value of the matrix  
# b) get the value of the matrix
# c) set the value of the inverse
# d) get the value of the inverse


makeCacheMatrix <- function(Morpheus = matrix()) {
  InvVal<-NULL  # initialise the inverse value
  set<-function(y){
    Morpheus<<-y #resets the matrix Morpheus to a new matrix y
    InvVal<<-NULL #resets the value to Null 
  }
  get = function() Morpheus # this returns the matrix Morpheus
  setmatrix = function(solve) InvVal<<- solve 
  getmatrix = function() InvVal
  list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix) # returns the special vector with the 4 functions just created
}

## This function computes the inverse of the matrix object created in the function makeCacheMatrix if it hasn't changed. 
# It checks if the inverse has been calculated & that the matrix hasn't changed
# it then retrieves the inverse from the cache

cacheSolve <- function(Morpheus=matrix(), ...) {
  ## Return a matrix that is the inverse of 'Morpheus'
  InvVal<-Morpheus$getmatrix() 
  if(!is.null(InvVal)){
    message("Retrieving the Inverse of the matrix from the cache ... the inverse is : ")
    return(InvVal)
  }
  #if the inverse hasn't been calculated yet ... there's some work to do now : 
  matrix<-Morpheus$get
  InvVal<-solve(matrix, ...) # calculates the inverse ("solve()")
  Morpheus$setmatrix(InvVal)
  cat("The inverse of the matrix is :", InvVal)
}

