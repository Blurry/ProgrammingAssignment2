## Unit test suite for cachematrix
## NOTE: Make sure that RUnit package is installed and set working directory to the assignment directory

library(RUnit)

source("cachematrix.R")

.setUp <- function() {
  if (exists("cached_inverse")) rm(cached_inverse, inherits = TRUE)
  testMatrix0 <<- matrix(c(1,2,3,0,1,4,5,6,0),3,3)
  testMatrix2 <<-  matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 0),3,3)
}

test.cache <- function()
{
  testMatrix1 <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 0),3,3)
  
  tm1 <- makeCacheMatrix(testMatrix1)
  test1 <- cacheSolve(tm1)
  ## Check that result is not empty
  checkTrue(!is.null(test1))
  ## Check that matrix is returned 
  checkEquals(class(test1), "matrix")
  
  test2 <- cacheSolve(tm1)
  checkIdentical(test1, test2)
  
  test3 <- cacheSolve(makeCacheMatrix(testMatrix2))
  checkIdentical(test1, test3)
}

test.inverse <- function() {
  ## pre-defined example
  testInv <- cacheSolve(makeCacheMatrix(testMatrix0))
  checkEquals(testInv[1,1], -24)
  testInvExpected <- matrix(sapply(c(-24, 18, 5, 20, -15, -4, -5, 4, 1), as.numeric),3,3);
  checkEqualsNumeric(testInvExpected, testInv)
  

  ## Check that general inverse is correct
  test3 <- cacheSolve(makeCacheMatrix(testMatrix2))
  checkEqualsNumeric(testMatrix2 %*% test3, diag(3))
  
  test4 <- cacheSolve(makeCacheMatrix(testMatrix0))
  checkTrue(!identical(test4,test3))
}

test.errorHandling <- function() {
  testMatrixSingular <<- matrix(1:9, 3, 3)
  checkException(cacheSolve(makeCacheMatrix('a')), 'Unable to take cacheSolve from wrong type')
  checkException(cacheSolve(makeCacheMatrix(NULL)), 'Unable to take cacheSolve from NULL')
  checkException(cacheSolve(makeCacheMatrix(testMatrixSingular)), 'Unable to take cacheSolve from singular')
}

runSuite <- function() {

  testsuite.cachesolve <- defineTestSuite("cachesolve",
                                 dirs = file.path(getwd()),
                                 testFileRegexp = "^.+_tests\\.R",
                                 testFuncRegexp = "^test.+",
                                 rngKind = "Marsaglia-Multicarry",
                                 rngNormalKind = "Kinderman-Ramage" 
                                 )

  testResult <- runTestSuite(testsuite.cachesolve)
  printTextProtocol(testResult)
}