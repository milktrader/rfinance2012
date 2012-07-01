suppressMessages(require(svUnit))

########## FUNCTIONS TO TEST ################

source('squares.r')
source('cubes.r')
source('hello.r')

########### TEST FUNCTIONS ################

test(squares) = function() {
  
  checkEqualsNumeric(squares(1),1)
  checkEqualsNumeric(squares(5),25)
  checkEqualsNumeric(squares(-5),25)
}

test(cubes) = function() {
  
  checkEqualsNumeric(cubes(1),1)
  checkEqualsNumeric(cubes(5),125)
}

test(hello) = function() {
  
  checkEquals(hello(), 'hello')
}


############### OUTPUT RESULTS ##############

(runTest(squares))
(runTest(cubes))
(runTest(hello))
