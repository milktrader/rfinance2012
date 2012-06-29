######### DEFINE OUTSIDE TEST FUNCTION ############

statusCheck = 'should have correct number of each status'
signalCheck = 'should have the correct number of signals'
qtyCheck    = 'should not exceed 100'

############ DEFINE INSIDE TEST FUNCTION ################

test(statusCheck) = function() {

  Status = .strategy$order_book.bug$bug$GSPC$Order.Status 

  t1     = nrow(Status[Status$Order.Status == 'rejected'])    # 34
  t2     = nrow(Status[Status$Order.Status == 'closed'])      # 162
  t3     = nrow(Status[Status$Order.Status == 'open'])        # 2 

  checkEquals(t1, 34)
  checkEquals(t2, 162)
  checkEquals(t3, 2)

}

test(signalCheck) = function() {
    
  t4 = nrow(mktdata[mktdata$fast.gt.up == 1])   # 61
  t5 = nrow(mktdata[mktdata$fast.lt.dn == 1])   # 54 
  
  checkEquals(t4, 61)
  checkEquals(t5, 54)
}

test(qtyCheck) = function() {
    
  Qty    = .strategy$order_book.bug$bug$GSPC$Order.Qty

  t6     = suppressWarnings(max(as.numeric(Qty), na.rm=TRUE)) # 100
  t7     = suppressWarnings(min(as.numeric(Qty), na.rm=TRUE)) # -100
  
  checkEquals(t6, 100)
  checkEquals(t7, -100)
}
 
 

