
bookStatus = .strategy$order_book.bug$bug$GSPC$Order.Status 
bookQty    = .strategy$order_book.bug$bug$GSPC$Order.Qty

t1 = bookStatus[bookStatus$Order.Status == 'rejected']    # 34
t2 = bookStatus[bookStatus$Order.Status == 'closed']      # 51
t3 = bookStatus[bookStatus$Order.Status == 'open']        # 2 

t4 = nrow(mktdata[mktdata$fast.gt.up == 1])   # 61
t5 = nrow(mktdata[mktdata$fast.lt.dn == 1])   # 54 

t6 = suppressWarnings(max(as.numeric(bookQty), na.rm=TRUE)) # 100
t7 = suppressWarnings(min(as.numeric(bookQty), na.rm=TRUE)) # -100





