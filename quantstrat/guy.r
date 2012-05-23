for(i in 1:nrow(SPY)) {
  CurrentDate = time(SPY)[i]
  ClosePrice  = as.numeric(Cl(SPY[i,]))
  Posn = getPosQty('faber', Symbol='SPY', Date=CurrentDate)
    
    if( !is.na(as.numeric(SPY[i, 'ten']))){

    if(Posn == 0) { 

      if(ClosePrice > as.numeric(SPY[i, 'ten'] )){

        addTxn('faber', Symbol='SPY', TxnDate=CurrentDate, TxnPrice=ClosePrice, TxnQty=1000, TxnFees=0) }

    } else {

      if(ClosePrice < as.numeric(SPY[i, 'ten'])) {

        addTxn('faber', Symbol='SPY' ,TxnDate=CurrentDate, TxnPrice=ClosePrice, TxnQty = -Posn, TxnFees=0)}
      }
    }


      updatePortf('faber', Dates=CurrentDate)
      updateAcct('faber', Dates=CurrentDate)
      updateEndEq('faber', Dates=CurrentDate)
      

}
