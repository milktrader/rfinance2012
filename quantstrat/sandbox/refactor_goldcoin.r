> golden_cross <- Lag(ifelse(GLD$fast > GLD$slow, 1, -1))
> foo = Lag(ifelse(SMA(Cl(GLD), n=i) > SMA(Cl(GLD), n=j), 1, -1))
> all.equal(golden_cross, foo)
[1] TRUE
> 
> golden_cross <- na.locf(golden_cross, na.rm=TRUE)
> foo = na.omit(foo)
> all.equal(golden_cross, foo)
[1] "Attributes: < Names: 2 string mismatches >"                                  
[2] "Attributes: < Length mismatch: comparison on first 8 components >"           
[3] "Attributes: < Component 7: Modes: character, numeric >"                      
[4] "Attributes: < Component 7: Lengths: 1, 80 >"                                 
[5] "Attributes: < Component 7: Attributes: < target is NULL, current is list > >"
[6] "Attributes: < Component 7: target is character, current is omit >"           
[7] "Attributes: < Component 8: 1 string mismatch >"                              
> 
> nrow(golden_cross)/nrow(foo)
[1] 1
> 
> bar = golden_cross - foo
> sum(bar)
[1] 0
