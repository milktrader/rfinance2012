trans = function(Port = 'bug', Sym = 'GSPC') {

  return(head(getTxns(Portfolio=Port,  Symbol=Sym)[,1], n=22))
}
