hello <- cxxfunction(

  signature(),

  body="cout << "hello";
        "


  plugin="Rcpp"

)
