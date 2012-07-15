time Rscript foo.r >> bar
cat bar | awk '{print $6,$10, $12}' > baz

