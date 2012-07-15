require(scatterplot3d)


qux = read.table('baz')
x = qux$V1
y = qux$V2
z = qux$V3
wireframe(z~x*y, zlab='profit factor', xlab='slow MA', ylab='fast MA')
