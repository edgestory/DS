dd1 = matrix(c(1342, 1442, 1252, 1343, 1425, 1362, 1456, 1272,
               1243, 1359,1412, 1253, 1201, 1478, 1322, 1406,
               1254, 1289, 1497, 1208))

dd1.ts = ts(dd1, start=c(2006,1), frequency=4)

plot(dd1.ts, mai="Random Variation Time Series")

dd2 = matrix(c(
  1142, 1242, 1452, 1543, 1125, 1262, 1456, 1572,
  1143, 1259, 1462, 1553, 1121, 1258, 1472, 1546,
  1154, 1249, 1477, 1548
))

dd2.ts = ts(data = dd2, start=c(2006,1), frequency = 4)
plot(dd2.ts)
autoplot(dd2.ts)

dd3 = matrix(
  c(1142, 1242, 1252, 1343, 1225, 1562, 1356, 1572,
    1343, 1459, 1412, 1453, 1401, 1478, 1322, 1606,
    1554, 1589, 1597, 1408)
)
dd3.ts= ts(data = dd3, start=c(2006,1), frequency = 4)

dd4 = matrix(
  c(1142, 1242, 1452, 1543, 1225, 1362, 1556, 1672,
    1343, 1459, 1662, 1753, 1421, 1558, 1772, 1846,
    1554, 1649, 1877, 1948)
)
dd4.ts = ts(data = dd4, start = c(2006,1), frequency = 4)


dd5 = matrix(
  c(1142, 1242, 1452, 1543, 1225, 1362, 1556, 1672,
    1343, 1459, 1662, 1753, 1221, 1358, 1572, 1646,
    1154, 1249, 1477, 1548)
)
dd5.ts = ts(data = dd5, start=c(2006,1), frequency = 4)

dd6 = matrix(c(1342, 1442, 1252, 1343, 1425, 1362,
               1456, 1272, 1243, 1359, 1812, 1653,
               1601, 1878, 1722, 1806, 1654, 1689, 1897, 1608))
dd6.ts = ts(dd6, start = c(2006,1), frequency = 4)

diff(dd6.ts)

class(dd1.ts)
start(dd1.ts)
end(dd1,ts)
frequency(dd1.ts)
cycle(dd1.ts)

window(dd1.ts, c(2007,2),c(2008,3))

cycle(dd1.ts)
tsp(dd1.ts)

zz=ts(matrix(1:24, 8,3), s=c(2001,1), f=4, n=c("a","b","c"))
zz

class(zz)
z1=zz[,"a"]
z1

z2<-zz[,"a"]+zz[,"b"]
z2

z3 = zz[,"a"]^2/zz[,"b"]
z3

ts.plot(zz, lty=1:3, main = "Multiple Time Series Data")

ts.plot(zz, lty=1:3)

autoplot(zz,facet=T)

dd1.ts
dda.ts = lag(dd1.ts, k=2)
dda.ts

diff(dd1.ts,di=2)
dd1.ts

diff(dd1.ts,lag=2)

diff(diffinv(dd1.ts))
dd1.ts

dd1.ts
library(forecast)

lambda = BoxCox.lambda(dd1.ts)

lambda

new = BoxCox(dd1.ts, lambda)
new

(dd1.ts^lambda-1)/lambda
InvBoxCox(new,lambda)


zz1 = ts(matrix(1:24,8,3), s=c(2001,1),f=4,n=c("a","b","c"))
zz1
