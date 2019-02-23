需要补充的分析


regress    
rvfplot,yline(0) xline(0)
rvpplot lnland
estat imtest, white

xtdes
xtsum   
reg    , vce(cluster id)
estimates store OLS
reg   //对比普通与聚类稳健，聚类为普通的一半大时差异大时用聚类稳健比较好
xtreg    ,fe r
LSDV 的stata命令
reg     i.id ,r


组内估计
xtreg     ,fe r
estimates store FE_robust

xtreg     ,fe
estimates store FE

当普通为聚类的一半时，进一步通过LSDV考察
reg   i.id, vce(cluster id)
estimaters store LSDV

随机效应FGLS
xtreg    ,re r theta
estimates store RE
xttest0

hausman检验
xtreg   ,fe
estimates store FE
xtreg    ,re
estimates store RE
hausman FE RE , constant sigmamor

稳健标准误的辅助回归
ssc..
quietly xtreg  ,re r
xtoverid

OLS+面板矫正标准误最为稳健
