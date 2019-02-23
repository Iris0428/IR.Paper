**以土地单产为土地生产率

use "D:\曾翠红\Stata14\新建文件夹 - 副本 (2)\新建文件夹\allnet.dta", clear
keep if sm==33|sm==34|sm==35|sm==36|sm==42|sm==43|sm==44|sm==45|sm==46


**包含的变量有（yield、income、profit、land、cland、labor、flabor、elabor、resident、machine、other、subsidy、insurance、plots、fstruct、job、concurrent、sex、age、educ、train、health、status）

**选取变量(删除收获面积、年末耕地面积、家庭常住人口、家庭全年总收入为0的农户数据，并将部分变量+0.001）
drop if rland==0  //
drop if rland==.
drop if ryields==.
g yield = ryields/rland
winsor yield, g (yieldo) p(0.05)
sum yieldo
drop if yield>833.3334
drop if yield<285.7143

g land = rland      
g labors= rlabors   
g elabors= relabors     
g e1=re1 
g e2=re2 
g e3=re3 
g e4=re4 
g e5=re5 
g e6=re6 
g e7=re7 
g e8=re8 
g e9=re9
g e10=re10
g e11=re11 
g e12=re12   //
ren age agee
//winsor land,gen (land1) p(0.025)
//sum land1
//sum land

//drop if elando ==.
//drop if elando ==0
drop if labors ==0
drop if labors==.
drop if quantitys ==.
//drop if resident ==.
//drop if resident ==0

replace eland=0 if eland==.
replace elabors=0 if elabors==.
replace e1=0 if e1==.
replace e2=0 if e2==.
replace e3=0 if e3==.
replace e4=0 if e4==.
replace e5=0 if e5==.
replace e6=0 if e6==.
replace e7=0 if e7==.
replace e8=0 if e8==.
replace e9=0 if e9==.
replace e10=0 if e10==.
replace e11=0 if e11==.
replace e12=0 if e12==.

replace s1=0 if s1==.
replace s2=0 if s2==.
replace s3=0 if s3==.
replace s4=0 if s4==.
g subsidys = s1+s2+s3+s4

replace sex=0 if sex!=1

g health = train3
replace health=4 if health==0
replace health=4 if health==.
replace health=4 if health>5
replace health=10 if health==1
replace health=9 if health==2
replace health=7 if health==2
replace health=6 if health==5
replace health=5 if health==10
replace health=4 if health==9
replace health=2 if health==7
replace health=1 if health==6

replace resident=0 if resident==.

replace familylabor=0 if familylabor==.

replace status1=0 if status1!=1
replace status2=0 if status2!=1
replace status3=0 if status3!=1
g status = status1+status2+status3
replace status=1 if status!=0

replace train1=0 if train1!=1
replace train2=0 if train2!=1
g train = train1+train2
replace train=1 if train!=0

replace insurances=0 if insurances==.
replace insurances=1 if insurances!=0

replace loan1=1 if loan1>0
replace loan1=0 if loan1!=1
replace loan2=1 if loan1>0
replace loan2=0 if loan2!=1
g loan=loan1+loan2
replace loan=1 if loan!=0

egen avg_agriincome = mean(agriincome)
replace agriincome=avg_agriincome if agriincome==.
egen avg_allincome = mean(allincome)
replace allincome=avg_allincome if allincome==.
egen avg_age = mean(agee)
replace agee=avg_age if agee==.
egen avg_educ = mean(educ)
replace educ=avg_educ if educ==.


g machanes = e8+e9+e10
g fertiles = e2+e3
g ots = e1+e4+e5+e6+e7+e12
g flabors = labors-elabors



//tostring id, replace 
//gen sm=substr(id,1,2)
//egen yearsm = concat(year sm)
//g wage0 = elaborpay/elabors
//bysort yearsm:egen wageo=mean(wage0)
//bysort yearsm:egen quantitye = mean(quantity)

winsor quantitys, gen (quantity) p(0.01)
//winsor agriincomes, gen (agriincome) p(0.001)
//winsor allincomes, gen (allincome) p(0.001)
winsor agee, gen (age) p(0.01)

g laboro = labors/land
winsor laboro, g (labor) p(0.01)   
g elaboro = elabors/land
//winsor elabor0, gen (elaboro) p(0.001) 
g flaboro = labor - elaboro

g fertile0 = fertiles/land
g machane0 = machanes/land
g ot0 = ots/land
g subsidy0 = subsidys/eland
winsor fertile0, gen (fertileo) p(0.01)
winsor machane0, gen (machaneo) p(0.01)
winsor ot0, gen (oto) p(0.1)
winsor subsidy0, gen (subsidyo) p(0.01)

g plots = eland/quantity
egen ave_plots = mean(plots)
replace plots=ave_plots if plots==.

egen avg_sub = mean(subsidyo)
replace subsidyo=avg_sub if subsidyo==.

g job = 1-agriincome/allincome
replace job=0 if job<0
egen avg_job = mean(job)
replace job=avg_job if job==.


g flabor = flaboro+0.001
g elabor = elaboro+0.001    
g fertile = fertileo+0.001
g machane = machaneo+0.001
g ot = oto+0.001
g subsidy = subsidyo+0.001


g fstructs = familylabor/resident
egen avg_fstruct = mean(fstructs)
replace fstructs=avg_fstruct if fstruct==.
winsor fstructs, gen (fstruct) p(0.01)

g caland = land 
replace caland = 1 if land>=0&land<10
replace caland = 2 if land>=10&land<50
replace caland = 3 if land>=50

tabstat yield land labor flabor elabor fertile machane ot subsidy insurance loan plots fstruct job sex age educ train health status, statistics(mean ma mi q sk k sd n) 


**删除面板重复值
duplicates drop id year,force
xtset id year

**分类均值、最大值、最小值、25%、50%、75%、偏度、峰度、标准差分析
xtsum yield land labor  flabor elabor fertile machane ot subsidy insurance loan plots fstruct job sex age educ train health status
tabstat yield land labor  flabor elabor fertile machane ot subsidy insurance loan plots fstruct job sex age educ train health status, by(caland) statistics(mean ma mi q sk k sd n) 
tabstat yield land labor  flabor elabor fertile machane ot subsidy insurance loan plots fstruct job sex age educ train health status if year==2011, by(caland) statistics(mean ma mi q sk k sd n)
tabstat yield land labor  flabor elabor fertile machane ot subsidy insurance loan plots fstruct job sex age educ train health status if year==2012, by(caland) statistics(mean ma mi q sk k sd n)
tabstat yield land labor  flabor elabor fertile machane ot subsidy insurance loan plots fstruct job sex age educ train health status if year==2013, by(caland) statistics(mean ma mi q sk k sd n)
tabstat yield land labor  flabor elabor fertile machane ot subsidy insurance loan plots fstruct job sex age educ train health status if year==2014, by(caland) statistics(mean ma mi q sk k sd n)
tabstat yield land labor  flabor elabor fertile machane ot subsidy insurance loan plots fstruct job sex age educ train health status if year==2015, by(caland) statistics(mean ma mi q sk k sd n)

**方差分析
oneway yield caland, mean bonferroni 
//oneway income cland, mean bonferroni 
//oneway profit cland, mean bonferroni 
oneway labor caland, mean bonferroni 
oneway flabor caland, mean bonferroni 
oneway elabor caland, mean bonferroni 
oneway machane caland, mean bonferroni 
oneway ot caland, mean bonferroni 
oneway subsidy caland, mean bonferroni 
oneway insurance caland, mean bonferroni 
oneway loan caland, mean bonferroni 
oneway plots caland, mean bonferroni 
oneway fstruct caland, mean bonferroni 
oneway job caland, mean bonferroni 
oneway sex caland, mean bonferroni 
oneway age caland, mean bonferroni 
oneway educ caland, mean bonferroni 
oneway train caland, mean bonferroni 
oneway health caland, mean bonferroni 
oneway status caland, mean bonferroni 

***生成交叉项
g lnyield = ln(yield)
g lnland = ln(land)
g lnlabor = ln(labor)
g lnflabor = ln(flabor)
g lnelabor = ln(elabor)
g lnfertile = ln(fertile)
g lnmachane = ln(machane)
g lnot = ln(ot)
g lnsubsidy = ln(subsidy)
g lninsurance = ln(insurance)
g lnland2 = lnland^2
g lnlabor2 = lnlabor^2
g lnflabor2 = lnflabor^2
g lnelabor2 = lnelabor^2
g lnfertile2 = lnfertile^2
g lnmachane2 = lnmachane^2
g lnot2 = lnot^2
g lnlaborlnfertile = lnlabor*lnfertile
g lnlaborlnmachane = lnlabor*lnmachane
g lnlaborlnot = lnlabor*lnot
g lnflaborlnelabor = lnflabor*lnelabor
g lnflaborlnfertile = lnflabor*lnfertile
g lnflaborlnmachane = lnflabor*lnmachane
g lnflaborlnot = lnflabor*lnot
g lnelaborlnfertile = lnelabor*lnfertile
g lnelaborlnmachane = lnelabor*lnmachane
g lnelaborlnot = lnelabor*lnot
g lnfertilelnmachane = lnfertile*lnmachane
g lnfertilelnot = lnfertile*lnot
g lnmachanelnot = lnmachane*lnot

**简单的相关性检验
//reg lnprofit lnland
pwcorr lnyield lnland,sig
//pwcorr lnprofit lnland,sig
qui reg lnyield lnland land lnlabor lnfertile lnmachane lnot lnlabor2 lnfertile2 lnmachane2 lnot2 lnlaborlnfertile lnlaborlnmachane lnlaborlnot lnfertilelnmachane lnfertilelnot lnmachanelnot sex age educ train health status  fstruct plots  insurance loan job lnsubsidy
estat vif
qui reg lnyield lnland land lnlabor lnfertile lnmachane lnot sex age educ train health status  fstruct plots  insurance loan job lnsubsidy
estat vif
pwcorr lnlabor lnfertile lnmachane lnot lnlabor2 lnfertile2 lnot2 lnlaborlnfertile lnlaborlnmachane lnlaborlnot lnfertilelnmachane lnfertilelnot lnmachanelnot

xtreg lnlabor lnland lnland2,fe r
xtreg lnflabor lnland lnland2,fe r
xtreg lnelabor lnland lnland2,fe r
xtreg lnfertile lnland lnland2,fe r
xtreg lnmachane lnland lnland2,fe r
xtreg lnot lnland lnland2,fe r

**混合回归、固定效应回归or随机效应回归？
qui xtreg lnyield lnland land lnlabor lnfertile lnmachane lnot lnlabor2 lnfertile2 lnmachane2 lnot2 lnlaborlnfertile lnlaborlnmachane lnlaborlnot lnfertilelnmachane lnfertilelnot lnmachanelnot sex age educ train health status  fstruct plots  insurance loan job lnsubsidy, re r
xtoverid
reg lnyield lnland land lnlabor lnfertile lnmachane lnot lnlabor2 lnfertile2 lnmachane2 lnot2 lnlaborlnfertile lnlaborlnmachane lnlaborlnot lnfertilelnmachane lnfertilelnot lnmachanelnot sex age educ train health status  fstruct plots  insurance loan job lnsubsidy i.sm i.year, vce(cluster id)
xtreg lnyield lnland land lnlabor lnfertile lnmachane lnot lnlabor2 lnfertile2 lnmachane2 lnot2 lnlaborlnfertile lnlaborlnmachane lnlaborlnot lnfertilelnmachane lnfertilelnot lnmachanelnot sex age educ train health status  fstruct plots  insurance loan job lnsubsidy i.year, re r
xtreg lnyield lnland land lnlabor lnfertile lnmachane lnot lnlabor2 lnfertile2 lnmachane2 lnot2 lnlaborlnfertile lnlaborlnmachane lnlaborlnot lnfertilelnmachane lnfertilelnot lnmachanelnot sex age educ train health status  fstruct plots  insurance loan job lnsubsidy i.year, fe r


**二次项和交叉项联合检验
tab year, gen(y)
xtreg lnyield lnland land lnlabor lnfertile lnmachane lnot lnlabor2 lnfertile2 lnmachane2 lnot2 lnlaborlnfertile lnlaborlnmachane lnlaborlnot lnfertilelnmachane lnfertilelnot lnmachanelnot sex age educ train health status  fstruct plots  insurance loan job lnsubsidy y2-y5, fe vce(cluster id)
test lnlabor2 lnfertile2 lnmachane2 lnot2 lnlaborlnfertile lnlaborlnmachane lnlaborlnot lnfertilelnmachane lnfertilelnot lnmachanelnot
test y2 y3 y4 y5

