
**第一步，重新处理数据，需要的数据有
**单一玉米回归
**包含的变量有（yieldo、incomeo、profito、lando、cland、labors、flabors、elabors、resident、machines、ots、subsidys、insurances、eland、quantity、
//              familylabor、resident、agriincome、allincome、sex、age、educ、train、health、status）



**一、选取变量及处理极端值

**选取变量(删除收获面积、年末耕地面积、家庭常住人口、家庭全年总收入为0的农户数据，并将部分变量+0.001）
drop if lando ==.
drop if lando==0
drop if eland ==.
drop if eland ==0
drop if resident ==.
drop if resident ==0
drop if allincome ==.
drop if allincome ==0
drop if quantity ==.
drop if quantity ==0
g labor0 = labors/lando
g flabor0 = flabors/lando
g elabor0 = elabors/lando
g machine0 = machines/lando
g ot0 = ots/lando
g subsidy0 = subsidys/eland
g insurance0 = insurances/eland
g plotso = eland/quantity
g fstructo = familylabor/resident
g job0o = agriincome/allincome
g laboro = labor0+0.001
g flaboro = flabor0+0.001
g elaboro = elabor0+0.001
g machineo = machine0+0.001
g oto = ot0+0.001
g subsidyo = subsidy0+0.001
g insuranceo = insurance0+0.001

**关键变量“yield、income、profit、land”直接删除样本两侧2.5%的异常值，非关键变量用winsor命令replace两侧0.01%的异常值
winsor yieldo, gen (yield) p(0.025)
use ".dta",clear
drop if yield>
drop if yield<

use ".dta",clear
winsor lando,gen (land) p(0.01)
drop if land>

winsor profito, gen (profit) p(0.025)
use ".dta",clear
drop if profit<
drop if profit>

winsor laboro, gen(labor) p(0.01)
winsor flaboro, gen (flabor) p(0.01)
winsor elabor, gen (elabor) p(0.01)
winsor machineo, gen (machine) p(0.01)
winsor oto, gen (ot) p(0.01)
winsor subsidyo, gen (subsidy) p(0.01)
winsor insuranceo, gen (insurance) p(0.01)
winsor fstructo, gen (fstruct) p(0.01)
winsor jobo, gen (job) p(0.01)
winsor wageo, gen (wage) p(0.01)

**二、统计性描述（总体和分类）

**检查变量是否唯一
isid id year

**生成land区间和相关统计行描述
// g cland = land 
//replace cland = 1 if land>0&land<5
//replace cland = 2 if land>=5&land<10
//replace cland = 3 if land>=10&land<20
//replace cland = 4 if land>=20&land<50
//replace cland = 5 if land>=50
//oneway price cland, mean bonferroni 
 
g cland = land 
replace cland = 1 if land>0&land<10
replace cland = 2 if land>=10&land<50
replace cland = 3 if land>=50

**生成新的分类变量求市场利润，提取id前2位数字为省码
tostring id, replace 
gen sm=substr(id,1,2)
egen yearsm = concat(year sm)
bysort yearsm:egen wageo=mean(wagesum)
g wage = wageo/elaborsum
g laborwage = flabor*wage
g profitmarket = profit - laborwage

**分类均值、最大值、最小值和方差分析
bys cland:sum yield income profit land labor flabor elabor machine ot subsidy insurance plots fstruct job sex age educ train health status
tabstat yield income profit profitmarket land labor flabor elabor machine ot subsidy insurance plots fstruct job sex age educ train health status , statistics(mean sd) by(cland)
ta cland 

**用excel计算面板中个体数量有多少


**二、实证分析的准备


**简单的相关性检验
pwcorr lnyield lnland,sig
pwcorr lnincome lnland,sig
pwcorr lnprofit lnland,sig


**生成超越对数函数需要的变量
g lnyield = ln(yield)
g lnincome = ln(income)
g lnprofit = ln(profit)
g lnland = ln(land)
g lnlabor = ln(labor)
g lnflanbor = ln(flabor)
g lnelabor = ln(labor)
g lnmachine = ln(machine)
g lnot = ln(ot)
g lnsubsidy = ln(subsidy)
g lninsurance = ln(insurance)
g lnland2 = lnland^2
g dland = 1/land
g lnlabor2 = lnlabor^2
g lnflabor2 = lnflabor^2
g lnelabor2 = lnelabor^2
g lnmachine2 = lnmachine^2
g lnother2 = lnother^2
g lnlaborlnmachine = lnlabor*lnmachine
g lnlaborlnot = lnlabor*lnot
g lnflaborlnelabor = lnflabor*lnelabor
g lnflaborlnmachine = lnflabor*lnmachine
g lnflaborlnot = lnflabor*lnot
g lnelaborlnmachine = lnelabor*lnmachine
g lnelaborlnot = lnelabor*lnot
g lnmachinelnot = lnmachine*lnot



**混合回归、固定效应回归or随机效应回归？
**单产、影子利润、市场利润
xtreg  var1 var2 i.year i.sm , fe
estimates store FE
xtreg  var1 var2 i.year i.sm, re
estimates store RE
hausman FE RE,constant sigmamore

FE 中p值越小越说明 FE回归优于混合回归；
RE 中p值越小越说明 RE回归优于混合回归；
HAUSMAN 检验中 Prob>chi2 越小越说明 FE优于RE；

**三、做图准备工作
**生成新变量（均值）
egen mlnmachine = mean(lnmachine)
egen mlnlabor = mean(lnlabor)
egen mlnflanbor = mean(lnflabor)
egen mlnelabor = mean(lnlabor)
egen mlnmachine = mean(lnmachine)
egen mlnother = mean(lnother)
egen mlnsubsidy = mean(lnsubsidy)
egen mlninsurance = mean(lninsurance)
egen mlnflabor2 = mean(mlnflabor2)
egen mlnmachine2 = mean(lnmachine2)
egen mlnother2 = mean(lnother2)
egen mlnlaborlnmachine = mean(lnlaborlnmachine)
egen mlnlaborlnother = mean(lnlaborlnother)
egen mlnflaborlnelabor = mean(lnflaborlnelabor)
egen mlnflaborlnmachine = mean(lnflaborlnmachine)
egen mlnflaborlnother = mean(lnflaborlnother)
egen mlnelaborlnmachine = mean(lnelaborlnmachine)
egen mlnelaborlnother = mean(lnelaborlnother)
egen mlnmachinelnother = mean(lnmachinelnother)
egen mplots = mean(plots)
egen mfstruct = mean(fstruct)
egen mjob = mean(job)
egen msex = mean(sex)
egen mage = mean(age)
egen meduc = mean(educ)
egen mtrain = mean(train)
egen mhealth = mean(health)
egen mstatus = mean(status)

**混合回归
Reg 

**固定效应模型
xtreg lnyield lnland lnland2 alpha1 mlnmachine mlnlabor  mlnflanbor mlnelabor  mlnmachine mlnother mlnsubsidy mlninsurance mlnflabor2 mlnmachine2 mlnother2 mlnlaborlnmachine mlnlaborlnother mlnflaborlnelabor mlnflaborlnmachine mlnflaborlnother mlnelaborlnmachine mlnelaborlnother mlnmachinelnother mplots  mfstruct mjob msex mage meduc mtrain mhealth mstatus 
xtreg lnyield lnland land
xtreg lnyield lnland dland




**随机效应模型

固定效应和混合模型对比，代表要素市场不完善的影响
固定效应和随机效应对比，代表测量误差
	
predict lnyieldhat
predict lnincomehat
predict lnprofithat

**生成系数变量

**复制数据在excel上制图



