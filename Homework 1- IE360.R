require(readxl) # to read excel input
require(data.table) # to use data.table functionalities
require(corrplot)
require(anytime)
require(xts)

data_path = '/Users/Asus/Downloads/EVDS.xlsx'

# read the data
dat = read_excel(data_path, sheet='EVDS', n_max=152)

# transform the data
evds = data.table(dat)

# display the structure of the data
str(evds)

# rename the data
rename_evds = c("date", "dollar_selling", "euro_selling", "cpty_ute_mfg", "unemp_rate")
setnames(evds, names(evds), rename_evds)

csv_path = '/Users/Asus/Downloads/multiTimeline.csv'

# import data
trends=fread(csv_path, skip = 1)

# rename the data
rename_gt = c("date", "sv_jobs_turkey")
names(trends) = rename_gt

# display the head of the data
head(trends, 5)

# merge the data
merged_data = merge(evds, trends, fill=NA, join = "left")

# check missing points
sum(is.na(merged_data))
colSums(is.na(merged_data))
rowSums(is.na(merged_data))
tail(merged_data)

# it is found that missing values belong to last 3 months
# due to pandemic, last three months' data will not provide 
# desired contribution to the analysis. Therefore, it is wisely
# to omit rows with NA values.
data_omit = na.omit(merged_data)
str(data_omit)          

# create date object
dates = data_omit$date
date_object = anydate(dates)
dates_format = as.Date(date_object,"%Y-%m")
str(dates_format)

data_omit$date=dates_format
data_omit$sv_jobs_turkey=as.numeric(data_omit$sv_jobs_turkey)
str(data_omit)

subset_data_omit = subset(data_omit, data_omit$date>= "2015-01-01")
str(subset_data_omit)

# create the xts object
time_series = xts(subset_data_omit, order.by = subset_data_omit$date)
is.xts(time_series)
str(time_series)

# observe the relationship between data
pairs(subset_data_omit, main = "Interrelationship Between Data", labels = c("Date", "Dollar Selling Level","Euro Selling Level","Capacity Utilization Rate","Unemployment Rate","Search Volume"))

# plot the change of exchange rates over time
ts.plot(cbind(time_series$dollar_selling,time_series$euro_selling), main = "The Change of Exhange Rates over Time", xlab="Date", ylab="Exhange Rates", col= c(rgb(0.2,0.4,0.1,0.7),rgb(0.8,0.4,0.1,0.7)))
legend("topleft", legend= c("Dollar Selling Level", "Euro Selling Level"), col = c(rgb(0.2,0.4,0.1,0.7),rgb(0.8,0.4,0.1,0.7)), pch=16)

# correlation calculations
# correlation between dollar selling and other elements
cor_dol_euro = cor(subset_data_omit$dollar_selling,subset_data_omit$euro_selling)
cor_dol_cpty = cor(subset_data_omit$dollar_selling,subset_data_omit$cpty_ute_mfg)
cor_dol_ur = cor(subset_data_omit$dollar_selling,subset_data_omit$unemp_rate)
cor_dol_sv = cor(subset_data_omit$dollar_selling,subset_data_omit$sv_jobs_turkey)
corr_dol = cbind(cor_dol_euro,cor_dol_cpty,cor_dol_ur,cor_dol_sv)
corrplot(corr_dol)

# correlation between euro selling and other elements
cor_euro_dollar = cor(subset_data_omit$dollar_selling,subset_data_omit$euro_selling)
cor_euro_cpty = cor(subset_data_omit$euro_selling,subset_data_omit$cpty_ute_mfg)
cor_euro_ur = cor(subset_data_omit$euro_selling,subset_data_omit$unemp_rate)
cor_euro_sv = cor(subset_data_omit$euro_selling,subset_data_omit$sv_jobs_turkey)
corr_euro = cbind(cor_euro_dollar,cor_euro_cpty,cor_euro_ur,cor_euro_sv)
corrplot(corr_euro)

# check the relationship between capacity utilization rate of 
# manufacturing industry and unemployment rate as well as 
# search volume of job advertisement on Google from Turkey
cor_cpty_ur = cor(subset_data_omit$cpty_ute_mfg,subset_data_omit$unemp_rate)
cor_cpty_sv = cor(subset_data_omit$cpty_ute_mfg,subset_data_omit$unemp_rate)
corr_cpty = cbind(cor_cpty_ur, cor_cpty_sv)
corrplot(corr_cpty)

# lastly, check the relationship between unemployment rate and
# search volume of job advertisement on Google from Turkey
cor_ur_sv = cor(subset_data_omit$sv_jobs_turkey,subset_data_omit$unemp_rate)
corr_ur=cbind(cor_ur_sv)
corrplot(corr_ur)
