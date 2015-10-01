dim (vv)

v5 <- readRDS('vol5.Rds')
v10 <- readRDS('vol10.Rds')
v15 <- readRDS('vol15.Rds')
v20 <- readRDS('vol20.Rds')
fix (v20)

v5 <- select(v5, god=a_int_y, god_rod=a_born_y, pol=ah5, m_rog=ai3, work_close=aj22, doxod=aj62, prava=aj63, uvag=aj64, udov_life=aj65, region=psu, type_nas_p=status, brak=a_marst)
v5_1 <- select(v5, region)
v5 <- my_split(df = v5_1, my_sep = ',', nc = 2, n_ncol = 1, df2 = v5)
v5 <- mutate(v5, vozr=god-god_rod)
v5 <- filter (v5, vozr <=30 & vozr >= 14)
dim(v5)

v10 <- select(v10, god=f_int_y, god_rod=f_born_y, pol=fh5, m_rog=fi3, work_close=fj22, doxod=fj62, prava=fj63, uvag=fj64, udov_life=fj65, region=psu, type_nas_p=status, brak=f_marst)
v10_1 <- select(v10, region)
v10 <- my_split(df = v10_1, my_sep = ',', nc = 2, n_ncol = 1, df2 = v10)
v10 <- mutate(v10, vozr=god-god_rod)
v10 <- filter (v10, vozr <=30 & vozr >= 14)
dim (v10)

v15 <- select(v15, god=k_int_y, god_rod=k_born_y, pol=kh5, m_rog=ki3, work_close=kj22, doxod=kj62, prava=kj63, uvag=kj64, udov_life=kj65, region=psu, type_nas_p=status, brak=k_marst)
v15_1 <- select(v15, region)
v15 <- my_split(df = v15_1, my_sep = ',', nc = 2, n_ncol = 1, df2 = v15)
v15 <- mutate(v15, vozr=god-god_rod)
v15 <- filter (v15, vozr <=30 & vozr >= 14)
dim (v10)



v20 <- select(v20, god=p_int_y, god_rod=ph6, pol=PH5, m_rog=PI3, work_close=pj22, doxod=pj62, prava=pj63, uvag=pj64, udov_life=pj65, region, type_nas_p=status, brak=p_marst)
v20_1 <- select(v20, region)
v20 <- my_split(df = v20_1, my_sep = ',', nc = 2, n_ncol = 1, df2 = v20)
v20 <- mutate(v20, vozr=god-god_rod)
v20 <- filter (v20, vozr <=30 & vozr >= 14)
dim (v20)

v22 <- readRDS('vol22.Rds')
fix (v22)
v22 <- select(v22, god=r_int_y, god_rod=rh6, pol=rh5, m_rog=ri3, work_close=rj22, doxod=rj62, prava=rj63, uvag=rj64, udov_life=rj65, region, type_nas_p=status, brak=r_marst)
v22_1 <- select(v22, region)
v22 <- my_split(df = v22_1, my_sep = ',', nc = 2, n_ncol = 1, df2 = v22)
v22 <- mutate(v22, vozr=god-god_rod)
v22 <- filter (v22, vozr <=30 & vozr >= 14)
dim (v22)


dat <- rbind(v5, v10, v15, v20, v22)
dim (dat)


table (dat$vol)

dat[dat$god==1995] <- '?????????? ??????????'
summary (dat$god)
dat$god <- as.factor (dat$god)
fix (dat)

dat <- readRDS('dan.Rds')
dat$vol[dat$god==1994 | dat$god==1995] <- 1994
dat$vol[dat$god==2005 | dat$god==2006] <- 2006 
dat$vol[dat$god==2011 | dat$god==2012] <- 2011
dat$vol[dat$god==2013 | dat$god==2014] <- 2014
dat$vol[dat$god==2001] <- 2001
saveRDS(dat, 'dan.Rds')
table (dat$pol)




dat$pol <- tolower(dat$pol)

dat$pol <- substr(dat$pol, 2,8)
dat$pol[dat$pol=='мужской' | dat$pol=='ужской'] <- 'мужской'
dat$pol [dat$pol=='женский' | dat$pol=='енский'] <- 'женский'
dat$pol[dat$pol==grep('мужск', dat$pol)] <- 'мужской'

saveRDS(dat, 'dan.Rds')

table (dat$pol)

install.packages('tm')
library (tm)
library (stringr)
dat$pol[dat$pol=="Мужской" | dat$pol=="\\'Мужской\\'" | dat$pol=="МУЖСКОЙ"] <- "Мужской"
ttt <- dat$pol[dat$pol=="'Мужской'"]
fix (ttt)
tt2 <- substr(txt, 2, 8)
tt2
table (dat$pol)
fix (dat)
dat$pol <- replace(dat$pol, "'", '')
txt <- as.vector (dat$pol)
txt <- tolower(txt)
tt <- removePunctuation(enc2utf8(txt))
fix (tt)
dat <- data.frame(dat, tt)
table (dat$tt)
stopwords <- c("‘", "’")
tt <- removeWords(txt, stopwords)
removeSparseTerms(txt, stopwords)



# обработка места проживания

table (dat$m_rog)
dat$m_rog <- tolower(dat$m_rog)
dd <- grep('в городе', dat$m_rog)
fix (dd)
dd
dat$m_rog[grep('в городе', dat$m_rog)] <- 'Город'
dat$m_rog[grep('городского типа', dat$m_rog)] <- 'ПГТ'
dat$m_rog[grep('в селе', dat$m_rog)] <- 'село'
dat$m_rog[grep('ответ', dat$m_rog)] <- 'Нет ответа'
saveRDS(dat, 'dan.Rds')



#обработка ситуации с работой
table (dat$work_close)
dat$work_close <- tolower(dat$work_close)

dat$work_close[grep('затрудняюсь', dat$work_close)] <- 'Затрудняюсь ответить'
dat$work_close[grep('не очень уверены', dat$work_close)] <- 'Не очень уверены'
dat$work_close[grep('полностью уверены', dat$work_close)] <- 'Полностью уверены'
dat$work_close[grep('совсем не уверены', dat$work_close)] <- 'Совсем не уверены'
dat$work_close[grep('и да, и нет', dat$work_close)] <- 'И да и нет'
dat$work_close[grep('нет ответа', dat$work_close)] <- 'Нет ответа'
dat$work_close[grep('скорее уверены', dat$work_close)] <- 'Скорее уверены'
saveRDS(dat, 'dan.Rds')



#удовлетворенность жизнью
table (dat$udov_life)
dat$udov_life <- tolower(dat$udov_life)

dat$udov_life[grep('затрудняюсь ответить', dat$udov_life)] <- 'Затрудняюсь ответить'
dat$udov_life[grep('не очень удовлетворены', dat$udov_life)] <- 'Не очень удовлетворены'
dat$udov_life[grep('отказ от ответа', dat$udov_life)] <- 'Отказ от ответа'
dat$udov_life[grep('скорее удовлетворены', dat$udov_life)] <- 'Скорее удовлетворены'
dat$udov_life[grep('и да, и нет', dat$udov_life)] <- 'И да, и нет'
dat$udov_life[grep('полностью удовлетворены', dat$udov_life)] <- 'Полностью удовлетворены'
dat$udov_life[grep('нет ответа', dat$udov_life)] <- 'Нет ответа'
dat$udov_life[grep('совсем не удовлетворены', dat$udov_life)] <- 'Совсем не удовлетворены'


str (dat)



# тип населенного пункта, где проживает респондент
table (dat$type_nas_p)
dat$type_nas_p <- tolower(dat$type_nas_p)

dat$type_nas_p[grep('город', dat$type_nas_p)] <- 'Город'
dat$type_nas_p[grep('село', dat$type_nas_p)] <- 'Село'
dat$type_nas_p[grep('пгт', dat$type_nas_p)] <- 'ПГТ'
dat$type_nas_p[grep('областной центр', dat$type_nas_p)] <- 'Областной центр'


# брак
table (dat$brak)
dat$brak <- tolower(dat$brak)

dat$brak[grep('вдова', dat$brak)] <- 'Вдовец, вдова'
dat$brak[grep('живете вместе', dat$brak)] <- 'Живут вместе, без регистрации'
dat$brak[grep('затрудняюсь ответить', dat$brak)] <- 'Затрудняюсь ответить'
dat$brak[grep('никогда в браке не состояли', dat$brak)] <- 'Никогда в браке не состояли'
dat$brak[grep('нет ответа', dat$brak)] <- 'Отказ от ответа'
dat$brak[grep('официально зарегистрированы', dat$brak)] <- 'Официально зарегистрированы, но не проживают'
dat$brak[grep('разведены', dat$brak)] <- 'Разведены и в браке не состоят'
dat$brak[grep('состоите в зарегистрированном браке', dat$brak)] <- 'Состоят в зарегистрированном браке'


# убираем точку в конце названий регионов
dat$n_var <- removePunctuation(dat$n_var)
fix (dat)



