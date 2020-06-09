
ncpt=c(3.507, 3.547, 3.608, 3.715, 3.776, 3.831, 3.835, 3.72)
costperday=c(469, 492, 506, 527, 556, 590, 626, 648)
year=seq(2004,2011)

tiff('Z:/j_scrdata/ascMMA/result/fig/number_cpt_perday.tif')
plot(year,ncpt,type='b',las=2,ylab='number of cpt codes per encouter day', xlab='year',main='numbre of procedures per day trend')
dev.off()
tiff('Z:/j_scrdata/ascMMA/result/fig/cost_perday.tif')
plot(year,costperday,type='b',las=2, ylab='cost per encouter day ($)', xlab='year',main='cost per day trend')
dev.off()
