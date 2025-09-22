import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import matplotlib.dates as mdates
from datetime import datetime
from matplotlib.dates import YearLocator

 
# max data gap to fill
nlim=10
start_date = "1897-01-01"
end_date = "2023-10-22"

df_tol = pd.read_csv("../ice_data/air_temp_data/toledo_all.csv",usecols=['DATE','TAVE'])
df_tol['DATE']=pd.to_datetime(df_tol['DATE'],format='%Y-%m-%d')
df_tol=df_tol.set_index('DATE')
df_tol=df_tol[start_date:end_date].interpolate(limit=nlim)
print(df_tol[df_tol.index.duplicated()])


df_eri = pd.read_csv("../ice_data/air_temp_data/erie_all.csv",usecols=['DATE','TAVE'])
df_eri['DATE']=pd.to_datetime(df_eri['DATE'],format='%Y-%m-%d')
df_eri=df_eri.set_index('DATE')
df_eri=df_eri[start_date:end_date].interpolate(limit=nlim)
print(df_eri[df_eri.index.duplicated()])

df_det = pd.read_csv("../ice_data/air_temp_data/detroit_all.csv",usecols=['DATE','TAVE'])
df_det['DATE']=pd.to_datetime(df_det['DATE'],format='%Y-%m-%d')
df_det=df_det.set_index('DATE')
df_det=df_det[start_date:end_date].interpolate(limit=nlim)
print(df_det[df_det.index.duplicated()])

df_clv = pd.read_csv("../ice_data/air_temp_data/cleveland_all.csv",usecols=['DATE','TAVE'])
df_clv['DATE']=pd.to_datetime(df_clv['DATE'],format='%Y-%m-%d')
df_clv=df_clv.set_index('DATE')
df_clv=df_clv[start_date:end_date].interpolate(limit=nlim)
print(df_clv[df_clv.index.duplicated()])

df_buf = pd.read_csv("../ice_data/air_temp_data/buffalo_all.csv",usecols=['DATE','TAVE'])
df_buf['DATE']=pd.to_datetime(df_buf['DATE'],format='%Y-%m-%d')
df_buf=df_buf.set_index('DATE')
df_buf=df_buf[start_date:end_date].interpolate(limit=nlim)
print(df_buf[df_buf.index.duplicated()])

df_ave = df_tol
df_ave['tol'] = df_tol['TAVE']
df_ave['eri'] = df_eri['TAVE']
df_ave['det'] = df_det['TAVE']
df_ave['clv'] = df_clv['TAVE']
df_ave['buf'] = df_buf['TAVE']
df_ave['all_ave'] = df_ave[["eri","det","clv","buf","tol"]].mean(axis=1)



# save to a csv file
#df_ave['all_ave'].to_csv("Lake_Erie_Average.csv")


#print(df_ave.head())

fig, ax = plt.subplots(3, sharex=False,figsize=(15,7))


values = [ df_ave['tol'], df_ave['eri'], df_ave['det'], df_clv['TAVE'], df_buf['TAVE'] ]
labels = ['Toledo', 'Erie', 'Detroit', 'Cleveland', 'Buffalo']
ax[0].boxplot( values, tick_labels=labels )
print(df_tol['TAVE'].max())
ax[0].text(0.7,35,'Lake Erie Locations')
ax[0].set_ylim(-45,45)

mper=360

CB_color_cycle = ['#377eb8', '#ff7f00', '#4daf4a',
                  '#f781bf', '#a65628', '#984ea3',
                  '#999999', '#e41a1c', '#dede00','#CC79A7']

ax[1].plot_date(df_tol.index,df_tol['TAVE'].rolling(mper,center=True,min_periods=mper).mean(),'-',color=CB_color_cycle[5],label="Toledo")
ax[1].plot_date(df_eri.index,df_eri['TAVE'].rolling(mper,center=True,min_periods=mper).mean(),'-',color=CB_color_cycle[6],label="Erie")
ax[1].plot_date(df_det.index,df_det['TAVE'].rolling(mper,center=True,min_periods=mper).mean(),'-',color=CB_color_cycle[7],label="Detroit")
ax[1].plot_date(df_clv.index,df_clv['TAVE'].rolling(mper,center=True,min_periods=mper).mean(),'-',color=CB_color_cycle[8],label="Cleveland")
ax[1].plot_date(df_buf.index,df_buf['TAVE'].rolling(mper,center=True,min_periods=mper).mean(),'-',color=CB_color_cycle[9],label="Buffalo")
ax[1].legend(loc='center',bbox_to_anchor=(0.5, -0.1),ncol=5)
ax[1].set_ylabel('surface air temperature [$^oC$]')
ax[1].tick_params(labelbottom=False)  
ax[1].text(df_tol.index[25],df_tol['TAVE'].rolling(mper,center=True,min_periods=mper).mean().max()-0.3,'Lake Erie Locations')


# plot all lakes
dfs = pd.read_csv("../ice_data/air_temp_data/Lake_Superior_Average.csv")
dfs['DATE']=pd.to_datetime(dfs['DATE'],format='%Y-%m-%d')
dfs=dfs.set_index('DATE')

dfh = pd.read_csv("../ice_data/air_temp_data/Lake_Huron_Average.csv")
dfh['DATE']=pd.to_datetime(dfh['DATE'],format='%Y-%m-%d')
dfh=dfh.set_index('DATE')

dfm = pd.read_csv("../ice_data/air_temp_data/Lake_Michigan_Average.csv")
dfm['DATE']=pd.to_datetime(dfm['DATE'],format='%Y-%m-%d')
dfm=dfm.set_index('DATE')

dfe = pd.read_csv("../ice_data/air_temp_data/Lake_Erie_Average.csv")
dfe['DATE']=pd.to_datetime(dfe['DATE'],format='%Y-%m-%d')
dfe=dfe.set_index('DATE')

dfo = pd.read_csv("../ice_data/air_temp_data/Lake_Ontario_Average.csv")
dfo['DATE']=pd.to_datetime(dfo['DATE'],format='%Y-%m-%d')
dfo=dfo.set_index('DATE')

mper=360

#ax[2].plot_date(dfe.index,dfe['all_ave'].rolling(mper,center=True,min_periods=mper).mean(),'-',color='r',label="station mean, Lake Erie")
ax[2].plot_date(dfs.index,dfs['all_ave'].rolling(mper,center=True,min_periods=mper).mean(),'-',color=CB_color_cycle[0],label="Lake Superior")
ax[2].plot_date(dfh.index,dfh['all_ave'].rolling(mper,center=True,min_periods=mper).mean(),'-',color=CB_color_cycle[1],label="Lake Huron")
ax[2].plot_date(dfm.index,dfm['all_ave'].rolling(mper,center=True,min_periods=mper).mean(),'-',color=CB_color_cycle[2],label="Lake Michigan")
ax[2].plot_date(dfe.index,dfe['all_ave'].rolling(mper,center=True,min_periods=mper).mean(),'-',color=CB_color_cycle[3],label="Lake Erie")
ax[2].plot_date(dfo.index,dfo['all_ave'].rolling(mper,center=True,min_periods=mper).mean(),'-',color=CB_color_cycle[4],label="Lake Ontario")
ax[2].set_ylim(0,14)
ax[2].legend(loc='center',bbox_to_anchor=(0.5, -0.3),ncol=5)


# axis adjustment
ax[1].xaxis.set_minor_locator(YearLocator(5))
ax[2].xaxis.set_minor_locator(YearLocator(5))

fig.savefig("LakeErie_airtempv2.png",dpi=150)
#plt.show()


