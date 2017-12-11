import pandas as pd
import sqlite3
from serenata_toolbox.chamber_of_deputies.presences_dataset import PresencesDataset

print('Fetching info from databasse...', end='\r')
con = sqlite3.Connection('../data/local.db')
cur = con.cursor()
result_list = list()

sql = 'SELECT DISTINCT congressperson_name, congressperson_document FROM previous_years WHERE term_id = 54'
for row in cur.execute(sql):
  result_list.append(row)

cur.close()
con.commit()
con.close()
print('Fetching data from databasse... Done')

print('Loading presences from url...', end='\r')
data = {'congressperson_name': [x[0] for x in result_list], 
  'congressperson_document': [x[1] for x in result_list]}
deputies = pd.DataFrame(data=data)
presences = PresencesDataset().fetch(deputies, '01/02/2011', '01/01/2015')
print('Loading info from url... Done')

print('Exporting data to csv...', end='\r')
presences.to_csv('../data/congressman_presences.csv', sep=',')  
print('Exporting data to csv... Done')