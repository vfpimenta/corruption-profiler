import pandas as pd
import sqlite3
import xml.etree.ElementTree as ET
from serenata_toolbox.chamber_of_deputies.presences_dataset import PresencesDataset

print('Fetching info from xml...', end='\r')
result_list = list()
tree = ET.parse('../data/Deputados.xml')
root = tree.getroot()[0]

for congressman in root:
  if congressman[1].text == '53':
    result_list.append((congressman[2].text,congressman[9].text))
print('Fetching data from xml... Done')

# print('Fetching info from databasse...', end='\r')
# con = sqlite3.Connection('../data/local.db')
# cur = con.cursor()
# result_list1 = list()

# sql = 'SELECT DISTINCT congressperson_id, congressperson_name, congressperson_document FROM previous_years WHERE legislature_53 = 1'
# for row in cur.execute(sql):
#   result_list1.append(row[0])

# cur.close()
# con.commit()
# con.close()
# print('Fetching data from databasse... Done')

print('Loading presences from url...', end='\r')
data = {'congressperson_name': [x[0] for x in result_list], 
  'congressperson_document': [x[1] for x in result_list]}
deputies = pd.DataFrame(data=data)
presences = PresencesDataset().fetch(deputies, '01/02/2011', '01/01/2015')
print('Loading info from url... Done')

print('Exporting data to csv...', end='\r')
presences.to_csv('../data/congressperson-presences-54.csv', sep=',')  
print('Exporting data to csv... Done')