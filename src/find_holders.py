#!/usr/bin/python3

import xml.etree.ElementTree as ET

print('Fetching info from xml...', end='\r')
result_list = list()
tree = ET.parse('../data/Deputados.xml')
root = tree.getroot()[0]

for congressman in root:
    if congressman[1].text == '54' and congressman[7].text == 'Efetivado':
        result_list.append(congressman[2].text)
print('Fetching data from xml... Done')

print('List of congressman:')
for congressman in result_list:
    print(congressman)
