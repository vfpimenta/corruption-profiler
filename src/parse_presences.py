#!/usr/bin/python3

import csv
import json
import sqlite3
import util
import unidecode
from datetime import datetime

cache = dict()
con = sqlite3.Connection('../data/local.db')
cur = con.cursor()

class ResultEmpty(Exception):
  pass

class ResultNotUnique(Exception):
  pass

def replace_template(arg):
  return 'replace(replace(replace(replace(replace(replace(replace(replace(replace(replace(replace({}, "Á","A"), "Ã","A"), "Â","A"), "É","E"), "Ê","E"), "Í","I"),"Ó","O") ,"Õ","O") ,"Ô","O"),"Ú","U"), "Ç","C")'.format(arg)

def fetch_unique(clause):
  

  result = None
  count = 0
  sql = 'SELECT DISTINCT congressperson_id FROM previous_years WHERE {}'.format(clause)
  for row in cur.execute(sql):
    count += 1
    result = row[0]

  if count < 1:
    raise ResultEmpty('No result found for query {}'.format(sql))
  elif count > 1:
    raise ResultNotUnique('Not unique result for query {}'.format(sql))
  else:
    return result

def get_gid(congressperson_name, congressperson_document):
  if congressperson_name in cache.keys():
    return cache.get(congressperson_name)

  try:
    clause = 'congressperson_name LIKE "{}" AND state = "{}"'.format(parse_name(congressperson_name), parse_state(congressperson_name))
    result = fetch_unique(clause)
    cache[congressperson_name] = result
    return result
  except ResultNotUnique as e:
    pass
    #print(e)
    #print('Trying again...')
  except ResultEmpty as e:
    pass
    #print(e)
    #print('Trying again...')

  try:
    clause = 'congressperson_name LIKE "{}" AND state = "{}"'.format(exclude_name(parse_name(congressperson_name), 'JÚNIOR'), parse_state(congressperson_name))
    result = fetch_unique(clause)
    cache[congressperson_name] = result
    return result
  except ResultNotUnique as e:
    pass
    #print(e)
    #print('Trying again...')
  except ResultEmpty as e:
    pass
    #print(e)
    #print('Trying again...')

  try:
    clause = 'congressperson_name LIKE "{}" AND state = "{}"'.format(decode_name(congressperson_name), parse_state(congressperson_name))
    result = fetch_unique(clause)
    cache[congressperson_name] = result
    return result
  except ResultNotUnique as e:
    pass
    #print(e)
    #print('Trying again...')
  except ResultEmpty as e:
    pass
    #print(e)
    #print('Trying again...')

  try:
    clause = 'congressperson_name LIKE "%{}%" AND state = "{}"'.format(surname(congressperson_name), parse_state(congressperson_name))
    result = fetch_unique(clause)
    cache[congressperson_name] = result
    return result
  except ResultNotUnique as e:
    pass
    #print(e)
    #print('Trying again...')
  except ResultEmpty as e:
    pass
    #print(e)
    #print('Trying again...')

  try:
    clause = '{} LIKE "{}" AND state = "{}"'.format(replace_template('congressperson_name'), parse_name(congressperson_name), parse_state(congressperson_name))
    result = fetch_unique(clause)
    cache[congressperson_name] = result
    return result
  except ResultNotUnique as e:
    pass
    #print(e)
    #print('Trying again...')
  except ResultEmpty as e:
    pass
    #print(e)
    #print('Trying again...')

  try:
    clause = 'congressperson_name LIKE "%{}%" AND state = "{}"'.format(parse_name(congressperson_name), parse_state(congressperson_name))
    result = fetch_unique(clause)
    cache[congressperson_name] = result
    return result
  except ResultNotUnique as e:
    raise e
  except ResultEmpty as e:
    raise e  

  # sql = 'SELECT DISTINCT congressperson_id FROM previous_years WHERE (congressperson_name LIKE "%{}%" AND state = "{}") OR (congressperson_name LIKE "%{}%" AND state = "{}") OR (congressperson_name LIKE "%{}%" AND congressperson_document = {} AND state = "{}")'.format(  
  #     parse_name(congressperson_name),
  #     parse_state(congressperson_name),
  #     decode_name(congressperson_name), 
  #     parse_state(congressperson_name),
  #     surname(congressperson_name), 
  #     congressperson_document,
  #     parse_state(congressperson_name))
  # #print('Sqlite 3: {}'.format(sql))
  # for row in cur.execute(sql):
  #   result = row[0]

  # if not result:
  #   sql = 'SELECT DISTINCT congressperson_id FROM previous_years WHERE ({} LIKE "%{}%" AND state = "{}") OR ({} LIKE "%{}%" AND state = "{}") OR ({} LIKE "%{}%" AND congressperson_document = {} AND state = "{}")'.format(
  #       replace_template('congressperson_name'), 
  #       parse_name(congressperson_name), 
  #       parse_state(congressperson_name),
  #       replace_template('congressperson_name'), 
  #       decode_name(congressperson_name),
  #       parse_state(congressperson_name), 
  #       replace_template('congressperson_name'), 
  #       surname(congressperson_name), 
  #       congressperson_document,
  #       parse_state(congressperson_name))
  #   #print('Sqlite 3: {}'.format(sql))
  #   for row in cur.execute(sql):
  #     result = row[0]

  

  # cache[congressperson_name] = result
  # return result

def get_index(str_date):
  date = datetime.strptime(str_date, '%Y-%m-%d %H:%M:%S')
  base_idx = (date.year-2011)*12 + date.month-1
  return base_idx-1

def exclude_name(str_name, *exclude):
  for ex in exclude:
    str_name = str_name.replace(ex,'')

  return str_name.strip()

def parse_name(str_name):  
  return str_name[:str_name.index('-')].upper().replace('`','\'').replace('DANGELO','D\'ANGELO').strip()

def parse_state(str_name):
  return str_name[str_name.index('/')+1:]

def surname(str_name):
  return parse_name(str_name)[parse_name(str_name).rfind(' ')+1:]

def decode_name(str_name):
  return unidecode.unidecode(parse_name(str_name))

def main():
  congressman_json = dict()
  with open('../data/congressperson-presences-54.csv','r') as csvfile:
    size = sum(1 for row in csvfile)
  with open('../data/congressperson-presences-54.csv','r') as csvfile:
    reader = csv.reader(csvfile)
    idx = 0
    for row in reader:
      idx += 1
      util.printProgressBar(idx, size, prefix='Parsing csv', suffix='Complete')
      if row[0] != '':
        gid = get_gid(row[3], row[2])
        if gid not in congressman_json.keys():
          congressman_json[gid] = [
            parse_name(row[3]),   # congressperson_name
            row[5],               # state
            row[4],               # party
            [0]*48,               # present_on_day 
            [0]*48                # presence
          ]

        if row[7] == 'Present':
          congressman_json[gid][3][get_index(row[6])] += 1
        if row[10] == 'Present':
          congressman_json[gid][4][get_index(row[6])] += 1

  with open('../data/JSON/standard/congressman_presences.json','w') as jsonfile:
    json.dump(congressman_json, jsonfile)

if __name__ == '__main__':
  main()
  cur.close()
  con.close()