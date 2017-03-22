import sqlite3

def fetch_expenses(congressperson_id):
	con = sqlite3.Connection('local.db')
	cur = con.cursor()
	values = []

	for year in range (2009,2017):
		for month in range(1,13):
			cur.execute('SELECT SUM(net_value) FROM previous_years WHERE congressperson_id = ? AND month = ? AND year = ?',[congressperson_id,month,year])
			value = cur.fetchall()[0][0]
			if value is None:
				value = 0
			values.append(value)

	cur.close()
	con.close()
	return values

def fetch_party_expenses(party):
	con = sqlite3.Connection('local.db')
	cur = con.cursor()
	values = []

	for year in range (2009,2017):
		for month in range(1,13):
			cur.execute('SELECT SUM(net_value) FROM previous_years WHERE party = ? AND month = ? AND year = ?',[party,month,year])
			value = cur.fetchall()[0][0]
			if value is None:
				value = 0
			values.append(value)

	cur.close()
	con.close()
	return values

def fetch_state_expenses(state):
	con = sqlite3.Connection('local.db')
	cur = con.cursor()
	values = []

	for year in range (2009,2017):
		for month in range(1,13):
			cur.execute('SELECT SUM(net_value) FROM previous_years WHERE state = ? AND month = ? AND year = ?',[state,month,year])
			value = cur.fetchall()[0][0]
			if value is None:
				value = 0
			values.append(value)

	cur.close()
	con.close()
	return values