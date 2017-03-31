import sqlite3

con = sqlite3.Connection('../data/local.db')
cur = con.cursor()
cur.execute('CREATE TABLE "calendar" ("year" INTEGER, "month" INTEGER);')
data = []

for year in range(2009,2017):
	for month in range(1,13):
		data.append((year,month))

cur.executemany('INSERT INTO "calendar" VALUES (?,?)', data)

cur.close()
con.commit()
con.close()
