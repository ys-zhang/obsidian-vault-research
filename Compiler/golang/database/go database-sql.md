# `sql.DB`

To access databases in Go, you use a `sql.DB`. You use this type to *create statements and transactions, execute queries, and fetch results*.

but  **a `sql.DB` isn’t a database connection**.

`sql.DB` performs some important tasks for you behind the scenes:
-   It **opens and closes connection**s to the actual underlying database, via the driver.
-   It **manages a pool of connection**s as needed, which may be a variety of things as mentioned.

The `sql.DB` abstraction is designed to keep you from worrying about how to manage concurrent access to the underlying datastore.

A connection is marked in-use when you use it to perform a task, and then returned to the available pool when it’s not in use anymore. One consequence of this is that **if you fail to release connections back to the pool, you can cause `sql.DB` to open a lot of connections**

# `sql.Open`

Perhaps counter-intuitively, `sql.Open()` **does not establish any connections to the database**, nor does it validate driver connection parameters.

The first actual connection to the underlying datastore will be established lazily, when it’s needed for the first time.

> `db.Ping` check right away that the database is available and accessible

Although it’s idiomatic to `Close()` the database when you’re finished with it, **the `sql.DB` object is designed to be long-lived.** Don’t `Open()` and `Close()` databases frequently. Instead, create **one** `sql.DB` object for each distinct datastore you need to access, and keep it until the program is done accessing that datastore.

# Retrieve data

There are several idiomatic operations to retrieve results from the datastore.

1.  **Execute a query** that returns rows.
2.  **Prepare a statement** for repeated use, execute it multiple times, and destroy it.
3.  **Execute a statement in a once-off fashion**, without preparing it for repeated use.
4.  **Execute a query that returns a single row**. There is a shortcut for this special case.

```go
var ( id int name string ) 

// rows is the result set
rows, err := db.Query("select id, name from users where id = ?", 1) 
if err != nil {
	log.Fatal(err) 
}
// remember to close rows, it can be called even if already closed
//   eventhough if the result set is finished correctly, 
//   an error during the reading may leave it open
//   as long as the result set is not closed
//   the connection is blocked and can't execute another query
defer rows.Close()               
for rows.Next() { 
	// read a row to a set of vars
	//   internally it calls `strconv.ParseXxx()`
	err := rows.Scan(&id, &name) // read a row to variables
	if err != nil { 
		log.Fatal(err) 
	} 
	log.Println(id, name) 
} 

// always check rows.Error as the end of the above for loop
err = rows.Err() 
if err != nil { 
	log.Fatal(err) 
}
```

# Preparing
see [[sql prepare]].

> At the database level (what is running on the db server), a prepared statement is bound to a single database connection. **In Go, however, You prepare it on a `DB` or a `Tx`.**
> Here’s how it works:
> 1.  When you prepare a statement, it’s prepared on a connection in the pool.
> 2.  The `Stmt` object remembers which connection was used.
> 3.  When you execute the `Stmt`, it tries to use the connection. If it’s not available because it’s closed or busy doing something else, it gets another connection from the pool _and re-prepares the statement with the database on another connection._

```go
stmt, err := db.Prepare("select id, name from users where id = ?")
if err != nil {
	log.Fatal(err) 
} 
// remember to close the stmt as 
//   prepare is done on the db sql engine
defer stmt.Close() 
rows, err := stmt.Query(1) 
if err != nil { 
	log.Fatal(err) 
} 
defer rows.Close() 
for rows.Next() { 
	// ... 
} 
if err = rows.Err(); err != nil {
	log.Fatal(err) 
}
```

Use `stmt.Exec()`, preferably with a prepared statement, to accomplish an `INSERT`, `UPDATE`, `DELETE`, or another statement that doesn’t return rows.

```go
stmt, err := db.Prepare("INSERT INTO users(name) VALUES(?)") 
if err != nil { 
	log.Fatal(err) 
}
defer stmt.Close()
res, err := stmt.Exec("Dolly") 
if err != nil { 
	log.Fatal(err) 
} 
lastId, err := res.LastInsertId() 
if err != nil { 
	log.Fatal(err) 
} 
rowCnt, err := res.RowsAffected() 
if err != nil {
	log.Fatal(err)
}
log.Printf("ID = %d, affected = %d\n", lastId, rowCnt)
```

>  The `Query()` will return a `sql.Rows`, which reserves a database connection until the `sql.Rows` is closed. Since there might be unread data (e.g. more data rows), the connection can not be used. In the example above, the connection will _never_ be released again. The garbage collector will eventually close the underlying `net.Conn` for you, but this might take a long time.

# TNX

> Prepared statements that are created in a transaction are **bound exclusively to that transaction**. See [prepared statements](http://go-database-sql.org/prepared.html) for more.


You should not mingle the use of transaction-related functions such as `Begin()` and `Commit()` with SQL statements such as `BEGIN` and `COMMIT` in your SQL code. Bad things might result:

-   The `Tx` objects **could remain open**, reserving a connection from the pool and not returning it.
-   The state of the database could get **out of sync** with the state of the Go variables representing it.
-   You could believe you’re executing queries on a single connection, inside of a transaction, when **in reality Go has created several connections for you invisibly and some statements aren’t part of the transaction**.

While you are working inside a transaction you should be careful not to make calls to the `db` variable. Make all of your calls to the `Tx` variable that you created with `db.Begin()`. If you make further calls to `db.Exec()` or similar, those will **happen outside the scope of your transaction**, on other connections.