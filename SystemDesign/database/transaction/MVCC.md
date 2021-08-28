# MVCC in PostgresSQL

	from book: PostgreSQL_10_High_Performance_Expert_Techniques/Chapter 7

1. (*version*) As transactions are created in the database, PostgreSQL advances a **transaction ID counter**, usually just called the `XID`, to keep track of them.
2. (*visibility*) row created by that operation saves the session's transaction ID into a field named the `insertion XID`, also referred to as the `xmin`: 
	> As rows are considered for inclusion in the query results, if they are committed and their insertion `xmin` is less than current TXN session's `XID`, they show up in the query.

		PRO: Reading never blocks writing, and writing never blocks reading.

### Transaction isolation level

#### Read Committed mode
In what it calls **Read Committed mode**, the default, having another row get changed underneath a session isn't a disaster. What the server will now do is start over the original work it was planning to do with the new copy of the row.

If an UPDATE, DELETE, or lock for a row that's already locked is needed, the session waits for that to clear.

> Read Committed gets *a new snapshot of database activity it might want to pay attention to* at the beginning of every statement.


#### Serialization

Read Committed has the problem of [[non-repeatable read]] and [[phantom read]]

> And your application has no choice here but to rollback the entire transaction it was in the middle of and start over again. 
> This level of isolation is normally required if you have a transaction that executes multiple statements that absolutely must operate on an identical view of the database.

>  Read Committed lets the visibility snapshot slip forward for things like updates to commonly touched rows as each statement is processed. 
>  A serialized session aims to keep a consistent view for the whole transaction, and if something happens to make that possible it throws an error instead of risking a problem.