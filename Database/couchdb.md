#database 
#NoSQL


[The Core API](https://docs.couchdb.org/en/stable/intro/api.html)


# Database Model

Features:
1. Document based NoSQL 
2. RESTful HTTP API for CRUD database documents
3. MVCC(Multi-Version Concurrency Control)
4. B-Tree for storage model

## Consistency Model

CouchDB  uses MVCC
1. each document has a field `_id` as its _unique id_; 
2. each document has a key `_rev`(short for revision), specifies the _version_ of the document.
3. _update_ needs to specifies the document version the update based on,  which must match the current version in the database, or else the update fails.

![[Pasted image 20220824200631.png]]

### Incremental Replication

_Incremental replication_ is a process where document changes are **periodically copied** between servers.
When merging, all versions are preserved, no matter confliction exists.
![[Pasted image 20220824200917.png]]


## Storage Model

- A B-tree backs up a  key sorted view 
    - the B-tree is constructed when the first query happens.
    - The B-tree is only constructed once, all subsequent queries uses the B-Tree.
    - doc CRUD will update the view's B-Tree's node correspondently
    - THIS MAKES A VIEW CAN BE USED AS AN INDEX

# Authentication Management

# Server admin
```toml
# config.init
[admins]
admin = password
```

```bash
curl -X PUT $HOST/_node/$NODE_NAME/_config/admins/$USER \
     -d '"password"'
```

# Users

```bash
curl -X PUT \
     http://localhost:5984/_users/org.couchdb.user:jan \
     -H "Accept: application/json" \
     -H "Content-Type: application/json" \
     -d '{"name": "jan", "password": "apple", "roles": [], "type": "user"}'
```

Database authorization rules assign a user into one of two classes
- _members_, who are allowed to read all documents and create and modify any document **except for** design documents.
- _admins_, who can read and write all types of documents, modify which users are members or admins, and set certain per-database configuration options.

```json
// schema of a `_security` document
{
  "admins": { 
    "names": [], 
    "roles": [] 
  }, 
  "members": { 
    "names": ["jan"], 
    "roles": [] 
  } 
}
```

# Special Databases/Endpoints

| database/endpoint              | meaning                                           |
| ------------------------------ | ------------------------------------------------- |
| `_users`                       | authentication database                           |
| `_config` (endpoint)           | database configuration                            |
| `_up` (endpoint)               | check database status                             |
| `/`                            | server info summary, CRUD databases               |
| `_all_dbs` (endpoint)          | a list of database names                          |
| `_utils` (endpoint)            | GUI                                               |
| `_uuid[?count=$num]`(endpoint) | let server returns `$num` UUIDs:  `{"uuids": []}` |
| `__replicate`                  | replicate action [Introduction to Replication](https://docs.couchdb.org/en/stable/replication/intro.html#replication-intro)                                 |



# Special Endpoints for Every Database 

| endpoint                          | meaning                       | docs                                                                                                 |
| --------------------------------- | ----------------------------- | ---------------------------------------------------------------------------------------------------- |
| `_all_docs`                       | list of all documents names   |                                                                                                      |
| `_changes`                        | changes _feed_                |                                                                                                      |
| `_security`                       | authorization of the database | [`_security` doc ref](https://docs.couchdb.org/en/stable/api/database/security.html#api-db-security) |
| `_design/$name`                   | design docs                   |                                                                                                      |
| `_design/$name/_view/$view_name$` |                               |                                                                                                      |


# Special Document Fields

| fields         | meaning                                                       |
| -------------- | ------------------------------------------------------------- |
| `_id`          | id of the doc, which can be accessed by `$HOST/$db_name/$_id` |
| `_rev`         | revision of the document                                      |
| `_attachments` | a list of [[#attachments]] to the doc                                                              |

## attachments


# Design Documents: Views

```json
{
    "_id": "_design/example",
    // define 2 views
    "views": {
        "view-number-one": {
            "map": "function (doc /* document */) { emit(key, value); }"
        },
        "view-number-two": {
            "map": "function (doc) { emit(key, value); }",
            "reduce": "function (keys, values, rereduce) {/* function code here - see below */}"
        }
    },
    "updates": {
        "updatefun1": "function(doc,req) {/* function code here - see below */}",
        "updatefun2": "function(doc,req) {/* function code here - see below */}"
    },
    "filters": {
        "filterfunction1": "function(doc, req){ /* function code here - see below */ }"
    },
    "validate_doc_update": "function(newDoc, oldDoc, userCtx, secObj) { /* function code here - see below */ }",
    "language": "javascript"
}
```

> when you have multiple views with the same map function in the same design document, CouchDB will optimize and only calculate that map function once.

Query results from view
```json
{
    "total_rows": 3,
    "offset": 0,
    "rows": [
        {
            // key of emit(key, value)
            "key": "2009/01/15 15:52:20",
            // document._id of the input doc  
            "id": "hello-world",
            // value of emit(key, value)
            "value": "Hello World"
        },
    ]
}
```

## How reduce function works

For a view with a reducer, the **map results** constructs a B-Tree:
![[Pasted image 20220824225233.png]]

CouchDB stores **both** keys and values inside each leaf node.

> CouchDB runs the reduce function on **all nodes** of the B-Tree.

**Re-reduce**  happens when the query range does not match a single node
![[Pasted image 20220824225931.png]]
In the above case re-reduce is called combining the 1st level reduce results of 3rd leaf node  and the 2nd parent node.


## View as Index

Since views are stored as B-Tree, it can be used as _indexes of the database_.

```bash
# WHERE index = index_value
$HOST/$DB/_design/$DESIGN/_view/$IDX_VIEW?key="$IDX_VALUE"

# WHERE index >= start AND index <= end
$HOST/$DB/_design/$DESIGN/_view/$IDX_VIEW?startkey="$start"&endkey="$end"
```




# Query

## Mango Query

## HTTP API


