#rpc #ipc #api 



![gRPC Architecture](https://grpc.io/img/landing-2.svg)

# Protocol buffers


![[Pasted image 20221118182007.png]]


```proto
syntax = "proto3"   // specifies the protobuf version

import "myproject/other_protos.proto"; // import 


message SearchRequest {  
  singular string query = 1;   // a string type field with field number 1 
  int32 page_number = 2;       // by default the field quantifier is `singlar` 
  // use `repeated` for array-like fields
  // field can be structural typed
  repeated Result results = 3; 
}
  

message Result { /* ... */ }

enum Corpus {
  // every enum must have 0 as an entry, which is the default one
  // zero value needs to the 1st entry
  UNSPECIFIED = 0;  
  UNIVERSAL = 1;
  WEB = 2;
  // this is an alias
  // need to set compiler option `allow_alas=true`
  WWW = 2; 
  //...
}

// service defines the interface
service SearchService {
  rpc Search(SearchRequest) returns (SearchResponse);
  // ...
}


```

- use `repeated` to create an array-like field
- use `map<key_type, value_type> field_name` to specify a map-like field
- use a `package package_name` statement adds a namespace 
- `CamelCase` for service and RPC, `snake_case` for field

```bash
protoc --proto_path=IMPORT_PATH \ # directory to resolve proto import statement
       --cpp_out=DST_DIR \        # 
       --java_out=DST_DIR \
       --kotlin_out=DST_DIR \
       --python_out=DST_DIR \
       --go_out=DST_DIR \
       --ruby_out=DST_DIR \
       --objc_out=DST_DIR \
       --csharp_out=DST_DIR \
       path/to/file1.proto path/to/file2.proto
```

As an extra convenience, if the `DST_DIR` ends in `.zip` or `.jar`, the compiler will write the output to a single ZIP-format archive file with the given name. `.jar` outputs will also be given a manifest file as required by the Java JAR specification.


