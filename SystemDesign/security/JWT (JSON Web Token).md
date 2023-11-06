
# Why JWT 

Before JWT, the web uses _session based authentication_ which is powered by _cookie_. 
![[Session-Cookie Based Authentication.png]]
The problem is _cookie_, does not exists in all platforms (clients), not in native apps.

# JWT 
![[Pasted image 20231010165322.png]]

> The token:
>   `header . payload . signature`

1. `header` specifies how to decode the token
2. `payload` is some data transferred to the server/client
3. `signature` used to verified the sender and the payload

```js 
// the JWT RFC requires the uncoded header to be
let header = { "typ": "JWT",  "alg": "HS256"};
let payload = { 
  // claims specified by RFC 
  "iss" : "apple.com",    // who issued the token
  "iat" : 1570238918,     // when the token is issued 
  "exp" : 1570238992,     // expire time of the token
  // extra fields, any info you want to exchange 
  "userId": "abcd12345ghijk",
  "username": "bezkoder",
  "email": "contact@bezkoder.com",
};

const data = Base64UrlEncode(header) + '.' + Base64UrlEncode(payload);
const hashedData = Hash(data, secret);
const signature = Base64UrlEncode(hashedData);
```


# Claims 

Claims are statements about an entity (typically, the user) and additional data. 

There are three types of claims: _registered_, _public_, and _private_ claims.

1. _Registered Claims_, claims that are not mandatory but recommended 
  1. `iss`: issuer 
  2. `exp`: expiration time
  3. `sub`: subject 
  4. `aud`: audience 






