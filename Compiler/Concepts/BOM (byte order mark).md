[Byte order mark - Wikipedia](https://en.wikipedia.org/wiki/Byte_order_mark)

The byte order mark (BOM) is a particular usage of the special Unicode character,  BYTE ORDER MARK, whose appearance as a magic number at the start of a text stream can signal several things to a program reading the text:

1. The byte order, or endianness([[little edian]] / [[big endian]]), of the text stream in the cases of 16-bit and 32-bit encodings;
2. The fact that the text stream's encoding is Unicode, to a high level of confidence;
3. Which Unicode character encoding is used.



| Bytes       | Encoding Form         |
| ----------- | --------------------- |
| EF BB BF    | UTF-8                 |
| FF FE       | UTF-16, little-endian |
| FE FF       | UTF-16, big-endian    |
| FF FE 00 00 | UTF-32, little-endian |
| 00 00 FE FF | UTF-32, big-endian    |


[FAQ - UTF-8, UTF-16, UTF-32 & BOM (unicode.org)](http://unicode.org/faq/utf_bom.html#bom5)

> UTF-8 can contain a BOM. However, it makes _no_ difference as to the endianness of the byte stream. UTF-8 always has the same byte order.
