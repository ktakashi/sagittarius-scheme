[§2] (sagittarius object) - Convenient refs and coercion procedures {#lib.sagittarius.object}
-------------

###### [!Library] `(sagittarius object)` 

This library provides convenient procedures.


###### [!Generic] `ref`  _object_ _key_ _args_ _..._
###### [!Generic] `(setter ref)`  _object_ _key_ _value_

Returns given _object_'s values associated with _key_. The default
implementation uses `slot-ref`.

Following classes are specialised by default.

- `<hashtable>` uses `hashtable-ref`
- `<list>` uses `list-ref`
- `<string>` uses `string-ref`
- `<vector>` uses `vector-ref`



###### [!Generic] `->string`  _object_

Returns string represented _object_.

###### [!Generic] `->integer`  _object_

Returns integer represented _object_.

###### [!Generic] `->number`  _object_

Returns number represented _object_.

The default value is 0.

If the given _object_ is number, it returns given object itself. 

If the given _object_ is string, it uses `string->number` as a
conversion procedure.

If the given _object_ is character, it uses `char->integer` as a
conversion procedure.


