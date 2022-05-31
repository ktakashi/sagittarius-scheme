[§2] (text csv) - Comma separated values parser library {#text.csv}
-------------

###### [!Library] `(text csv)` 

This library provides comma separated values parser and write procedures.
The implementation conforms RFC 4180.


### [§3] High level APIs

The high level APIs are implemented as object oriented style. So we have CSV
object and its accessors.

###### [!Function] `csv?`  _object_

Returns #t if _object_ is csv object, otherwise #f.

###### [!Generic] `csv-header`  _(csv_ _<csv>)_

Retrieves CSV header from given CSV object _csv_ if it has, otherwise
'().


###### [!Generic] `csv-records`  _(csv_ _<csv>)_

Retrieves CSV records from given CSV object _csv_ if it has, otherwise
'().


###### [!Generic] `csv-read`  _(port_ _<port>)_ _._ _option_
###### [!Generic] `csv-read`  _(string_ _<string>)_ _._ _option_

Reads CSV data from given _port_ and returns csv object.

If the second form is called, the procedure opens string input port from given
_string_ and passes it to the first form.

The _option_ will be passed to `csv->list` described below.


###### [!Generic] `csv-write`  _(csv_ _<csv>)_ _:optional_ _(out_ _(current-output-port))_

Writes given CSV object _csv_ to the output port _port_.

### [§3] Middle level APIs

###### [!Function] `csv->list`  _port_ _:optional_ _(first-line-is-header?_ _#f)_

Reads CSV data from given input port _port_ and returns alist
representing CSV data.

If optional argument _first-line-is-header?_ is #t, then the procedure reads
the first line as header line.

The returning alist is following format;

``````````scheme
alist  := (header{0,1} record*)
header := (:header value*)
record := (:record value*)
value  := string
``````````

Note: the value returning from `csv-records` or `csv-header` do not
have meta values `:record` and `:header`.


### [§3] Low level APIs

###### [!Class] `<csv>` 

The class representing CSV. If you need to create empty CSV object, you
can do like following code;

``(make <csv>)``

.

Make sure, you import `(clos user)` library.


###### [!Generic] `add-record!`  _(csv_ _<csv>)_ _(list_ _<list>)_

Adds a CSV representing record _list_ to given CSV object _csv_.

The record must have `:record` keyword in the first element of the list.

###### [!Generic] `set-header!`  _(csv_ _<csv>)_ _(list_ _<list>)_
###### [!Generic] `set-header!`  _(csv_ _<csv>)_ _(string_ _<string>)_

Sets a CSV header _list_ to given CSV object _csv_.

If the second form is called, then first parse the string to CSV header and
calls the first form with parsed CSV header.

The _list_ must have `:header` keyword in the first element of the
list.

###### [!Generic] `add-records!`  _(csv_ _<csv>)_ _(list_ _<list>)_
###### [!Generic] `add-records!`  _(csv_ _<csv>)_ _(string_ _<string>)_

Convenient procedures.

Adds given CSV records _list_ to given CSV object _csv_.

If the second form is called, then first parse the given string to CSV
representing list and call the first form.


