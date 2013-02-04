![alt text](https://raw.github.com/breakds/lazy-bone/master/logo/lazy-bone-small.png "lazy-bone") [Common Lisp Web Application Framework for lazy Programmers](https://github.com/breakds/lazy-bone)
======

## Introduction

Cassandra does not provide an official driver for common lisp, but there is one for python.
This package provides routines that send CQL queries to a python server via socket, and the python server will
execute the query with the python driver and send the result back to common lisp.



## Author

**BreakDS**

+ breakds@gmail.com
+ https://github.com/breakds


## Usage

### Step 1

Start the python server by calling the cqlServer.py

```bash
path-to-this-project/python/cqlServer.py
```

### Step 2

Load the asdf system via quicklisp. (Put the project folder under quicklisp-folder/local-projects or create a soft link 
under that folder before loading the system)

```lisp
(ql:quickload 'clq-socket)
```


### Step 3

Connect to a cassandra keyspace

```lisp
(cql-socket:cql-use-keyspace "keyspace-name" :host "localhost" :port 9160)
```

### Step 4

You can start to send queries now by

```lisp
(cql-socket:cql-query "select * from data-base-name")
```

## Lisence

This is free and unencumbered software released into the public domain.

Anyone is free to copy, modify, publish, use, compile, sell, or
distribute this software, either in source code form or as a compiled
binary, for any purpose, commercial or non-commercial, and by any
means.

In jurisdictions that recognize copyright laws, the author or authors
of this software dedicate any and all copyright interest in the
software to the public domain. We make this dedication for the benefit
of the public at large and to the detriment of our heirs and
successors. We intend this dedication to be an overt act of
relinquishment in perpetuity of all present and future rights to this
software under copyright law.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

For more information, please refer to <http://unlicense.org/>


