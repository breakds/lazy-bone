![alt text](https://raw.github.com/breakds/lazy-bone/master/logo/lazy-bone-small.png "lazy-bone") 
[Common Lisp Web Application Framework for the Lazy](https://github.com/breakds/lazy-bone)
======

## Introduction

Thanks to Backbone.js, development of a single-page web application is much simplified. However, the development process is still
tedious. This framework frees you from the tedious work by allowing you to write some simple lisp code and generating code
for both client-side and server-side. The features are
  * Generating javascript code dynamically, built upon Backbone.js for the client
  * Generating HTML dynamically with [html-template](http://weitz.de/html-template/)
  * With [parenscript](http://common-lisp.net/project/parenscript/), you can enjoy the power of lisp macros while programming the web application
  * Provide some continuation-based syntax, which is natural in web application development
  * Write your code for server and client **simultaneously**
  * After compiling your application, just run (your-app-name:start-server) to start your web application


## Author

**BreakDS**

+ breakds@gmail.com
+ https://github.com/breakds


## Example

An example is built on top of this library can be found [here](https://github.com/breakds/lazy-bone-example)


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


