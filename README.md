# DummyServer - ds
[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/dummyserver.json)](https://alire.ada.dev/crates/dummyserver.html)

DummyServer is a terminal program that serves dummy content (resources).
These resources are defined in a single JSON configuration and configuration is thus very simple and fast.
The prime purpose of DummyServer is to serve content to test client applications where each configuration file represents a testcase or testsuite.
By starting multiple instances of ds with different ports, it is possible to serve multiple APIs or multiple testsuites simultaneously.

## Configure ds

The configuration consists of an array of resource definitions, only one level deep.

```json
[
   {
      "name" : "/index.html",
      "content" : "<html><body><h1>Hello, world</h1></body></html>",
      "content-type": "text/html"
   },
   {
      "name" : "/counter.json",
      "content" : "{\"counter\": 23}",
      "content-type": "application/json"
   }
]
```

Each valid resource contains a name of the resource and the content to be served. On a normal server the name would correspond with a file and the server would serve the contents of that file as a response. DummyServer does not serve files, it only serves the content defined in the configuration file. The content needs to be a string, but could be JSON as a string or even binary data as a string.

The content-type is optional and will default to "text/plain;charset=iso-8859-1".

## Start ds

DummyServer is a terminal program and is started in the terminal with the path to a configuration file.

```sh
> ds serve config.json

DummyServer (ds) Version 1.1.0
ⓘ Serving on <http://localhost:8080>

```

The default port used is 8080, but a different port can be specified.

```sh
> ds serve -p 4200 config.json

DummyServer (ds) Version 1.1.0
ⓘ Serving on <http://localhost:4200>

```

## Tools

DummyServer contains two tools to help you create content suitable for a configuration file.

### Base64

This tool converts a binary file into a string using Base64 encoding.

```sh
> ds base64 qrcode.png
iVBORw0KGgoAAAANSUhEUgAAASwAAAEsCAYAAAB5fY51AAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6A...
```

### Stringify

Converts the contents of a text file into a single line string.

```sh
> ds stringify Examples/simple.json
[       {         \"name\" : \"/index.html\",         \"content\" : \"<html><body><h1>Hello, world</h1></body></html>\",         \"content-type\": \"text/html\"      },      {         \"name\" : \"/counter.json\",         \"content\" : \"{\\"counter\\": 23}\",         \"content-type\": \"application/json\"      }]
```
