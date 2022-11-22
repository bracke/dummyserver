# DummyServer - ds
[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/dummyserver.json)](https://alire.ada.dev/crates/dummyserver.html)

DummyServer is a terminal program that serves dummy content (resources).
These resources are defined in a single JSON configuration and configuration is thus very simple and fast.
The prime purpose of DummyServer is to serve content to test client applications.

## Configure ds

The configuration consists of an array of resource definitions, only one level deep.

```json
[
   {
      "name" : "index.html",
      "content" : "<html><body><h1>Hello, world</h1></body></html>"
   },
   {
      "name" : "/counter.json",
      "content" : "{\"counter\": 23}"
   }
]
```

Each valid resource contains a name of the resource and the content to be served. On a normal server the name would correspond with a file and the server would serve the contents of that file as a response. DummyServer does not serve files, it only serves the content defined in the configuration file. The content needs to be a string, but could be JSON as a string or even binary data as a string.

## Start ds

DummyServer is a terminal program and is started in the terminal with the path to a configuration file.

```sh
ds config.json
Serving on http://localhost:8080
```

The default port used is 8080, but a different port can be specified.

```sh
ds -p 4200 config.json
Serving on http://localhost:4200
```

