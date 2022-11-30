# DummyServer - ds

[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/dummyserver.json)](https://alire.ada.dev/crates/dummyserver.html)

DummyServer is a terminal program that serves dummy content (resources).
These resources are defined in a single JSON configuration and configuration is thus very simple and fast.
The prime purpose of DummyServer is to serve content to test client applications where each configuration file represents a testcase or testsuite.
By starting multiple instances of ds with different ports, it is possible to serve multiple APIs or multiple testsuites simultaneously.

## Downloading and Building From Source

The easiest way to build DummyServer is using the Alire packet manager.

1. Download Alire from <https://alire.ada.dev>
2. Run "alr get dummyserver" in the terminal / console.
3. Change into the dummyserver folder that has been created by Alire.
4. Run "alr build" in the terminal / console.

You  may be prompted for selecting a toolchain - just accept the default.

After a successfull build, the DummyServer executable "ds" will be in the subfolder "bin". Copy ds into a folder that is in the execution path.

```sh
cp bin/ds /usr/local/bin
```

## Start ds

DummyServer is a terminal program and is started in the terminal with the path to a configuration file as an argument.

```sh
ds serve config.json

DummyServer (ds) Version 1.1.0
ⓘ Serving on <http://localhost:8080>

```

The default port used is 8080, but a different port can be specified.

```sh
ds serve -p 4200 config.json

DummyServer (ds) Version 1.1.0
ⓘ Serving on <http://localhost:4200>

```

## Configure ds

A vaild DummyServer configuration consists of an array of resource definitions, only one level deep.

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

Each valid resource contains the name of the resource and the content to be served. On a normal server the name would correspond with a file and the server would serve the contents of that file as a response. DummyServer does not serve files, it only serves the content defined in the configuration file. The content needs to be a string, but could be JSON as a string or even binary data as a string.

The content-type is optional and will default to "text/plain;charset=iso-8859-1".
All resourcenames should start with a "/".

The *Examples* folder contains several examples of valid configuration files:

1. *simple.json* Simple examples.
2. *qrcode.json* HTML resource with image that is a binary resource.
3. *counter.json* HTML and javascript resource which displays a counter value which is fetched as json resource.
4. *jsonapi.json* HTML and javascript resource which runs a single page app (SPA) which fetches a JSON:API resource.
5. *restapi.json* HTML and javascript resource which runs a single page app (SPA) which fetches a REST resource.

## Builtin Tools

DummyServer contains two tools to help you create content suitable for a configuration file.

### Base64

This tool converts a binary file into a string using Base64 encoding.

```sh
ds base64 qrcode.png

iVBORw0KGgoAAAANSUhEUgAAASwAAAEsAgMAAAAEE2bmAAAADFBMVEX////q6uoSEhIAAABMejGuAAABcElEQVR42u3asWoDMRBFUfUp9P9/6SJ9EhADlyexIzbtfeB4tZKOm2FknB2GMcYYY4wxXz/MZ4wx//5W1miumX2tllZrfeO69s3TJ8TaO0tLa64da75GrNWaz7X3lpYWa7VUdsf/WlpaqM8S684rS0sL1zRmfmt7cW5raSGwTq9cm9HSukw47d49WlrR8eK67nCMV29padW5ely3hPUOu7e0tNLbK3Vs/sScVm9pofp4J0bZKZea0dJqKjVq8yxFtLQepQkjumEFY3xWa2lp5W6+o373Ha2lpbVXYsof3ovPi2hptWftCgyu2gUtrWcJ607i4TePe0tLiydoeqnDvbe0tNDr8pyNcEVvaWmtmb3f8TrNXdLS6jOpULr/Tqel1T7rs3fE/rkMLa3M/vxhSvTHo6el1T7Ln3uyN763tLRiHXtgXSFvLC2t8Epr/hOgpdWdtfGbR42xH1XbW1paDHsehdmes1paEWOMMcYYY34Bv45zZrXZJSoAAAAASUVORK5CYII=
```

### Stringify

Converts the contents of a text file into a single line string.

```sh
ds stringify Examples/simple.json -w

[{\"name\":\"/index.html\",\"content\":\"<html><body><h1>Hello,world</h1></body></html>\",\"content-type\":\"text/html\"},{\"name\":\"/counter.json\",\"content\":\"{\"counter\":23}\",\"content-type\":\"application/json\"}]
```

## Troubleshooting

1. Always validate the config file. Use an online validation tool for this. Any search in any search engine for "json validator" will give you a bunch of tools for that.

2. Use the builtin tool *stringify* to convert "structured" text into a single line string for the config file.

3. Use the builtin tool *base64* for converting binary files into a string for the config file.

## Errors

### Exception Before Serving

If DummyServer throws an exception before the server is started (no "serving on localhost" message), then the problem likely occured while loading the configuration file and likely is caused by a problem in the configuration file. The solution is to validate and fix the config file (1.).

### No exception, but the Response is Incorrect

This likely also points to a problem with the configuration file. Maybe a reesource has the wrong name or two respurces have the same name. Also remember all resource names should start with a "/".

If you get the right content response, but the content is displayed wrong, then the content-type is likely wrong. This could also be a problem with other resources, in particular css files.

### Exceptions After the Server has Started

This points to a problem in the code and is likely my fault. Please consider creating an issue report on github. Have a look at the help topic *issues*.

```sh
ds help issues
```

## Issues

Think you've found a bug or have a new feature to suggest? Let me know!

Please create an issue on the GitHub page.

The issue should contain:

- A description.
- Any error message displayed.
- The output in the terminal/console.
- How ds was started - the full command.
- The configuration file used.
- The url used in the browser.

Providing these details will help enormously with finding the cause of the problems and thus make it much more likely that a fix can be found.

## Contributing

Please consider contributing to this project. Not just for code, but also for adding to the documentation or fixing spelling mistakes.

You can contribute to this project by creating pull request with your changes. You can use Git or GitHub for that. For small changes, the edit function in GitHub is probably the easiest option:

- View the project on GitHub.
- Select the "Code" tab.
- Find the file you want to edit.
- Click on the pen icon.
- Do the changes You want.
- Fill in the form in the bottom and click on "Propose changes".
