<p align="center">
  <img alt="verb" src="https://github.com/federicotdn/verb/raw/master/docs/img/logo.png" width="50%">
  <br/>
</p>

**Verb** is a package for Emacs which allows you to organize and send HTTP requests.

The package introduces a new major mode, **Verb mode**, which is based on [Outline mode](https://www.gnu.org/software/emacs/manual/html_node/emacs/Outline-Mode.html). The core idea is to organize specifications for HTTP requests in a tree structure. Properties defined in the higher levels extend or sometimes override properties defined in the lower levels - this way, it is easy to define many HTTP request specifications without having to repeat common components as URL hosts, authentication headers, ports, etc. Verb tries to combine the usefulness of Outline mode with the common functionality provided by other HTTP clients.

Verb has been tested on Emacs 26 and 27.

[![Build Status](https://travis-ci.org/federicotdn/verb.svg?branch=master)](https://travis-ci.org/federicotdn/verb)
[![MELPA](https://melpa.org/packages/verb-badge.svg)](https://melpa.org/#/verb)
![License](https://img.shields.io/github/license/federicotdn/verb.svg)

## Features

- Send requests from Emacs using HTTP and HTTPS.
- Organize request specifications using Outline mode (the precursor to [Org mode](https://orgmode.org/)).
- Easily define common attributes (URLs, query string, headers, etc.) for many requests.
- Correctly handle text encodings (charsets) for requests and responses.
- View PDF, PNG, JPEG, BMP, GIF and SVG responses inside Emacs.
- Evaluate and substitute Emacs Lisp expressions in specifications text.
- Supports uploading files on requests.
- Optionally uses `url-queue.el` backend.
- Easy to use! (hopefully).

## Installation
You can install Verb by using the `package-install` command (make sure [MELPA](https://melpa.org/) is included in your package sources):

<kbd>M-x</kbd> `package-install` <kbd>RET</kbd> `verb` <kbd>RET</kbd>

If you're using [use-package](https://github.com/jwiegley/use-package), you can add the following to your `init.el`:

```elisp
(use-package verb
  :mode ("\\.verb\\'" . verb-mode))
```

Alternatively, you can just add [`verb.el`](verb.el) to your `load-path` instead of installing it as a package.

## Quick Start

Here's a quick example in case you want to get started without reading the [Usage Guide](#usage-guide). Place the following on an `example.verb` file:
```
* Quick Start for Verb
# Comments start with '#' (must be placed right
# after headings, which start with '*')

# Verb file for https://docs.postman-echo.com/
template https://postman-echo.com
Accept: application/json

** POST some contents
# Because the base URL is defined in the parent
# heading, there's no need to repeat it here.
# We can also add more headers.
post /post
Content-Type: application/json

{
    "foo": "bar"
}

** GET and respond with status code
# Send a GET request and get a specific status code
# as a response.
# Use embedded Lisp code in the reqeuest spec.
get /status/{{(read-number "Status: " 200)}}
```

Then, move the point to one of the level 2 heading (marked with `**`), and press <kbd>C-c C-r C-r</kbd> to send an HTTP request.

## Screenshots

![Screenshot from 2020-01-11 21-52-28](https://user-images.githubusercontent.com/6868935/72210619-5b650880-34be-11ea-8916-f03d151ccc9e.png)

![Screenshot from 2020-01-11 21-55-10](https://user-images.githubusercontent.com/6868935/72210620-5bfd9f00-34be-11ea-9f4a-6e3339a84d40.png)

## Usage Guide

After installing Verb, get started by creating a new `guide.verb` file. The `.verb` extension is added to the `auto-mode-alist` as part of the package autoloads, so the `verb-mode` major mode will be activated automatically. In the example file, add the following contents:

```
* Get a user
get https://reqres.in/api/users
```

This defines a minimal HTTP request specification, describing a method (`GET`) and a URL (`https://reqres.in/api/users`). The request is contained under a heading marked with only one `*`, which makes it a level 1 heading. The number of `*`s determines a heading's level.

### Sending Requests

To actually send the HTTP request, use the `verb-send-request-on-point-other-window` command, which by default is bound to <kbd>C-c C-r C-r</kbd>. This command will send the HTTP request, and show the response on another window using `switch-to-buffer-other-window`. If you wish to view the response on the same window, use the `verb-send-request-on-point` command, by default bound to <kbd>C-c C-r C-f</kbd>.

### The Response Body Buffer

After you have sent the request and the server has answered back successfully, you should now be seeing the response body buffer. The response body buffer always has the `verb-response-body-mode` minor mode activated (indicated by `Verb[Body]` in the modeline). 

The buffer will have an active [header line](https://www.gnu.org/software/emacs/manual/html_node/elisp/Header-Lines.html), showing something similar to:

```
HTTP/1.1 200 OK | 0.754s | application/json | 1020 bytes
```

This text indicates the status of the HTTP response, the time in seconds it took for it to be completed, the type of the contents received (or `-` if the content type is unknown), and the number of bytes in the response body (read from the `Content-Length` header, when possible, otherwise from the local buffer size).

The contents of the response body will be shown on the buffer. To choose how they will be actually shown, the following steps are followed:

1. The content type is extracted from the `Content-Type` header. If the header is not present, the content type is defined as `nil`.
2. A content handler is chosen for this content type. There are two types of handlers: handlers for text content types (such as JSON, XML, etc.) and handlers for binary content types (such as PNG, PDF, etc.). If a binary content type handler for this type is available, pick that one. Otherwise, if a text content type handler for this type is available, choose that one. If no handler matched the content type (or if the content type is `nil`), choose `fundamental-mode` by default (as a text content type handler).
3. **Text:** If the chosen handler is for text, decode the response body using the charset described in the `Content-Type` header. If no charset was specified, use the one specified by `verb-default-response-charset` (default: `utf-8`). After that is done, call the handler (e.g. `xml-mode`). **Binary:** If the chosen handler is for a binary type, call the handler directly after loading the raw bytes into the buffer (e.g. `doc-view-mode`).
4. The handler will have set an appropiate major mode to display and/or edit the received content.

To close the response buffer, you can use the `verb-kill-response-buffer-and-window` command, which is bound by default to <kbd>C-c C-r C-k</kbd>. This command will also kill the associated response headers buffer (see next section).

### The Response Headers Buffer

If you wish to see the HTTP response headers, use the `verb-toggle-show-headers` command while the response body buffer is selected. By default, it is bound to <kbd>C-c C-r C-r</kbd>.

The response headers buffer will be opened on a new window. The new window will be generated by splitting the window displaying the response body buffer into two parts using `split-window`. The response headers buffer will have the `verb-response-headers-mode` major mode activated, indicated by `Verb[Headers]` in the modeline. The buffer will also have a header line showing the number of headers received.

The contents of the response headers buffer will be the actual HTTP headers received, for example:
```
Content-Encoding: gzip
Content-Type: application/json; charset=utf-8
Date: Thu, 02 Jan 2020 23:29:19 GMT
Server: nginx
Vary: Accept-Encoding
Content-Length: 619
Connection: keep-alive
```

To close the response headers buffer, use the `verb-toggle-show-headers` command again (<kbd>C-c C-r C-r</kbd>) while the response body buffer is selected.

### Specifying HTTP Headers

You can add headers to your request specifications. To do this, simply write them below the request method and URL. Following from our first example:
```
* Get a user
get https://reqres.in/api/users
Accept: application/json
Content-Language: de-DE
```

All headers must be written immediately after the method + URL line, without any blank lines in between.

**Note:** "header" != "heading", "header" is used to refer to HTTP headers, and "heading" is used to refer to the elements used to separate sections of text.

### Adding a Body

To add a body to your HTTP request, simply insert it below the method, URL and headers. Continuing with our previous example, add the following contents at the end of the file:

```
* Create a user
post https://reqres.in/api/users
Accept: application/json
Content-Type: application/json; charset=utf-8

{
    "name": "John",
    "age": 42
}
```

The headers and body may be separated by a blank line, which will not be included in the request. If no blank line is present, the request body is considered to start from the first line not matching a `<Header>: <Value>` format.

To encode the request body, Verb will use the `charset` value defined in the `Content-Type` header. If the header is present but `charset` is not defined, the charset `verb-default-request-charset` will be used (default: `utf-8`) and added to the header value. If the header is not present, the charset `verb-default-request-charset` will be used, but no `Content-Type` header wil be sent.

### Extend and Override Requests

Our example file should now look like the following:

```
* Get a user
get https://reqres.in/api/users
Accept: application/json
Content-Language: de-DE

* Create a user
post https://reqres.in/api/users
Accept: application/json
Content-Type: application/json

{
    "name": "John",
    "age": 42
}
```

Notice that the two request specifications share many things in common: the URL host, path and one header. In order to avoid repeating all this information, we can actually define a `template` request, establishing all the common attributes among requests, and then extend this template request with different values. To to this, let's create a new level 1 heading, and move the already existing headings below it, making them level 2 headings:

```
* User management
template https://reqres.in/api/users
Accept: application/json

** Get a user
get
Content-Language: de-DE

** Create a user
post
Content-Type: application/json

{
    "name": "John",
    "age": 42
}
```

Now, when we send the request under "Get a user", Verb will collect all the properties defined in the parent headings (in this case, a URL and one header), and then extend/override them with the attributes under this specific heading. This is how each attribute of an HTTP request specification is extended/overridden:

- **Method:** The last heading's (i.e. the one with the highest level) method will be used. The value `template` does not count as a method and will be ignored.
- **URL:**
  - **Schema**: The last defined heading's URL schema will be used (`http` or `https`).
  - **Host**: The last defined heading's URL host will be used.
  - **Port**: The last defined heading's URL port will be used.
  - **Path**: All paths will be concatenated, starting from the first heading (i.e. the one with the lowest level).
  - **Query**: Query string arguments will be merged. Values from higher level headings take priority.
  - **Fragment**: The last defined heading's URL fragment will be used.
- **Headers:**: All headers will be merged. Values from higher level headings take priority.
- **Body**: The last request body present in a heading will be used (if no heading defines a body, none will be used).

You can create hierarchies with any number of headings, with many levels of nesting. A good idea is to create a single `.verb` file to describe, for example, a single HTTP API. This file will contain a level 1 heading defining some common attributes, such as the URL schema, host and root path, along with an `Authentication` header. The level 2 headings will specify different resources, and the level 3 headings will specify actions to run on those resources. For example:

```
* Foobar Blog API
template https://foobar-blog-api.org/api/v1
Authentication: username=john&password=foobar
Accept: application/json

** Users
template /users

*** Create a user
post
Content-Type: application/json

{
    "name": "John",
    "posts": []
}

*** Search users
get ?name=John

*** Delete all users
delete

** Posts
template /posts?lang=en

*** Search posts
get ?text=example
```

### Emacs Lisp Code Tags

You can embed Lisp code inside request specifications. When sending the request, Verb will evaluate all code tags, and replace them with the results of the evaluations. Code tags may appear anywhere on the request specification: the URL, headers and body. By default, code tags are delimited with `{{` and `}}` (see the customizable variable `verb-code-tag-delimiters`).

Depending on the type of the resulting value for a code tag, Verb will do the following:
- `string`: The value will be inserted as-is into the request contents.
- `buffer`: The buffer's contents will be inserted into the request using `insert-buffer-substring`. If the buffer's `verb-kill-this-buffer` variable is set to non-nil, the buffer will be killed after its contents have been read. The variable's default value is `nil`.
- Other types: The value will be converted to a string using `(format "%s" result)` and inserted into the request contents.

Here's an example that uses code tags:

```
post https://some-example-api.com/api/users
Authentication: {{(verb-var token)}}

{
    "username": "{{(user-full-name)}}",
    "operating_system": "{{system-type}}"
}
```

The example uses the `verb-var` function. This function returns the value of the symbol being passed to it, unless the symbol does not have a value, in which case its value is set using `read-string`.

### File Uploads

To upload a file, you can use the included `verb-read-file` function. This function reads a file into a buffer and sets its `verb-kill-this-buffer` variable to `t`, and then returns the buffer. Use it from inside code tags to insert the contents of a local file in a request. Here's an example:

```
post https://some-example-api.com/api/upload
Content-Type: text/plain

{{(verb-read-file "documents/myfile.txt")}}
```

Remember to specify `Content-Type` in your HTTP headers, as Verb won't do this for you. This will let the server know how to interpret the contents of the request.

### Body Lines starting with `*`

You may have noticed that because headings start with `*`, you cannot include lines starting with `*` in your request bodies, because Verb will interpret them as a new heading. To get around this, you can prefix request body lines starting with `*` with an empty code tag, `{{}}`. The empty code tag will evaluate to the empty string, so it won't modify the content of your request body. Here's an example of this:

```
post https://some-example-api.com/api/upload
Content-Type: text/markdown

# Sample Markdown file

{{}}**This text is bold.**
{{}}*This text is italicized.*
```

### Outline Mode Commands

Because Verb mode is based on Outline mode, all the commands available in Outline mode are available in Verb mode as well. Some examples:

- <kbd>C-c C-a</kbd>: `outline-show-all`
- <kbd>C-c C-p</kbd>: `outline-previous-visible-heading`
- <kbd>C-c C-n</kbd>: `outline-next-visible-heading`
- <kbd>C-c RET</kbd>: `outline-insert-heading`

(Use <kbd>C-h f</kbd> `outline-mode` <kbd>RET</kbd> for the full commands list)

Additionally, Verb adds the following commands:
- <kbd>TAB</kbd>: `verb-cycle` Imitates the `org-cycle` command found in Org mode.
- <kbd>C-RET</kbd>: `verb-insert-heading` Inserts below a new heading with the same level as the one on point.

### Customization

To see which aspects of Verb may be customized, use <kbd>M-x</kbd> `customize-group` <kbd>RET</kbd> `verb` <kbd>RET</kbd>.

### Export Requests

You can export request specifications to other formats by using the `verb-export-request-on-point` command, by default bound to <kbd>C-c C-r C-e</kbd>.

The available export functions are:
- `human`: Display the request specification in a more human-friendly way.
- `verb`: Display the request specification in the same format Verb uses. This is still useful as the request displayed will be the one generated by combining the properties of the parent headings as well.

## API and Hooks

These are the hooks currently available for use in Verb:
- `verb-mode-hook`
- `verb-response-body-mode-hook`
- `verb-response-headers-mode-hook`

All variables, functions and classes starting with `verb-` but not starting with `verb--` are part of the package's public API.

There are two [EIEIO](https://www.gnu.org/software/emacs/manual/html_node/eieio/) classes which are very central to Verb's internal and external design:
- `verb-request-spec` represents an HTTP request specification. It includes all the slots that have been mentioned previously: `method`, `url`, `headers` and `body`.
- `verb-response` represents an HTTP response. Its slots include: `request`, which points back to the `verb-request-spec` instance that requested this response, an alist of HTTP headers `headers`, the numerical status code `status`, the time it took in seconds to receive the response `duration`, and the number of bytes in the response body `body-bytes`.

## Examples

The [`docs/`](docs) directory contains various `.verb` files which showcase different features of the package.

## Contributing

PRs and suggestions are welcome. Ideally, new features and functions should include tests, see file `test/verb-test.el`. To run the tests locally, you will need to have a Python 3.6+ interpreter installed, and then run the following command (needed only once):
```bash
$ make setup-tests
```

Then, you can run the tests:
```bash
$ make test
```

You can also check for byte-compilation warnings and documentation/package issues. First, run (needed only once):
```bash
$ make setup-check
```

After that, run the checks:
```bash
$ make check
```

## Related Packages

- [restclient](https://github.com/pashky/restclient.el): Verb is an attempt to improve upon the core idea of the `restclient` package: writing request specifications on a buffer, and receiving the responses on another. The most important differences between the two packages are:
  - Verb uses a tree-like structure to organize request specifications, `restclient` uses a flat one.
  - Verb displays HTTP response headers on a separate buffer, `restclient` includes them commented out in the main response buffer.
  - Verb correctly handles URLs such as https://api.ipify.org?format=json (400 when using `restclient`, 200 when using Verb and `curl`).
  - Verb has only been tested on Emacs 26+, `restclient` was tested on those and older versions as well (which is important if you're using an older Emacs version).
  - In Verb, lines starting with `#` can be included in a request body (and `*` as well).
  - Verb does not support exporting requests to `curl`, `restclient` does.
  - Licensing (GPLv3 vs. Public domain).
- [http.el](https://github.com/emacs-pe/http.el): I have not tested this package, so I can't provide a comparison.

## License

Distributed under the GNU General Public License, version 3.

See [LICENSE](LICENSE) for more information.
