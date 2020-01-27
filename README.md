<p align="center">
  <img alt="verb" src="https://github.com/federicotdn/verb/raw/master/docs/img/logo.png" width="50%">
  <br/>
</p>

**Verb** is a package for Emacs which allows you to organize and send HTTP requests.

The package introduces a new minor mode, **Verb mode**, which works as an extension to [Org mode](https://orgmode.org/). The core idea is to organize specifications for HTTP requests using Org's tree structure. Properties defined in the higher levels extend or sometimes override properties defined in the lower levels - this way, it is easy to define many HTTP request specifications without having to repeat common components as URL hosts, authentication headers, ports, etc. Verb tries to combine the usefulness of Org mode with the common functionality provided by other HTTP clients.

Verb has been tested on Emacs 26 and 27.

[![Build Status](https://travis-ci.org/federicotdn/verb.svg?branch=master)](https://travis-ci.org/federicotdn/verb)
[![MELPA](https://melpa.org/packages/verb-badge.svg)](https://melpa.org/#/verb)
[![MELPA Stable](https://stable.melpa.org/packages/verb-badge.svg)](https://stable.melpa.org/#/verb)
![License](https://img.shields.io/github/license/federicotdn/verb.svg)

## Features

- Send requests from Emacs using HTTP and HTTPS.
- Organize request specifications using Org mode.
- Easily define common attributes (URLs, query string, headers, etc.) for many requests.
- Correctly handle text encodings (charsets) for requests and responses.
- View PDF, PNG, JPEG, BMP, GIF and SVG responses inside Emacs.
- Evaluate and substitute Emacs Lisp expressions in specifications text.
- Can export requests to `curl` format.
- Integrates with Babel.
- Includes mouse support (menu bar and mode line)
- Supports uploading files on requests.
- Optionally uses `url-queue.el` backend.
- Easy to use! (hopefully).

## Installation
You can install Verb by using the `package-install` command (make sure either [MELPA](https://melpa.org/) or [MELPA Stable](https://stable.melpa.org/) are included in your package sources):

<kbd>M-x</kbd> `package-install` <kbd>RET</kbd> `verb` <kbd>RET</kbd>

Alternatively, you can just add [`verb.el`](verb.el) to your `load-path` instead of installing it as a package.

Once Verb has been installed and loaded, add the following to your `init.el`:
```elisp
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map))
```

If you're using [use-package](https://github.com/jwiegley/use-package), try this instead:
```elisp
(use-package verb
  :after org
  :config (define-key org-mode-map (kbd "C-c C-r") verb-command-map))
```

Both cases will set <kbd>C-c C-r</kbd> as the prefix key for all Verb commands in Org mode. Feel free to use another key if you prefer that.

## Quick Start

Here's a quick example in case you want to get started without reading the [Usage Guide](#usage-guide). Place the following on an `example.org` file:
```
#+FILETAGS: :verb:

* Quick Start for Verb
# Comments start with '#'. You can only place
# comments before the URL and in the headers.
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
# Use embedded Lisp code in the request spec.
get /status/{{(read-number "Status: " 200)}}
```

Then, move the point to one of the level 2 headings (marked with `**`), and press <kbd>C-c C-r C-r</kbd> to send the HTTP request.

## Screenshots

![Screenshot from 2020-01-11 21-52-28](https://user-images.githubusercontent.com/6868935/72210619-5b650880-34be-11ea-8916-f03d151ccc9e.png)

![Screenshot from 2020-01-11 21-55-10](https://user-images.githubusercontent.com/6868935/72210620-5bfd9f00-34be-11ea-9f4a-6e3339a84d40.png)

## Usage Guide

This guide assumes that you're using <kbd>C-c C-r</kbd> as the prefix key for all Verb commands, and that you're also getting started with Org mode.

### Writing Request Specifications
After setting up Verb, begin by creating a new `guide.org` file. In the example file, add the following contents:

```
* Get users list         :verb:
get https://reqres.in/api/users
```

This defines a minimal HTTP request specification under the "Get users list" heading, composed of a method (`GET`) and a URL (`https://reqres.in/api/users`). The heading is prefixed with only one `*`, which makes it a level 1 heading. The number of `*`s determines a heading's level. All the text under a heading corresponds to the HTTP request it is describing. It is not possible to write request specifications without adding a heading at the top.

Note that the heading has a `:verb:` tag. **Verb functions only process headings that contain this tag, and ignore the rest.** This allows you to create documents that may have a combination of HTTP request specifications and other information types. Note that in Org mode, by default subheadings inherit their parents' tags (see the `org-use-tag-inheritance` variable). To easily add the `:verb:` tag to all headings in an Org document, add the following at the top of your file:
```
#+FILETAGS: :verb:
```

You may tweak the text value of the tag used by modifying the `verb-tag` variable. Note that if you modify it, you'll need to update your files as well.

### Enabling Verb in Org Buffers
When you open an `.org` file with HTTP request specifications in it, Verb mode won't be enabled by default. To enable it, you can choose from these different options:
- Run one of the commands that enable Verb automatically (e.g. `verb-send-request-on-point-other-window-stay`). You may use the keybinding set up in your `init.el` file (i.e. <kbd>C-c C-r C-r</kbd>, see [Installation](#installation)).
- Run <kbd>M-x</kbd>`verb-mode`<kbd>RET</kbd>.
- Add a file-local variable at the bottom of your file:
```
# Local Variables:
# eval: (verb-mode)
# End:
```

In general, the first option should be useful enough for most cases. Once Verb mode has been enabled, `Verb` should appear on the modeline.

### Sending Requests

To actually send the HTTP request, use one of the `verb-send-request-on-point` commands. They are the following:
- <kbd>C-c C-r C-r</kbd>: `verb-send-request-on-point-other-window-stay` sends the request and shows the response on a buffer in another window, but doesn't switch to that window.
- <kbd>C-c C-r C-s</kbd>: `verb-send-request-on-point-other-window` sends the request, shows the response on a buffer in another window, and switches to it.
- <kbd>C-c C-r C-f</kbd>: `verb-send-request-on-point-other-window-stay` sends the request, and shows the response on a buffer in the currently selected window.

Request sending is asynchronous - you can do other stuff while Emacs waits for the server's response. If the response is taking too long to be received, a warning will be displayed in the minibuffer. You can modify this behaviour by modifying the `verb-show-timeout-warning` variable's value.

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

There's two recommended ways of closing response buffers:
- If the response buffer is the current buffer, you can use the `verb-kill-response-buffer-and-window` command, which is bound by default to <kbd>C-c C-r C-k</kbd>. This command will also kill the associated response headers buffer (see next section).
- If the response buffer is not the current buffer (e.g. you are still on your `guide.org` buffer), you can kill **all** response buffers by using the `verb-kill-all-response-buffers`, which is bound to <kbd>C-c C-r C-k</kbd> by default. Response headers buffers will also be killed automatically.

As you send more HTTP requests, more response buffers will be created, with `<N>` at the end of their name to distinguish between them. If you wish to automatically have old response buffers killed when making a new request, set the `verb-auto-kill-response-buffers` variable to `t`.

### Re-sending requests

If you wish to re-send the request that generated the current response buffer, select the window showing it and use the `verb-re-send-request` command, which is bound to <kbd>C-c C-r C-f</kbd> by default. Note that the exact same request will be sent, even if the originating `.org` file was modified.

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
* Get users list         :verb:
get https://reqres.in/api/users
Accept: application/json
Content-Language: de-DE
```

All headers must be written immediately after the method + URL line, without any blank lines in between. It is also possible to comment out headers. To do this, simply add `#` at the beginning of the line.

A certain set of headers will **always** be included in sent requests, even if they haven't been specified. Some of them are due to requirements of the HTTP standard, and others due to limitations of the `url` Emacs library. They are the following:
- `MIME-Version`: `1.0`
- `Connection`: `close` or `keep-alive`
- `Host`: *URL host*
- `Accept`: `*/*` (default value, but may be overwritten by the user)
- `Accept-Encoding`: `gzip`
- `Extension`: `Security/Digest Security/SSL`

If you include one of these headers in one of your requests (except `Accept`), Verb will add a warning to the [log](#verb-log).

**Note:** "header" != "heading", "header" is used to refer to HTTP headers, and "heading" is used to refer to the elements used to separate sections of text.

### Adding a Body

To add a body to your HTTP request, simply insert it below the method, URL and headers. A blank line **must** be left between the headers and the body. Continuing with our previous example, add the following contents at the end of the file:

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

To encode the request body, Verb will use the `charset` value defined in the `Content-Type` header. If the header is present but `charset` is not defined, the charset `verb-default-request-charset` will be used (default: `utf-8`) and added to the header value. If the header is not present, the charset `verb-default-request-charset` will be used, but no `Content-Type` header will be sent. Note that the current buffer's encoding has no effect on how the request body is encoded.

### Extend and Override Requests

Our example file should now look like the following:

```
* Get users list         :verb:
get https://reqres.in/api/users
Accept: application/json
Content-Language: de-DE

* Create a user          :verb:
post https://reqres.in/api/users
Accept: application/json
Content-Type: application/json

{
    "name": "John",
    "age": 42
}
```

Notice that the two request specifications share many things in common: the URL host, path and one header. In order to avoid repeating all this information, we can actually define a `template` request, establishing all the common attributes among requests, and then extend this template request with different values. Using `template` allows you to avoid specifying an HTTP method at a points in your file where you only want to establish shared attributes for other requests. To use it, create a new level 1 heading, and move the already existing headings below it, making them level 2 headings:

```
* User management             :verb:
template https://reqres.in/api/users
Accept: application/json

** Get users list
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

Now, when we send the request under "Get users list", Verb will collect all the properties defined in all the parent headings tagged with `:verb:` (in this case, a URL and one header), and then extend/override them with the attributes under this specific heading. This is how each attribute of an HTTP request specification is extended/overridden:

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

If you try to send a request from the level 1 header, you'll get an error, as at that level there's no specified HTTP method.

You can create hierarchies with any number of headings, with many levels of nesting. A good idea is to create a single `.org` file to describe, for example, a single HTTP API. This file will contain a level 1 heading defining some common attributes, such as the URL schema, host and root path, along with an `Authentication` header. The level 2 headings will specify different resources, and the level 3 headings will specify actions to run on those resources. For example (unrelated to `guide.org`):

```
* Foobar Blog API                    :verb:
template https://foobar-blog-api.org/api/v1
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

*** Delete all posts
delete
```

### Emacs Lisp Code Tags

You can embed Emacs Lisp code inside request specifications by using code tags. When sending the request, Verb will evaluate all code tags, and replace them with the results of the evaluations. Code tags may appear anywhere in the request specification: the URL, method, headers and body. By default, code tags are delimited with `{{` and `}}` (see the customizable variable `verb-code-tag-delimiters`).

Depending on the type of the resulting value for a code tag, Verb will do the following:
- `string`: The value will be inserted as-is into the request contents.
- `buffer`: The buffer's contents will be inserted into the request using `insert-buffer-substring`. If the buffer's `verb-kill-this-buffer` variable is set to non-`nil`, the buffer will be killed after its contents have been read. The variable's default value is `nil`.
- Other types: The value will be converted to a string using `(format "%s" result)` and inserted into the request contents.

Let's extend the previous example so that it now uses code tags:
```
* User management              :verb:
template https://reqres.in/api/users
Authentication: {{(verb-var token)}}
Accept: application/json

** Get users list
get
Content-Language: de-DE

** Create a user
post
Content-Type: application/json

{
    "name": "{{(user-full-name)}}",
    "age": "{{(read-string "Age: ")}}"
}
```

The example uses the `verb-var` function in the first code tag. This function returns the value of the symbol being passed to it, unless the symbol does not have a value, in which case its value is set using `read-string` and then returned. It is useful for creating request specifications that require external (potentially secret) values, that only need to be set once.

If you wish to quickly re-set the value of a variable previously set with `verb-var`, use the `verb-set-var` command. The command is bound to <kbd>C-c C-r C-v</kbd> by default, and works similarly to the built-in `set-variable` command. You will be prompted for a variable that has been previously set with `verb-var`.

### Last Response

If you wish to access the last response's attributes, use the `verb-last` variable (type: `verb-response`). The following example does this; add it to the ending of your `guide.org` file:

```
** Get last created user
# Extract the "id" value from the previous
# JSON response body.

get /{{(cdr (assoc-string "id" (json-read-from-string (oref verb-last body))))}}
Accept: application/json
```

### Storing Responses by Key

When writing a request specification, you may add properties via the Org special `:properties:`/`:end:` drawer to its heading. Any properties starting with `Verb-` (case insensitive) will be added to the request as metadata. Other properties will be ignored.

The `Verb-Store` property has a special meaning. When this property is set, Verb will automatically store the request's response under the value set. To retrieve the response later, use the `verb-stored-response` function. It takes as an argument the same string key used previously.

So, for example, we could modify our create/retrieve user endpoints like so:

```
** Create a user
:properties:
:Verb-Store: new-user
:end:
post
Content-Type: application/json

{
    "name": "{{(user-full-name)}}",
    "age": "{{(read-string "Age: ")}}"
}

** Get last created user
get /{{(cdr (assoc-string "id" (json-read-from-string (oref (verb-stored-response "new-user") body))))}}
Accept: application/json
```

After the "Create a user" request has been sent at least once, the result will be stored internally under "new-user". It can then be used later at any time. Sending the request again will overwrite the previous value. The `Verb-Store` mechanism is a bit more robust than using just `verb-last`, as sending any (unrelated) request will always set `verb-last` globally.

### Body Lines starting with `*`

You may have noticed that because headings start with `*`, you cannot include lines starting with `*` in your request bodies, because Verb will interpret them as a new heading. To get around this, you can prefix request body lines starting with `*` with an empty code tag, `{{}}`. The empty code tag will evaluate to the empty string, so it won't modify the content of your request body. Following from our previous example, we can add a new level 2 heading:

```
** Upload file to user storage
post /{{(verb-var user-id)}}/upload
Content-Type: text/markdown

# Sample Markdown file

{{}}**This text is bold.**
{{}}*This text is italicized.*
```

### File Uploads

To upload a file, you can use the included `verb-read-file` function. This function reads a file into a buffer and sets its `verb-kill-this-buffer` variable to `t`, and then returns the buffer. Use it from inside code tags to insert the contents of a local file in a request. To test this, we can modify the previous example so that instead of manually writing a Markdown file, we now read one from disk:

```
** Upload file to user storage
post /{{(verb-var user-id)}}/upload
Content-Type: text/markdown

{{(verb-read-file "documents/myfile.md")}}
```

Remember to specify `Content-Type` in your HTTP headers, as Verb won't do this for you. This will let the server know how to interpret the contents of the request.

### Base Headers

You can define a set of base headers for all your HTTP requests via the `verb-base-headers` variable. These headers will be defined globally, but may still be overridden by re-specifying them somewhere in the headings hierarchy. The variable must be set to an alist of `(KEY . VALUE)` elements, where `KEY` and `VALUE` are strings. For example, here's how to add a `User-Agent` header to all requests in all files from your `init.el`:

```elisp
(setq verb-base-headers '(("User-Agent" . "my-user-agent")))
```

### Export Requests

You can export request specifications to other formats by using the `verb-export-request-on-point` command, by default bound to <kbd>C-c C-r C-e</kbd>. When used, you will be prompted for an export function name. The ones currently available are:
- `curl`: Convert the request specification into a [curl](https://curl.haxx.se/) command and add it to the kill ring (clipboard).
- `human`: Display the request specification in a more human-friendly way.
- `verb`: Display the request specification in the same format Verb uses. This is still useful as the request displayed will be the one generated by combining the properties of the parent headings as well.

You can export directly to `curl` by using the `verb-export-request-on-point-curl` command, bound by default to <kbd>C-c C-r C-u</kbd>.

**Note:** code tags will be evaluated when exporting a request.

### Babel Integration
Verb also works on Org [Babel](https://orgmode.org/worg/org-contrib/babel/) source blocks. This feature allows you to send an HTTP request, and view the results in the same `.org` buffer where the request was read from.

To enable this feature, remember to add `verb` to the `org-babel-load-languages` list. To do this, you may add the following to your `init.el`:
```elisp
(org-babel-do-load-languages
 'org-babel-load-languages
 '((verb . t)))
```

Once that's done, simply wrap your HTTP request specifications with `#+begin_src`/`#+end_src` like so:
```
* Make a request to an API
#+begin_src verb :wrap src ob-verb-response
get https://api.kanye.rest
#+end_src
```

Babel source blocks in Verb mode accept a header argument called `:op`. Its default value is `:op send`.

#### Sending Requests (`:op send`)

To send the request, move the point to the source block and press <kbd>C-c C-c</kbd>. The result of the request will appear below. The `:wrap src ob-verb-response` tells Babel to wrap the response in a source block, using `ob-verb-response-mode` as major mode for font locking.

After the request has been sent, Emacs will be blocked until the response has arrived. There's a configurable timeout for this; see the `verb-babel-timeout` variable.

**Note:** when Verb operates on a Babel source block, **it still takes into consideration the whole headings hierarchy**. This means that any attributes defined in lower-level headings will be brought over and potentially overriden by the current source block's. The request specifications in the lower-level headings may be defined in Babel source blocks as well; Verb will read them anyways.

**Note:** the heading containing the source block where <kbd>C-c C-c</kbd> is pressed does not need to be tagged with `:verb:`.

#### Exporting Requests (`:op export ...`)

If you wish to export the request to a particular format instead, use the `:op export ...` header argument on your source block. These are the values it can be used with:
- `:op export curl`: Export this request to `curl` format and insert the results below.
- `:op export human`: Export this request to human-readable format and insert the results below.
- `:op export verb`: Export this request to Verb format and insert the results below.

So for example, if you wanted to export the previous example to `curl`, you would need to write:
```
* Export request to curl
#+begin_src verb :op export curl
get https://api.kanye.rest
#+end_src
```

And then execute the source block again with <kbd>C-c C-c</kbd>, which will execute the export and insert the results below.

### Customization

To see all aspects of Verb that may be customized, use <kbd>M-x</kbd> `customize-group` <kbd>RET</kbd> `verb` <kbd>RET</kbd>.

### Verb Log

When you send a request or receive a response, some information is logged in the `*Verb Log*` buffer. You can use this log to get some more details on any errors that might have happened and other internal stuff. You can disable logging by setting the `verb-enable-log` variable to `nil`. While reading the log, you can press <kbd>q</kbd> to go back to the previous buffer.

## Hooks, Variables, Functions

To see a listing of Verb's public symbols (hooks, functions, variables, classes, etc.), see the [verb-api.md](docs/verb-api.md) file.

## Examples

The [docs/](docs) directory contains various `.org` files which showcase different features of the package.

## Changelog

The changelog for this project can be found in [CHANGELOG.md](CHANGELOG.md).

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
  - Licensing (GPLv3 vs. Public domain).
- [walkman](https://github.com/abrochard/walkman): Write HTTP requests in Org mode and send them using `curl`.
- [http.el](https://github.com/emacs-pe/http.el): I have not tested this package, so I can't provide a comparison.

## License

Distributed under the GNU General Public License, version 3.

See [LICENSE](LICENSE) for more information.
