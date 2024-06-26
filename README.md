<p align="center">
  <img alt="verb" src="https://github.com/federicotdn/verb/raw/main/extra/logo/logo2.png" width="50%">
  <br/>
</p>

**Verb** is a package for Emacs which allows you to organize and send HTTP requests.

The package introduces a new minor mode, **Verb mode**, which works as an extension to [Org mode](https://orgmode.org/). The core idea is to organize specifications for HTTP requests using Org's tree structure. Properties defined in the child headings extend or sometimes override properties defined in the parent headings - this way, it is easy to define many HTTP request specifications without having to repeat common components as URL hosts, authentication headers, ports, etc. Verb tries to combine the usefulness of Org mode with the common functionality provided by other HTTP clients. However, very little knowledge of Org mode is needed to use Verb.

Verb requires at least Emacs version 26 to work.

[![CI Status](https://github.com/federicotdn/verb/workflows/CI/badge.svg)](https://github.com/federicotdn/verb/actions)
[![MELPA](https://melpa.org/packages/verb-badge.svg)](https://melpa.org/#/verb)
[![MELPA Stable](https://stable.melpa.org/packages/verb-badge.svg)](https://stable.melpa.org/#/verb)
![License](https://img.shields.io/github/license/federicotdn/verb.svg)

## Features

- Send HTTP and HTTPS requests from Emacs.
- Organize request specifications into trees using Org mode.
- Easily define common attributes (URLs, query strings, headers, etc.) for many requests.
- Correctly handle text encodings (charsets) for requests and responses.
- Display PDF, PNG, JPEG, BMP, GIF and SVG responses.
- Embed Emacs Lisp expressions in specifications (with code completion!).
- Can export requests to `curl` and other external tools.
- Integrates with Babel and EWW.
- Includes mouse support (menu bar and mode line).
- Supports file uploads.
- Has no dependencies!
- Easy to use! (hopefully).

## Table of Contents
- [Installation](#installation)
- [Quick Start](#quick-start)
- [Screenshots](#screenshots)
- [Usage Guide](#usage-guide)
  - [Writing Request Specifications](#writing-request-specifications)
  - [Enabling Verb in Org Buffers](#enabling-verb-in-org-buffers)
  - [Sending Requests](#sending-requests)
  - [The Response Body Buffer](#the-response-body-buffer)
  - [Re-sending Requests](#re-sending-requests)
  - [Show Corresponding Request](#show-corresponding-request)
  - [The Response Headers Buffer](#the-response-headers-buffer)
  - [Specifying HTTP Headers](#specifying-http-headers)
  - [Adding a Body](#adding-a-body)
  - [Extend and Override Requests](#extend-and-override-requests)
  - [Modifying Requests before Sending](#modifying-requests-before-sending)
  - [Emacs Lisp Code Tags](#emacs-lisp-code-tags)
  - [Code Completion](#code-completion)
  - [Verb Variables](#verb-variables)
  - [Last Response](#last-response)
  - [Storing Responses by Key](#storing-responses-by-key)
  - [Request Mapping Functions](#request-mapping-functions)
  - [Body Lines starting with `*`](#body-lines-starting-with-)
  - [File Uploads](#file-uploads)
  - [Multipart Data](#multipart-data)
  - [Base Headers](#base-headers)
  - [Export Requests](#export-requests)
  - [Babel Integration](#babel-integration)
    - [Sending Requests](#sending-requests-op-send)
    - [Send with Partial Retrieval](#send-with-partial-retrieval-op-send-)
    - [Exporting Requests](#exporting-requests-op-export-)
  - [Proxies](#proxies)
  - [Customization](#customization)
  - [Verb Log](#verb-log)
- [Hooks, Variables, Functions](#hooks-variables-functions)
- [Examples](#examples)
- [Troubleshooting](#troubleshooting)
- [Changelog](#changelog)
- [Contributing](#contributing)
- [Related Packages](#related-packages)
- [Similar Packages](#similar-packages)
- [Contributors](#contributors)
- [License](#license)

## Installation
### Emacs 26+

You can install Verb by using the `package-install` command (make sure either [MELPA](https://melpa.org/) or [MELPA Stable](https://stable.melpa.org/) are included in your package sources):

<kbd>M-x</kbd> `package-install` <kbd>RET</kbd> `verb` <kbd>RET</kbd>

Once Verb has been installed and loaded, add the following to your `init.el`:
```elisp
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map))
```

If you're using [use-package](https://github.com/jwiegley/use-package), you'll need to modify your entry for `org` instead. Create one if you don't have one already, and under the `:config` key, add the code necessary to bind the Verb command map to a key. The end result should look something like this:
```elisp
(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config (define-key org-mode-map (kbd "C-c C-r") verb-command-map))
```

Both cases will set <kbd>C-c C-r</kbd> as the prefix key for all Verb commands in Org mode. Feel free to use another key if you prefer that.

If you do not want to use a package manager, you can just add all `.el` files to your `load-path` instead of installing it as a package.

### Spacemacs

Verb is available on the `develop` branch of Spacemacs. To enable it, read the [documentation on enabling Verb support](https://develop.spacemacs.org/layers/+emacs/org/README.html#verb-support). To view the default keybindings, see the [Verb mode bindings](https://develop.spacemacs.org/layers/+emacs/org/README.html#verb) table.

## Quick Start

Here's a minimal example in case you want to get started without reading the [Usage Guide](#usage-guide). Place the following on an Org mode buffer:
```
* Example request                            :verb:
get https://api.ipify.org/?format=json
```

Then, press <kbd>C-c C-r C-r</kbd> to send the HTTP request.

Here's a more complete example that includes defines two requests, both of which share the same base URL and `Accept` header:

```
* Quick Start for Verb                       :verb:
# Comments start with '#'. You can only place
# comments before the URL and in the headers.

template https://reqres.in/api
Accept: application/json

** Create a new user
# Because the base URL is defined in the parent
# heading, there's no need to repeat it here.
# We can also add more headers here, or override
# ones defined in parents.

post /users
Content-Type: application/json; charset=utf-8

{
    "name": "Jane Smith",
    "city": "Berlin"
}

** Fetch a product
# Use Emacs Lisp code tags to make the request
# content dynamic. Code tags can be used anywhere
# in the request specification.

get /products/{{(read-number "Product ID: ")}}
```

You can send any of the two requests by moving the point to one of the level 2 headings (marked with `**`), and then pressing <kbd>C-c C-r C-r</kbd>.

## Screenshots

![n2](https://user-images.githubusercontent.com/6868935/73489132-6bc81f00-43aa-11ea-9815-2d482cb60a40.png)
![n3](https://user-images.githubusercontent.com/6868935/73489133-6bc81f00-43aa-11ea-84c3-d9c1695145d0.png)
![n1](https://user-images.githubusercontent.com/6868935/73489131-6b2f8880-43aa-11ea-82bd-45142b942055.png)

## Usage Guide

This guide, and other Verb-related documentation, assume that you're using <kbd>C-c C-r</kbd> as the prefix key for all Verb commands, and that you're also getting started with Org mode.

All public (and private) variables and functions in the Verb package are documented. If you wish to know more about one of them, use <kbd>C-h v</kbd> and <kbd>C-h f</kbd> respectively.

### Writing Request Specifications
After setting up Verb, begin by creating a new `guide.org` file. In the example file, add the following contents:

```
* Get users list         :verb:
get https://reqres.in/api/users
```

This defines a minimal HTTP request specification under the "Get users list" heading, composed of a method (`GET`) and a URL (`https://reqres.in/api/users`). The heading is prefixed with only one `*`, which makes it a level 1 heading. The number of `*`s determines a heading's level. All the text under a heading corresponds to the HTTP request it is describing. It is not possible to write request specifications without adding a heading at the top.

Note that the heading has a `:verb:` tag. **Verb functions only process headings that contain this tag, and ignore the rest.** This allows you to create documents that may have a combination of HTTP request specifications and other information types. To tag a heading, simply move the point to it and press <kbd>C-c C-c</kbd>, and then type in `verb` <kbd>RET</kbd>. Note that in Org mode, by default, headings inherit their parents' tags (see the `org-use-tag-inheritance` variable). This implies that once you've tagged one of the parent headings, all its child headings will have that tag as well.

To easily add the `:verb:` tag to all headings in an Org document, add the following at the top of your file:
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

In general, the first option should be useful enough for most cases. Once Verb mode has been enabled, `Verb` should appear on the modeline. To disable Verb mode, run <kbd>M-x</kbd>`verb-mode`<kbd>RET</kbd>.

### Sending Requests

To actually send the HTTP request, use one of the `verb-send-request-on-point` commands. They are the following:
- <kbd>C-c C-r C-r</kbd>: `verb-send-request-on-point-other-window-stay` sends the request and shows the response on a buffer in another window, but doesn't switch to that window.
- <kbd>C-c C-r C-s</kbd>: `verb-send-request-on-point-other-window` sends the request, shows the response on a buffer in another window, and switches to it.
- <kbd>C-c C-r C-f</kbd>: `verb-send-request-on-point` sends the request, and shows the response on a buffer in the currently selected window.
- <kbd>C-c C-r C-<return></kbd>: `verb-send-request-on-point-no-window` sends the request, but does not show the response buffer anywhere. The response status (e.g. `HTTP/1.1 200 OK | GET http://example.com`) will be shown on the minibuffer. This is useful for cases where one is only interested in the request's side effects.

Request sending is asynchronous - you can do other stuff while Emacs waits for the server's response. If the response is taking too long to be received, a warning will be displayed in the minibuffer. You can modify this behaviour by modifying the `verb-show-timeout-warning` variable's value.

### The Response Body Buffer

After you have sent the request and the server has answered back successfully, you should now be seeing the populated response body buffer. The response body buffer always has the `verb-response-body-mode` minor mode activated (indicated by `Verb[Body]` in the modeline).

The buffer will have an active [header line](https://www.gnu.org/software/emacs/manual/html_node/elisp/Header-Lines.html), showing something similar to:

```
HTTP/1.1 200 OK | 0.754s | application/json | 1020 bytes | /foo
```

This text indicates the status of the HTTP response, the time in seconds it took for it to be completed, the type of the contents received (or `-` if the content type is unknown), and the number of bytes in the response body (read from the `Content-Length` header, when possible, otherwise from the local buffer size).

The contents of the response body will be shown on the buffer. To choose how they will be actually shown, the following steps are followed:

1. The content type is extracted from the `Content-Type` header. If the header is not present, the content type is defined as `nil`.
2. A content handler is chosen for this content type. There are two types of handlers: handlers for text content types (such as JSON, XML, etc.) and handlers for binary content types (such as PNG, PDF, etc.). These handlers are listed in the `verb-content-type-handlers` variable. If no handler matched the content type (or if the content type is `nil`), choose `fundamental-mode` by default (as a text content type handler).
3. Depending on the content type handler chosen: \
   **Text:** If the chosen handler is for text, decode the response body using the charset described in the `Content-Type` header. If no charset was specified, use the one specified by `verb-default-response-charset` (default: `utf-8`). After that is done, call the handler (e.g. `xml-mode`). \
   **Binary:** If the chosen handler is for a binary type, call the handler directly after loading the raw bytes into the buffer (e.g. `doc-view-mode`).
4. The handler will have set an appropriate major mode to display and/or edit the received content.

There's two recommended ways of closing response buffers:
- If the response buffer is the current buffer, you can use the `verb-kill-response-buffer-and-window` command, which is bound by default to <kbd>C-c C-r C-k</kbd>. This command will also kill the associated response headers buffer (see the [Response Headers Buffer](https://github.com/federicotdn/verb#the-response-headers-buffer) section).
- If the response buffer is not the current buffer (e.g. you are still on your `guide.org` buffer), you can kill **all** response buffers by using the `verb-kill-all-response-buffers`, which is bound to <kbd>C-c C-r C-k</kbd> by default. Response headers buffers will also be killed automatically.

As you send more HTTP requests, more response buffers will be created, with `<N>` at the end of their name to distinguish between them. If you wish to automatically have old response buffers killed when making a new request, set the `verb-auto-kill-response-buffers` variable to `t`. If wish for old response buffers to be killed, with the exception of the N most recent ones, then set `verb-auto-kill-response-buffers` to that integer number. This is useful for keeping track of the history of responses received, without creating too many buffers.

### Re-sending Requests

If you wish to re-send the request that generated the current response buffer, select the window showing it and use the `verb-re-send-request` command, which is bound to <kbd>C-c C-r C-f</kbd> by default. Note that the exact same request will be sent, even if the originating `.org` file was modified. To instead re-send the request using EWW, use <kbd>C-c C-r C-w</kbd> instead (this only works for `GET` requests).

### Show Corresponding Request

While viewing the contents of an HTTP response, you can use the `verb-show-request` command in order to show the corresponding request that generated this response. By default, the command is bound to <kbd>C-c C-r C-s</kbd>.

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
- `Content-Length`: *number of bytes in request body* (only when body is present)
- `Host`: *URL host*
- `Accept`: `*/*` (default value, but may be overwritten by the user)
- `Accept-Encoding`: `gzip`
- `Extension`: `Security/Digest Security/SSL`

If you include one of these headers in one of your requests (except `Accept`), Verb will add a warning to the [log](#verb-log).

**Note:** "header" != "heading", "header" is used to refer to HTTP headers, and "heading" is used to refer to the elements Org mode uses to separate sections of text. Sometimes, "headline" or "outline" is used to refer to headings as well.

### Adding a Body

To add a body to your HTTP request, simply insert it below the method, URL and headers. A blank line **must** be left between the headers and the body. Continuing with our previous example, add the following contents at the end of the file:

```
* Create a user         :verb:
post https://reqres.in/api/users
Accept: application/json
Content-Type: application/json; charset=utf-8

{
    "name": "John",
    "age": 42
}
```

The body will include everything starting from the line next to the blank line after the headers, up to the buffer's ending or the next heading (i.e. the next line starting with `*`).

**Note**: By default, all whitespace present will be included in the request body. You can control this behaviour with the `verb-trim-body-end` variable, for example, set it to `"[ \t\n\r]+"` to trim all trailing whitespace. This is useful if you wish to leave some blank lines between request specifications for increased readability.

To encode the request body, Verb will use the `charset` value defined in the `Content-Type` header of the request. If the header is present but `charset` is not defined, or if the header is not present, the charset `verb-default-request-charset` will be used (default: `utf-8`). Note that the current buffer's file encoding has no effect on how the request body is encoded.

If your body contains binary data (i.e. raw bytes that do not correspond to any particular character), that data will be sent without any encoding.

The request body can also be wrapped inside a Babel source block. If this is the case, the lines containing the `#+begin_src` and `#+end_src` delimiters will be automatically erased before the request is sent. For example, the request body above could be wrapped with a `javascript` source block for better font locking:

```
* Create a user         :verb:
post https://reqres.in/api/users
Accept: application/json
Content-Type: application/json; charset=utf-8

#+begin_src javascript
{
    "name": "John",
    "age": 42
}
#+end_src
```

**Note**: This feature is **not** related with Verb's [Babel Integration](#babel-integration), which only applies to Babel source blocks with `verb` specified as language, and takes into consideration the whole request specification (not just the body).

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
Content-Type: application/json; charset=utf-8

{
    "name": "John",
    "age": 42
}
```

Notice that the two request specifications share many things in common: the URL host, path and one header. In order to avoid repeating all this information, we can actually define a `template` request, establishing all the common attributes among requests, and then extend this template request with different values. Using `template` allows you to avoid specifying an HTTP method at a points in your file where you only want to establish shared attributes for other requests. To use it, create a new level 1 heading, and move the already existing headings below it, making them level 2 child headings:

```
* User management             :verb:
template https://reqres.in/api/users
Accept: application/json

** Get users list
get
Content-Language: de-DE

** Create a user
post
Content-Type: application/json; charset=utf-8

{
    "name": "John",
    "age": 42
}
```

Now, when we send the request under "Get users list", Verb will collect all the properties defined in all the parent headings tagged with `:verb:` (in this case, a URL and one header), and then extend/override them with the attributes under this specific heading. Any number of levels can be traversed this way. This is how each attribute of an HTTP request specification is extended/overridden:

- **Method:** The last heading's (i.e. the one with no children) method will be used. The value `template` does not count as a method and will be ignored.
- **URL:**
  - **Scheme**: The last defined heading's URL scheme will be used (`http` or `https`).
  - **Host**: The last defined heading's URL host will be used.
  - **Port**: The last defined heading's URL port will be used.
  - **Path**: All paths will be concatenated, starting with the first heading (i.e. the topmost parent).
  - **Query**: Query string arguments will be merged. Values from child headings have higher priority.
  - **Fragment**: The last defined heading's URL fragment will be used.
- **Headers**: All headers will be merged. Values from child headings have higher priority.
- **Body**: The last request body present in a heading will be used (if no heading defines a body, none will be used).

If you try to send a request from the level 1 header, you'll get an error, as at that level there's no specified HTTP method.

You can create hierarchies with any number of headings, with many levels of nesting. A good idea is to create a single `.org` file to describe, for example, a single HTTP API. This file will contain a level 1 heading defining some common attributes, such as the URL scheme, host and root path, along with an `Authentication` header. The level 2 headings will specify different resources (e.g. `users`, `products`, etc.), and the level 3 headings will specify actions to run on those resources (e.g. `post`, `put`, etc.). For example (unrelated to `guide.org`):

```
* Foobar Blog API                    :verb:
template https://foobar-blog-api.org/api/v1
Accept: application/json

** Users
template /users

*** Create a user
post
Content-Type: application/json; charset=utf-8

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

### Modifying Requests before Sending

As you add more and more headings with different properties, it can get hard to track what will actually be sent once you use one of the `verb-send-request-on-point-*` commands. To review a request before it is sent, use the keyboard prefix argument <kbd>C-u</kbd> before invoking one of the send commands. This will open a temporary buffer which will contain only the request that is about to be sent. In this buffer, you can actually modify the contents of the request in whatever way you like. By doing this, you can try different variations of one request, without having to edit your `.org` file.

Once you have finished reviewing/modifying the request, press <kbd>C-c C-c</kbd> to send it. If you don't want to send the request, press <kbd>C-c C-k</kbd> to kill the buffer.

**Note**: Any changes done in the temporary buffer will not be saved.

### Emacs Lisp Code Tags

You can embed Emacs Lisp code inside request specifications by using code tags. When sending the request, Verb will evaluate all code tags, and replace them with the results of the evaluations. Code tags may appear anywhere in the request specification: the URL, method, headers and body. By default, code tags are delimited with `{{` and `}}` (see the customizable variable `verb-code-tag-delimiters`). Note that code tags are in no way related to [Org mode macros](https://orgmode.org/manual/Macro-Replacement.html).

Depending on the type of the resulting value for a code tag, Verb will do the following:
- `string`: The value will be inserted as-is into the request contents.
- `buffer`: The buffer's contents will be inserted into the request using `insert-buffer-substring`. If the buffer's `verb-kill-this-buffer` variable is set to non-`nil`, the buffer will be killed after its contents have been read. The variable's default value is `nil`.
- Other types: The value will be converted to a string using `(format "%s" result)` and inserted into the request contents.

Let's extend the previous example so that it now uses code tags:
```
* User management              :verb:
template https://reqres.in/api/users
Accept: application/json

** Get users list
get
Content-Language: de-DE

** Create a user
post
Content-Type: application/json; charset=utf-8

{
    "name": "{{(user-full-name)}}",
    "age": "{{(read-string "Age: ")}}"
}
```

Notice that interactive functions like `read-string` can be used inside code tags as well - they will be evaluated before the request is sent, and the resulting value will be inserted into the content.

### Code Completion

You can enable completion for Emacs Lisp inside code tags. To do this, set the `verb-enable-elisp-completion` variable to `t` (the default value). Code completion will work automatically with [`company-mode`](https://github.com/company-mode/company-mode), if it is installed.

Note that the point must be surrounded by the code tag delimiters (e.g. `{{` and `}}`) in the same line for completion to work. If you're using `electric-pair-mode`, matching tag delimiters will be inserted automatically, so this won't be a problem. `verb-mode` should also be enabled, as enabling it will load the completion function itself.

### Verb Variables

Let's suppose that the two endpoints from the previous example now require authentication to be used. We could then modify the example to look like this:
```
* User management              :verb:
template https://reqres.in/api/users
Accept: application/json
Authentication: {{(verb-var token)}}

** Get users list
get
Content-Language: de-DE

** Create a user
post
Content-Type: application/json; charset=utf-8

{
    "name": "{{(user-full-name)}}",
    "age": "{{(read-string "Age: ")}}"
}
```

The example now uses the `verb-var` macro in the first code tag. This macro essentially returns the value associated with the specified symbol - in this case, `token`. If the symbol does not have any associated value yet, the user is prompted for one using `read-string`. The value is then associated with the symbol and returned. If you don't wish to be prompted for a value, you can specify a second parameter, which will be used as the default value. That value will be associated to the symbol the first time `verb-var` is invoked.

If you wish to explicitly re-set the value of a variable set with `verb-var`, use the `verb-set-var` interactive command. The command is bound to <kbd>C-c C-r C-v</kbd> by default, and works similarly to the built-in `set-variable` command. You will be prompted for a variable that has been previously set with `verb-var`. You may also specify a completely new variable name, in which case it will be created and its value set. To see the current value of all variables, use the `verb-show-vars` command. To unset all variable values, use the `verb-unset-vars` command.

To quickly copy the value of a variable into the clipboard, use the keyboard prefix argument <kbd>C-u</kbd> before invoking `verb-set-var`.

`verb-var` and `verb-set-var` are useful for writing requests that include sensitive information (such as passwords or tokens), or for writing requests that can be parameterized with different values (such as IDs or search terms).

**Note**: Values set with `verb-var` and `verb-set-var` will be lost if the buffer is killed.

### Last Response

If you wish to access the last response's attributes, use the `verb-last` variable (type: `verb-response`). The following example does this; add it to the ending of your `guide.org` file:

```
(...)

** Get last created user
# Extract the "id" value from the previous
# JSON response body.

get /{{(verb-json-get (oref verb-last body) "id")}}
Accept: application/json
```

The `verb-json-get` function takes a JSON-formatted text as its first argument and a list of keys as the rest, and returns the value under those keys in the JSON text (similar to how [JSONPath](https://goessner.net/articles/JsonPath/) works). This function is useful for using previous responses' contents, check its documentation for more details.

If you wish to use the last response's headers instead, you can use the `verb-headers-get` function. An example call may look like: `(verb-headers-get (oref verb-last headers) "Content-Type")`, which will return the string contents of the `Content-Type` response header.

### Storing Responses by Key

When writing a request specification, you may add [properties](https://orgmode.org/manual/Property-Syntax.html) via the Org special `:properties:`/`:end:` drawer to its heading. Any properties starting with `Verb-` (case insensitive) will be added to the request as metadata. Other properties will be ignored.

The `Verb-Store` property has a special meaning. When this property is set, Verb will automatically store the request's response under the specified value. To retrieve the response later, use the `verb-stored-response` function. It takes as an argument the same string key used previously.

So, for example, we could modify our create/retrieve user endpoints like so:

```
(...)

** Create a user
:properties:
:Verb-Store: new-user
:end:

post
Content-Type: application/json; charset=utf-8

{
    "name": "{{(user-full-name)}}",
    "age": "{{(read-string "Age: ")}}"
}

** Get last created user
get /{{(verb-json-get (oref (verb-stored-response "new-user") body) "id")}}
Accept: application/json
```

After the "Create a user" request has been sent at least once, the result will be stored internally under "new-user". It can then be used later at any time. Sending the request again will overwrite the previous value, and killing the response buffer will not erase the stored response. The `Verb-Store` mechanism is a bit more robust than using just `verb-last`, as sending any (unrelated) request will always re-set `verb-last` globally.

**Note**: When reading heading properties such as `Verb-Store`, properties for parent headings are ignored by default. This can be controlled using the `org-use-property-inheritance` variable (default: `nil`).

### Request Mapping Functions

The `Verb-Map-Request` heading property also has a special meaning in Verb. When present, it can be used to specify a mapping function than will be called right before the corresponding request is sent or exported, with the request itself as its sole argument. The function must return the same request specification object (type `verb-request-spec`), or a new one. With this, it is possible to apply custom transformations to requests before they are sent or exported.

So, for example, having the following function:
```elisp
(defun remove-body-newlines (rs)
  ;; RS is of type `verb-request-spec'
  (oset rs body (replace-regexp-in-string "\n" " " (oref rs body)))
  rs)
```

We could add the following level 2 heading to the example in the previous section:

```
(...)

** Upload file to user storage
:properties:
:Verb-Map-Request: remove-body-newlines
:end:

post /{{(verb-var user-id)}}/upload
Content-Type: text/plain; charset=utf-8

foo,
bar,
baz
```

When sent or exported, the request's body will by modified by `remove-body-newlines`, and the resulting body content will be a single line, `foo,bar,baz`.

The function to be mapped can also be a `lambda` expression, like so:

```
(...)

** Upload file to user storage
:properties:
:Verb-Map-Request:  (lambda (rs)
:Verb-Map-Request+:   (thread-last
:Verb-Map-Request+:     (oref rs body)
:Verb-Map-Request+:     (replace-regexp-in-string "\n" " ")
:Verb-Map-Request+:     (oset rs body))
:Verb-Map-Request+:   rs)
:end:

post /{{(verb-var user-id)}}/upload
Content-Type: text/plain; charset=utf-8

foo,
bar,
baz
```

This has the same effect as the previous example. Note also how we've used the feature of adding to a propertie's value. The final `lambda` expression will be equivalent to

```elisp
(lambda (rs) (thread-last (oref rs body) (replace-regexp-in-string "\n" " ") (oset rs body)) rs)
```

**Note**: The mapping function will be called after evaluating code tags, and the request specification passed will already have its inherited/overridden values from parent headings.

**Note**: When reading heading properties such as `Verb-Map-Request`, properties for parent headings are ignored by default. This can be controlled using the `org-use-property-inheritance` variable (default: `nil`).

### Body Lines starting with `*`

You may have noticed that because headings start with `*`, you cannot include lines starting with `*` in your request bodies, because Org will interpret them as a new heading. To get around this, you can prefix request body lines starting with `*` with an empty code tag, `{{}}`. The empty code tag will evaluate to the empty string, so it won't modify the content of your request body. Following from our previous example, we can modify it like so:

```
(...)

** Upload file to user storage
post /{{(verb-var user-id)}}/upload
Content-Type: text/markdown; charset=utf-8

# Sample Markdown file

{{}}**This text is bold.**
{{}}*This text is italicized.*
```

### File Uploads

To upload a file, you can use the included `verb-read-file` function. This function reads a file into a buffer and sets its `verb-kill-this-buffer` variable to `t`, and then returns the buffer. Use it from inside code tags to insert the contents of a local file in a request. To test this, we can modify the previous example so that instead of manually writing a Markdown file, we now read one from disk:

```
(...)

** Upload file to user storage
post /{{(verb-var user-id)}}/upload
Content-Type: text/markdown; charset=utf-8

{{(verb-read-file "documents/myfile.md")}}
```

Remember to specify `Content-Type` in your HTTP headers, as Verb won't do this for you. This will let the server know how to interpret the contents of the request.

**Note**: If uploading binary files (e.g. a PNG image), it's a good idea to set `verb-read-file`'s second argument (`coding-system`) to `'binary`. This will instruct Emacs to insert the file contents into the request buffer as raw bytes.

### Multipart Data

Verb makes it easy for you to use the `multipart/form-data` content type in your requests. Two helper functions are provided: `verb-boundary` and `verb-part`.

When `verb-boundary` is called using code tags within a request specification, it will return a string containing a valid randomly-generated multipart boundary. This function must be called at least once in order to establish the boundary value when a request is being constructed from request specifications.

On the other hand, the `verb-part` function can be used in code tags to start new parts (when called with at least one argument), and also to insert the final boundary delimiter (when called with no arguments). The first argument will correspond to the `name` attribute of the `Content-Disposition` header, and the second to the `filename` attribute of the same header.

The following is an example that combines these two functions, along with `verb-read-file`:

```
(...)

** Upload two files to user storage
:properties:
:Verb-Map-Request: verb-body-cr-to-crlf
:end:

post /{{(verb-var user-id)}}/upload
Content-Type: multipart/form-data; boundary={{(verb-boundary)}}

{{(verb-part "file" "file1.txt")}}
Content-Type: text/plain

{{(verb-read-file "documents/file1.txt")}}
{{(verb-part "file" "file2.xml")}}
Content-Type: application/xml

{{(verb-read-file "documents/file2.xml")}}
{{(verb-part)}}
```

**Important**: In most cases, you will also need to apply the `verb-body-cr-to-crlf` function to your request before it is sent. This is needed to ensure that all line endings in the request body use CRLF instead of just LF. This is also shown in the example above.

### Base Headers

You can define a set of base headers for all your HTTP requests in all `.org` files via the `verb-base-headers` variable. These headers will be defined globally, but may still be overridden by re-specifying them somewhere in the headings hierarchy. The variable must be set to an alist of `(KEY . VALUE)` elements, where `KEY` and `VALUE` are strings. For example, here's how to add a `User-Agent` header to all requests in all files from your `init.el`:

```elisp
(setq verb-base-headers '(("User-Agent" . "my-user-agent")))
```

### Export Requests

You can export request specifications to other formats or tools by using the `verb-export-request-on-point` command, by default bound to <kbd>C-c C-r C-e</kbd>. When used, you will be prompted for an export function. The ones currently available are:
- `curl`: Convert the request specification into a [curl](https://curl.haxx.se/) command and add it to the kill ring (clipboard).
- `verb`: Display the request specification in the same format Verb uses. This is still useful as the request displayed will be the one generated by combining the properties of the parent headings as well.
- `eww`: Perform the request described by the specification using EWW (Emacs Web Wowser). This will only work on `GET` requests.
- `websocat`: Convert the request specification into a [websocat](https://github.com/vi/websocat) command and add it to the kill ring.

**Note:** Code tags will be evaluated when exporting a request.

### Babel Integration
Verb also works on Org [Babel](https://orgmode.org/worg/org-contrib/babel/) source blocks. This feature allows you to send an HTTP request, and view the results in the same `.org` buffer where the request was read from. You can also export requests to different formats (like `curl`) and view the results in the same buffer as well.

To enable this feature, remember to add `verb` to the `org-babel-load-languages` list. To do this, you may add the following to your `init.el`:
```elisp
(org-babel-do-load-languages
 'org-babel-load-languages
 '((verb . t)))
```

Once that's done, simply wrap your HTTP request specification (excluding the Org heading) with `#+begin_src`/`#+end_src` using `verb` as the source block language. For example, given the following request:
```
* Make a request to an API         :verb:

post https://example.com/api/users
Content-Type: application/json; charset=utf-8

{
    "name": "Jane Smith",
    "age": "35"
}
```

The Babel-compatible version would be:
```
* Make a request to an API         :verb:

#+begin_src verb :wrap src ob-verb-response
post https://example.com/api/users
Content-Type: application/json; charset=utf-8

{
    "name": "Jane Smith",
    "age": "35"
}
#+end_src
```

Babel source blocks with `verb` as a language accept a header argument called `:op`. Depending on the value that appears after this argument, Verb will execute different actions when <kbd>C-c C-c</kbd> is pressed.

**Note:** It is possible to specify arguments for a `verb` source block by using the [`:var` keyword](https://orgmode.org/manual/Environment-of-a-Code-Block.html). To read the arguments, use `(verb-var <variable-name>)` inside a code block.

#### Sending Requests (`:op send`)

By default, if `:op` is not specified, Verb will assume `:op send` was intended.

To send the request, move the point to its `verb` source block and press <kbd>C-c C-c</kbd>. The result of the request will appear below. Adding the `:wrap src ob-verb-response` argument tells Babel to wrap the response in another source block, using `ob-verb-response-mode` as major mode for font locking.

As opposed to requests sent with the `verb-send-request-on-point-*` commands, requests sent with Babel will block Emacs until they are complete. There's a configurable timeout for this, see the `verb-babel-timeout` variable for more details.

**Note:** When Verb operates on a Babel source block, **it still takes into consideration the whole headings hierarchy**. This means that any attributes defined in parent headings will be brought over and potentially overridden by the current source block's. The request specifications in the parent headings may be defined in Babel source blocks as well, Verb will read them anyways. In other words, you can freely mix between regular request specifications and request specification written inside Babel source blocks within the hierarchy.

**Note:** The heading containing the source block where <kbd>C-c C-c</kbd> is pressed does not need to be tagged with `:verb:`.

#### Send with Partial Retrieval (`:op send ...`)

Instead of specifying just `:op send`, you may add an additional argument: `get-headers` or `get-body`. Using the former will change the result of executing the source block to just the response headers. Using the latter will do the same, but for the response body. Here's an example:

```
* Make a request to an API (get body only)         :verb:
#+begin_src verb :wrap src ob-verb-response :op send get-body
post https://example.com/api/users
Content-Type: application/json; charset=utf-8

{
    "name": "Jane Smith",
    "age": "35"
}
#+end_src
```

#### Exporting Requests (`:op export ...`)

If you wish to export the request to a particular format instead, use the `:op export ...` header argument on your source block. These are the values it can be used with:
- `:op export curl`: Export this request to `curl` format and insert the results below.
- `:op export verb`: Export this request to Verb format and insert the results below.
- `:op export websocat`: Export this request to `websocat` format and insert the results below.

So for example, if you wanted to export the previous example to `curl`, you would need to write:
```
* Export request to curl         :verb:
#+begin_src verb :op export curl
post https://example.com/api/users
Content-Type: application/json; charset=utf-8

{
    "name": "Jane Smith",
    "age": "35"
}
#+end_src
```

And then execute the source block again with <kbd>C-c C-c</kbd>, which will execute the export and insert the results below.

### Proxies
There's two ways of using HTTP proxies in Verb. The first one is to manually configure the `url-proxy-services` variable like explained in [Proxies and Gatewaying](https://www.gnu.org/software/emacs/manual/html_node/url/Proxies.html). The second one is to specify a proxy address by using the `Verb-Proxy` heading property:

```
** Make a request using an HTTP proxy         :verb:
:properties:
:Verb-Proxy: my-proxy:5050
:end:

get http://internal-api/users
```

When the request is sent, the value of `Verb-Proxy` will automatically be added to `url-proxy-services`, and then automatically removed.

### Customization

To see all aspects of Verb that may be customized, use <kbd>M-x</kbd> `customize-group` <kbd>RET</kbd> `verb` <kbd>RET</kbd>.

### Verb Log

When you send a request or receive a response, some information is logged in the `*Verb Log*` buffer. You can use this log to get some more details on any errors that might have happened and other internal stuff. You can disable logging by setting the `verb-enable-log` variable to `nil`. To read the log, you can use the `verb-show-log` command. While reading the log, you can press <kbd>q</kbd> to go back to the previous buffer.

The Emacs `url` library also keeps its own internal log - it can be useful for debugging requests that are not working as expected. To enable `url` logging, set `url-debug` to `t` (by default, it's disabled). After sending a request, switch to the `*URL-DEBUG*` buffer to read any logged information.

## Hooks, Variables, Functions

To see a listing of Verb's publicly defined hooks, functions, variables and classes, see the [verb-api.md](extra/verb-api.md) file.

## Examples

The [examples/](examples) directory contains various `.org` files which showcase different features of the package.

## Troubleshooting
**Problem**: When trying to send a request, an error is shown: "No request specifications found".

**Fix**: Tag the headings containing request specifications with `:verb:`. Tags are inherited by default, so in most cases you can just tag the topmost parent heading.

---

**Problem**: URL elements containing underscores such as `page_size` are shown as subscripts ([Issue #3](https://github.com/federicotdn/verb/issues/3)).

**Fix**: Set the `org-use-sub-superscripts` variable to `{}` or `nil`. You can do this file-locally by adding the following at the end of the file:
```
# Local Variables:
# org-use-sub-superscripts: {}
# End:
```

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

To run only one test, set the `SELECTOR` environment variable to the tests's name:
```bash
$ SELECTOR=test-nonempty-string make test
```

You can also check for byte-compilation warnings and documentation/package issues. First, run (needed only once):
```bash
$ make setup-check
```

After that, run the checks:
```bash
$ make check
```

It's a good idea to test your changes on a vanilla Emacs instance (`-q` flag). To easily do this, use the `run` recipe:
```bash
$ make run
```

Finally, a list of all recipes and their descriptions can be obtained using `make help` or simply `make`.

A PR will need to successfully go through all the checks mentioned above in order to be reviewed first. Please remember to target your PR against the `main` branch (not `master`).

## Related Packages

Verb's functionality can be extended via some related packages, such as:
- [impostman](https://github.com/flashcode/impostman): Can be used to import [Postman](https://www.postman.com/) collections into Verb.

## Similar Packages

- [restclient](https://github.com/pashky/restclient.el): Verb is an attempt to improve upon the core idea of the `restclient` package: writing request specifications on a buffer, and receiving the responses on another. As of April 17, 2024 `restclient` has been archived and is no longer maintained.
- [walkman](https://github.com/abrochard/walkman): Write HTTP requests in Org mode and send them using `curl`.
- [plz-see.el](https://github.com/astoff/plz-see.el): An interactive HTTP client for Emacs based on the [plz.el](https://github.com/alphapapa/plz.el) library.

## Contributors
These are the users that have contributed to developing Verb, via code and/or documentation (in order of date of first contribution):
- [stig](https://github.com/stig)
- [flashcode](https://github.com/flashcode)
- [ananthakumaran](https://github.com/ananthakumaran)
- [prashantvithani](https://github.com/prashantvithani)
- [c4710n](https://github.com/c4710n)
- [bigodel](https://github.com/bigodel)
- [agzam](https://github.com/agzam)
- [isamert](https://github.com/isamert)

Thank you!

## License

Distributed under the GNU General Public License, version 3.

See [LICENSE](LICENSE) for more information.
