import os
from sanic import Sanic, response
from sanic.request import Request
from sanic.response import HTTPResponse
from sanic.exceptions import NotFound, MethodNotSupported

app = Sanic(name="test server")


@app.route("/basic")
async def basic(request: Request) -> HTTPResponse:
    return response.text("Hello, World!")


@app.route("/headers-test")
async def headers_test(request: Request) -> HTTPResponse:
    return response.text(
        "HeadersTest", headers={"x-test-1": "foo", "OTHER-TEST": "bar"}
    )


@app.route("/basic-json")
async def basic_json(request: Request) -> HTTPResponse:
    return response.json({"hello": "world", "foo": True}, sort_keys=True)


@app.route("/error-400")
async def error_400(request: Request) -> HTTPResponse:
    return response.text("", 400)


@app.route("/error-401")
async def error_401(request: Request) -> HTTPResponse:
    return response.text("", 401)


@app.route("/response-latin-1")
async def response_latin_1(request: Request) -> HTTPResponse:
    return response.raw(
        "ñáéíóúß".encode("latin1"), content_type="text/plain; charset=latin1"
    )


@app.route("/request-latin-1", methods=["POST"])
async def request_latin_1(request: Request) -> HTTPResponse:
    if request.headers["Content-Type"] != "text/plain; charset=latin1":
        return response.text("FAIL")

    if request.body.decode("latin1") != "áéíóúñü":
        return response.text("FAIL")

    return response.text("OK")


@app.route("/request-utf-8-default", methods=["POST"])
async def request_utf_8_default(request: Request) -> HTTPResponse:
    if request.headers["Content-Type"] != "text/plain":
        return response.text("FAIL")

    if request.body.decode("utf-8") != "áéíóúñü":
        return response.text("FAIL")

    return response.text("OK")


@app.route("/request-utf-8-default-2", methods=["POST"])
async def request_utf_8_default_2(request: Request) -> HTTPResponse:
    if request.body.decode("utf-8") != "áéíóúñü":
        return response.text("FAIL")

    return response.text("OK")


@app.route("/response-utf-8-default")
async def response_utf_8_default(request: Request) -> HTTPResponse:
    # Do not specify charset=
    return response.raw("ñáéíóúß".encode("utf-8"), content_type="text/plain")


@app.route("/response-big5")
async def response_big_5(request: Request) -> HTTPResponse:
    return response.raw("常用字".encode("big5"), content_type="text/plain; charset=big5")


@app.route("/")
async def root(request: Request) -> HTTPResponse:
    if request.args.get("foo") == "bar":
        return response.text("OK")

    return response.text("FAIL")


@app.route("/redirect-301")
async def redirect_301(request: Request) -> HTTPResponse:
    return response.redirect("/basic", status=301)


@app.route("/redirect-302")
async def redirect_302(request: Request) -> HTTPResponse:
    return response.redirect("/basic", status=302)


@app.route("/redirect-308-2", methods=["POST"])
async def redirect_308_2(request: Request) -> HTTPResponse:
    return response.text("Redirect successful")


@app.route("/redirect-308", methods=["POST"])
async def redirect_308(request: Request) -> HTTPResponse:
    return response.redirect("/redirect-308-2", status=308)


@app.route("/no-user-agent")
async def no_user_agent(request: Request) -> HTTPResponse:
    if "User-Agent" in request.headers:
        return response.text("FAIL")

    return response.text("OK")


@app.route("/content-length", methods=["POST"])
async def content_length(request: Request) -> HTTPResponse:
    if "Content-Length" not in request.headers:
        return response.text("FAIL")

    if len(request.body) != int(request.headers["Content-Length"]):
        return response.text("FAIL")

    return response.text("OK")


@app.route("/body-md5", methods=["POST"])
async def body_md5(request: Request) -> HTTPResponse:
    import hashlib

    return response.text(hashlib.md5(request.body).hexdigest())


@app.route("/echo", methods=["POST"])
async def echo(request: Request) -> HTTPResponse:
    charset = "utf-8"
    if "Content-Type" in request.headers:
        ct = request.headers["Content-Type"]
        if "charset=" in ct:
            charset = ct.split("charset=")[-1].strip()

    return response.text(request.body.decode(charset))


@app.route("/zero-bytes-json")
async def zero_bytes_json(request: Request) -> HTTPResponse:
    return response.text("", headers={"Content-Type": "application/json"})


@app.route("/sorted-headers", methods=["GET", "POST"])
async def sorted_headers(request: Request) -> HTTPResponse:
    headers = sorted(k.lower() + ": " + v for k, v in request.headers.items())
    return response.text("\n".join(headers), headers={"Content-Type": "text/plain"})


app.static("/image.png", "test/image.png")


@app.route("/set-cookies")
async def set_cookies(request: Request) -> HTTPResponse:
    resp = response.text("OK")
    for key in request.args:
        val = request.args.get(key)
        resp.cookies[key] = val

    return resp


@app.route("/get-cookies")
async def get_cookies(request: Request) -> HTTPResponse:
    val = "".join(f"{key}={val}\n" for key, val in request.cookies.items())
    return response.text(val)


@app.route("/delete-cookies")
async def delete_cookies(request: Request) -> HTTPResponse:
    resp = response.text("OK")
    for key in request.args:
        if key in request.cookies:
            del resp.cookies[key]

    return resp


@app.exception(NotFound)
async def not_found(request: Request, exception: NotFound) -> HTTPResponse:
    return response.text("Not found", status=404)


@app.exception(MethodNotSupported)
async def not_supported(request: Request, MethodNotSupported: NotFound) -> HTTPResponse:
    return response.text("Method not supported", status=405)


def main() -> None:
    if "SKIP_PIDFILE" not in os.environ:
        pidfile = os.path.join(os.path.dirname(__file__), "server.pid")
        with open(pidfile, "w") as f:
            f.write(str(os.getpid()))

    app.run(host="0.0.0.0", port=int(os.environ.get("PORT", "8000")), access_log=False)


if __name__ == "__main__":
    main()
