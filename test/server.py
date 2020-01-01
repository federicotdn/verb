import os
import logging
from sanic import Sanic, response
from sanic.log import logger
from sanic.request import Request

app = Sanic()
logger.setLevel(logging.CRITICAL)

pidfile = os.path.join(os.path.dirname(__file__), "server.pid")
with open(pidfile, "w") as f:
    f.write(str(os.getpid()))


@app.route("/basic")
async def test1(request: Request):
    return response.text("Hello, World!")


@app.route("/headers-test")
async def headers_test(request: Request):
    return response.text(
        "HeadersTest", headers={"x-test-1": "foo", "OTHER-TEST": "bar"}
    )


@app.route("/basic-json")
async def test1(request: Request):
    return response.json({"hello": "world", "foo": True}, sort_keys=True)


@app.route("/error-400")
async def test2(request: Request):
    return response.text("", 400)


# Charset list:
# https://www.w3.org/International/O-charset-list.html


@app.route("/response-latin-1")
async def test3(request: Request):
    return response.raw(
        "ñáéíóúß".encode("latin1"), content_type="text/plain; charset=latin1"
    )


@app.route("/request-latin-1", methods=["POST"])
async def test_request_latin_1(request: Request):
    if request.headers["Content-Type"] != "text/plain; charset=latin1":
        return response.text("FAIL")

    if request.body.decode("latin1") != "áéíóúñü":
        return response.text("FAIL")

    return response.text("OK")


@app.route("/request-utf-8-default", methods=["POST"])
async def test_request_utf_8_default(request: Request):
    if request.headers["Content-Type"] != "text/plain; charset=utf-8":
        return response.text("FAIL")

    if request.body.decode("utf-8") != "áéíóúñü":
        return response.text("FAIL")

    return response.text("OK")


@app.route("/request-utf-8-default-2", methods=["POST"])
async def test_request_utf_8_default_2(request: Request):
    if request.body.decode("utf-8") != "áéíóúñü":
        return response.text("FAIL")

    return response.text("OK")


@app.route("/response-utf-8-default")
async def test4(request: Request):
    # Do not specify charset=
    return response.raw("ñáéíóúß".encode("utf-8"), content_type="text/plain")


@app.route("/response-big5")
async def test5(request: Request):
    return response.raw("常用字".encode("big5"), content_type="text/plain; charset=big5")


@app.route("")
async def test6(request: Request):
    if request.args.get("foo") == "bar":
        return response.text("OK")

    return response.text("FAIL")


@app.route("/redirect-301")
async def test_redirect_301(request: Request):
    return response.redirect("/basic", status=301)


@app.route("/redirect-302")
async def test_redirect_302(request: Request):
    return response.redirect("/basic", status=302)


@app.route("/redirect-308-2", methods=["POST"])
async def test_redirect_308_2(request: Request):
    return response.text("Redirect successful")


@app.route("/redirect-308", methods=["POST"])
async def test_redirect_308(request: Request):
    return response.redirect("/redirect-308-2", status=308)


if __name__ == "__main__":
    app.run(host="0.0.0.0", port=8000, access_log=False)
