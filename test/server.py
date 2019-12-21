import os
from sanic import Sanic, response
from sanic.request import Request

app = Sanic()

pidfile = os.path.join(os.path.dirname(__file__), "server.pid")
with open(pidfile, "w") as f:
    f.write(str(os.getpid()))


@app.route("/basic")
async def test1(request: Request):
    return response.text("Hello, World!")


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


@app.route("/response-utf-8-default")
async def test4(request: Request):
    # Do not specify charset=
    return response.raw("ñáéíóúß".encode("utf-8"), content_type="text/plain")


@app.route("/response-big5")
async def test5(request: Request):
    return response.raw("常用字".encode("big5"), content_type="text/plain; charset=big5")


if __name__ == "__main__":
    app.run(host="0.0.0.0", port=8000, access_log=False)
