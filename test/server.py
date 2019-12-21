import os
from sanic import Sanic, response
from sanic.request import Request

app = Sanic()

pidfile = os.path.join(os.path.dirname(__file__), "server.pid")
with open(pidfile, "w") as f:
    f.write(str(os.getpid()))


@app.route("/basic")
async def test(request: Request):
    return response.text("Hello, World!")

@app.route("/error-400")
async def test(request: Request):
    return response.text("", 400)


if __name__ == "__main__":
    app.run(host="0.0.0.0", port=8000, access_log=False)
