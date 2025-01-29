package main

import (
	"crypto/md5"
	"fmt"
	"io"
	"net/http"
	"os"
	"slices"
	"sort"
	"strconv"
	"strings"
)

func basicHandler(w http.ResponseWriter, r *http.Request) {
	fmt.Fprintf(w, "Hello, World!")
}

func headersTestHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("x-test-1", "foo")
	w.Header().Set("OTHER-TEST", "bar")
	fmt.Fprintf(w, "HeadersTest")
}

func basicJSONHandler(w http.ResponseWriter, r *http.Request) {
	w.Header()["Date"] = nil
	w.Header().Set("Content-Type", "application/json")
	fmt.Fprintf(w, `{"hello": "world", "foo": true}`)
}

func keywordsJSONHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "application/json")
	fmt.Fprintf(w, `{"t": true}`)
}

func error400Handler(w http.ResponseWriter, r *http.Request) {
	w.WriteHeader(http.StatusBadRequest)
}

func error401Handler(w http.ResponseWriter, r *http.Request) {
	http.Error(w, "", http.StatusUnauthorized)
}

func responseLatin1Handler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "text/plain; charset=latin1")
	// "ñáéíóúß" encoded in latin-1:
	w.Write([]byte("\xf1\xe1\xe9\xed\xf3\xfa\xdf"))
}

func requestLatin1Handler(w http.ResponseWriter, r *http.Request) {
	if r.Header.Get("Content-Type") != "text/plain; charset=latin1" {
		http.Error(w, "FAIL", http.StatusBadRequest)
		return
	}
	body, err := io.ReadAll(r.Body)
	if err != nil {
		panic(err)
	}

	// "áéíóúñü" encoded in latin-1:
	if string(body) != "\xe1\xe9\xed\xf3\xfa\xf1\xfc" {
		http.Error(w, "FAIL", http.StatusBadRequest)
		return
	}
	fmt.Fprintf(w, "OK")
}

func requestUTF8DefaultHandler(w http.ResponseWriter, r *http.Request) {
	if r.Header.Get("Content-Type") != "text/plain" {
		http.Error(w, "FAIL", http.StatusBadRequest)
		return
	}
	body, err := io.ReadAll(r.Body)
	if err != nil {
		panic(err)
	}

	if string(body) != "áéíóúñü" {
		http.Error(w, "FAIL", http.StatusBadRequest)
		return
	}
	fmt.Fprintf(w, "OK")
}

func requestUTF8Default2Handler(w http.ResponseWriter, r *http.Request) {
	body, err := io.ReadAll(r.Body)
	if err != nil {
		panic(err)
	}

	if string(body) != "áéíóúñü" {
		http.Error(w, "FAIL", http.StatusBadRequest)
		return
	}
	fmt.Fprintf(w, "OK")
}

func responseUTF8DefaultHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "text/plain")
	w.Write([]byte("ñáéíóúß")) // Go source code is UTF-8 encoded
}

func responseBig5Handler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "text/plain; charset=big5")
	resp := "\xb1`\xa5\xce\xa6r" // "常用字" encoded in Big5
	w.Write([]byte(resp))
}

func rootHandler(w http.ResponseWriter, r *http.Request) {
	if r.URL.Path != "/" {
		http.NotFound(w, r)
		return
	}
	if r.URL.Query().Get("foo") == "bar" {
		fmt.Fprintf(w, "OK")
		return
	}
	fmt.Fprintf(w, "FAIL")
}

func redirect301Handler(w http.ResponseWriter, r *http.Request) {
	http.Redirect(w, r, "/basic", http.StatusMovedPermanently)
}

func redirect302Handler(w http.ResponseWriter, r *http.Request) {
	w.Header()["Content-Type"] = nil
	http.Redirect(w, r, "/basic", http.StatusFound)
}

func redirect308Handler(w http.ResponseWriter, r *http.Request) {
	http.Redirect(w, r, "/redirect-308-2", http.StatusPermanentRedirect)
}

func redirect3082Handler(w http.ResponseWriter, r *http.Request) {
	fmt.Fprintf(w, "Redirect successful")
}

func noUserAgentHandler(w http.ResponseWriter, r *http.Request) {
	if r.UserAgent() != "" {
		http.Error(w, "FAIL", http.StatusBadRequest)
		return
	}
	fmt.Fprintf(w, "OK")
}

func contentLengthHandler(w http.ResponseWriter, r *http.Request) {
	if r.Header.Get("Content-Length") == "" {
		http.Error(w, "FAIL", http.StatusBadRequest)
		return
	}
	body, err := io.ReadAll(r.Body)
	if err != nil {
		panic(err)
	}

	if len(body) != int(r.ContentLength) {
		http.Error(w, "FAIL", http.StatusBadRequest)
		return
	}
	fmt.Fprintf(w, "OK")
}

func bodyMD5Handler(w http.ResponseWriter, r *http.Request) {
	body, err := io.ReadAll(r.Body)
	if err != nil {
		panic(err)
	}

	md5Hash := fmt.Sprintf("%x", md5.Sum(body))
	fmt.Fprintf(w, "%v", md5Hash)
}

func echoHandler(w http.ResponseWriter, r *http.Request) {
	io.Copy(w, r.Body)
}

func echoArgsHandler(w http.ResponseWriter, r *http.Request) {
	values := []string{}
	for key, val := range r.URL.Query() {
		for _, v := range val {
			values = append(values, fmt.Sprintf("%s=%s", key, v))
		}
	}
	sort.Strings(values)
	fmt.Fprintf(w, "%v", strings.Join(values, "\n"))
}

func zeroBytesJSONHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "application/json")
	fmt.Fprintf(w, "")
}

func sortedHeadersHandler(w http.ResponseWriter, r *http.Request) {
	if r.URL.Query().Get("dropcookies") != "" {
		r.Header.Del("Cookie")
	}

	headers := []string{}
	for k, v := range r.Header {
		headers = append(headers, fmt.Sprintf("%s: %s", strings.ToLower(k), v[0]))
	}
	headers = append(headers, fmt.Sprintf("host: %s", r.Host))
	if r.UserAgent() != "" {
		headers = append(headers, fmt.Sprintf("user-agent: %s", r.UserAgent()))
	}

	sort.Strings(headers)
	fmt.Fprintf(w, "%v", strings.Join(headers, "\n"))
}

func notCompressedHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Encoding", "gzip")
	fmt.Fprintf(w, "hello, world!")
}

func setCookiesHandler(w http.ResponseWriter, r *http.Request) {
	err := r.ParseForm()
	if err != nil {
		panic(err)
	}

	for key, val := range r.Form {
		cookie := http.Cookie{Name: key, Value: val[0], Path: "/"}
		http.SetCookie(w, &cookie)
	}
	fmt.Fprintf(w, "OK")
}

func getCookiesHandler(w http.ResponseWriter, r *http.Request) {
	cookies := r.Cookies()
	slices.SortFunc(cookies, func(a, b *http.Cookie) int {
		if a.Name < b.Name {
			return -1
		}
		return 1
	})
	for _, cookie := range cookies {
		fmt.Fprintf(w, "%s=%s\n", cookie.Name, cookie.Value)
	}
}

func deleteCookiesHandler(w http.ResponseWriter, r *http.Request) {
	err := r.ParseForm()
	if err != nil {
		panic(err)
	}

	for key := range r.Form {
		cookie := http.Cookie{Name: key, Value: "", Path: "/", MaxAge: -1}
		http.SetCookie(w, &cookie)
	}
	fmt.Fprintf(w, "OK")
}

func formUrlencodedHandler(w http.ResponseWriter, r *http.Request) {
	err := r.ParseForm()
	if err != nil {
		panic(err)
	}

	if r.Form.Get("hello") != "world" || r.Form.Get("foo") != `{"test":123}` {
		http.Error(w, "FAIL", http.StatusBadRequest)
		return
	}
	fmt.Fprintf(w, "OK")
}

func multipartHandler(w http.ResponseWriter, r *http.Request) {
	err := r.ParseMultipartForm(10 << 20) // 10 MB limit
	if err != nil {
		panic(err)
	}

	if r.FormValue("foo1") != "bar1" || r.FormValue("foo2") != "bar2" {
		http.Error(w, "FAIL", http.StatusBadRequest)
		return
	}
	fmt.Fprintf(w, "OK")
}

func imageHandler(w http.ResponseWriter, r *http.Request) {
	http.ServeFile(w, r, "test/data/test.png")
}

func main() {
	http.HandleFunc("GET /basic", basicHandler)
	http.HandleFunc("GET /headers-test", headersTestHandler)
	http.HandleFunc("GET /basic-json", basicJSONHandler)
	http.HandleFunc("GET /keywords-json", keywordsJSONHandler)
	http.HandleFunc("GET /error-400", error400Handler)
	http.HandleFunc("GET /error-401", error401Handler)
	http.HandleFunc("GET /response-latin-1", responseLatin1Handler)
	http.HandleFunc("POST /request-latin-1", requestLatin1Handler)
	http.HandleFunc("POST /request-utf-8-default", requestUTF8DefaultHandler)
	http.HandleFunc("POST /request-utf-8-default-2", requestUTF8Default2Handler)
	http.HandleFunc("GET /response-utf-8-default", responseUTF8DefaultHandler)
	http.HandleFunc("GET /response-big5", responseBig5Handler)
	http.HandleFunc("GET /", rootHandler)
	http.HandleFunc("GET /redirect-301", redirect301Handler)
	http.HandleFunc("GET /redirect-302", redirect302Handler)
	http.HandleFunc("POST /redirect-308", redirect308Handler)
	http.HandleFunc("POST /redirect-308-2", redirect3082Handler)
	http.HandleFunc("GET /no-user-agent", noUserAgentHandler)
	http.HandleFunc("POST /content-length", contentLengthHandler)
	http.HandleFunc("POST /body-md5", bodyMD5Handler)
	http.HandleFunc("POST /echo", echoHandler)
	http.HandleFunc("GET /echo-args", echoArgsHandler)
	http.HandleFunc("GET /zero-bytes-json", zeroBytesJSONHandler)
	http.HandleFunc("POST /sorted-headers", sortedHeadersHandler)
	http.HandleFunc("GET /sorted-headers", sortedHeadersHandler)
	http.HandleFunc("GET /not-compressed", notCompressedHandler)
	http.HandleFunc("GET /set-cookies", setCookiesHandler)
	http.HandleFunc("GET /get-cookies", getCookiesHandler)
	http.HandleFunc("GET /delete-cookies", deleteCookiesHandler)
	http.HandleFunc("POST /form-urlencoded", formUrlencodedHandler)
	http.HandleFunc("POST /multipart", multipartHandler)
	http.HandleFunc("GET /image.png", imageHandler)

	port, err := strconv.Atoi(os.Getenv("PORT"))
	if err != nil {
		port = 8000
	}

	if _, found := os.LookupEnv("SKIP_PIDFILE"); !found {
		pidfile := "test/server.pid"
		pid := os.Getpid()
		os.WriteFile(pidfile, []byte(strconv.Itoa(pid)), 0644)
		fmt.Printf("server pid: %v\n", pid)
	}
	addr := fmt.Sprintf("localhost:%v", port)
	fmt.Printf("running server at: %v\n", addr)
	http.ListenAndServe(addr, nil)
}
