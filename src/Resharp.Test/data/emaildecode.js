!function () {
    "use strict"; function throwError(e) {
        try { if ("undefined" == typeof console) return; "error" in console ? console.error(e) : console.log(e) } catch (e) {

        }
    } function replaceNodes(e) {
        return d.innerHTML = '<a href="' + e.replace(/"/g, "&quot;") + '"></a>', d.childNodes[0].getAttribute("href") || ""
    }
    function parseHex(string, startpos) {
        var r = string.substr(startpos, 2);
        return parseInt(r, 16)
    }
    function decodeEmail(string, c) {
        for (var o = "", prev = parseHex(string, c), i = c + 2; i < string.length; i += 2) {
            var l = parseHex(string, i) ^ prev;
            o += String.fromCharCode(l)
        }
        try {
            o = decodeURIComponent(escape(o))
        } catch (u) {
            throwError(u)
        }
        return replaceNodes(o)
    } function c(t) {
        for (var r = t.querySelectorAll("a"), c = 0; c < r.length; c++)try {
            var o = r[c], a = o.href.indexOf(l); a > -1 && (o.href = "mailto:" + decodeEmail(o.href, a + l.length))
        } catch (i) { throwError(i) }
    } function o(t) {
        for (var r = t.querySelectorAll(u), c = 0; c < r.length; c++)try {
            var o = r[c], a = o.parentNode, i = o.getAttribute(f); if (i) {
                var l = decodeEmail(i, 0), d = document.createTextNode(l); a.replaceChild(d, o)
            }
        } catch (h) { throwError(h) }
    } function a(t) {
        for (var r = t.querySelectorAll("template"), n = 0; n < r.length; n++)try {
            i(r[n].content)
        } catch (c) { throwError(c) }
    } function i(t) { try { c(t), o(t), a(t) } catch (r) { throwError(r) } } var l = "/cdn-cgi/l/email-protection#", u = ".__cf_email__", f = "data-cfemail", d = document.createElement("div"); i(document), function () {
        var e = document.currentScript
            || document.scripts[document.scripts.length - 1];
        e.parentNode.removeChild(e)
    }()
}();
