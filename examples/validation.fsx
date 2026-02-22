#r "nuget: resharp, 1.0.0"
open System

// --- full-string validation with anchors ---

// validate date format: DD.MM.YYYY or DD/MM/YYYY
let dateRe = Resharp.Regex(@"\A[0-9]{2}[/.-][0-9]{2}[/.-]([0-9]{4}|[0-9]{2})\z")

let dates = [
    "01.01.2023"
    "1/2/23"
    "01-01-2023"
    "not-a-date"
]

for d in dates do
    printfn "%-15s %b" d (dateRe.IsMatch(d))

// validate IP address
let ipRe =
    Resharp.Regex(
        @"\A(\d{1,2}|1\d\d|2[0-4]\d|25[0-5])\.(\d{1,2}|1\d\d|2[0-4]\d|25[0-5])\.(\d{1,2}|1\d\d|2[0-4]\d|25[0-5])\.(\d{1,2}|1\d\d|2[0-4]\d|25[0-5])\z"
    )

let ips = [
    "0.0.0.0"
    "192.168.1.1"
    "255.255.255.255"
    "999.1.1.1"
]

for ip in ips do
    printfn "%-18s %b" ip (ipRe.IsMatch(ip))

// --- intersection for multi-constraint validation ---

// password: 8+ chars, has uppercase, has lowercase, has digit, no consecutive digits
let passwordRe = Resharp.Regex(@"_{8,}&_*[A-Z]_*&_*[a-z]_*&_*\d_*&~(_*\d\d_*)")

let passwords = [
    "Abcdefg1"
    "abcdefg1"
    "ABCDEFG1"
    "Ab1cd2ef"
    "Ab12cdef"
]

for p in passwords do
    printfn "%-12s %b" p (passwordRe.IsMatch(p))
