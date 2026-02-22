// Basic RE# usage from C#
// run: dotnet script Basic.cs (or reference the Resharp NuGet package in a project)

using System;

// simple matching
var re = new Resharp.Regex(@"\w+");
Console.WriteLine(re.IsMatch("hello"));         // True
Console.WriteLine(re.Count("one two three"));    // 3

foreach (var m in re.Matches("one two three"))
    Console.WriteLine($"  '{m.Value}' at {m.Index}");

// intersection: 4-letter word starting with 'c' AND ending with 's'
var inter = new Resharp.Regex(@"c...&...s");
foreach (var m in inter.Matches("raining cats and dogs"))
    Console.WriteLine($"intersection: {m.Value}");  // cats

// complement: does not contain two consecutive digits
var noDouble = new Resharp.Regex(@"~(_*\d\d_*)");
Console.WriteLine(noDouble.IsMatch("a1b2c3"));  // True  (no consecutive digits)
Console.WriteLine(noDouble.IsMatch("a12bc"));    // False (contains "12")

// replace with function
var digits = new Resharp.Regex(@"\d+");
var result = digits.Replace("price: 10, tax: 3", m => $"[{m}]");
Console.WriteLine(result);  // price: [10], tax: [3]
