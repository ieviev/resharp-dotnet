// stubs for compatibility without importing entire dotnet runtime
using System.Runtime.InteropServices;

namespace System.Text.RegularExpressions
{
    /// <summary>Provides functionality to convert <see cref = "RegexNode"/>s to corresponding <see cref = "SymbolicRegexNode{S}"/>s.</summary>
    internal sealed class SR
    {
        internal static string MakeException => "Could not parse pattern";
        internal static string UnrecognizedUnicodeProperty => "Unrecognized Unicode Property";

        internal static string Format(string o1, [Optional] object o2, [Optional] object o3, [Optional] object o4)
        {
            return "Failed to parse pattern";
        }
    }
}