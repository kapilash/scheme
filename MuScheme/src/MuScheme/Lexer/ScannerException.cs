using System;

namespace MuScheme.Lexer
{
    public class ScannerException : Exception
    {
        public ScannerException(string message) : base(message) { }
        public ScannerException() : base() { }

        public ScannerException(string message, int line, int column)
            : base($" {message} at ({line}, {column})") { }
    }
}
