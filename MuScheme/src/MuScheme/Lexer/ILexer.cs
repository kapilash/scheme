using System;
using System.IO;

namespace MuScheme.Lexer
{
    internal interface ILexer
    {
        Token Scan(char nextChar, TextReader reader);
    }
}
