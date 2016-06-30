using System;
namespace MuScheme.Lexer
{
    internal interface ILexer
    {
        Token Scan(IReader reader);
    }
}
